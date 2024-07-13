const std = @import("std");

const Dependency = union(enum) {
  const Atomic = union(enum) {
    signal: u16,
    wire: []const u8,
    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
      switch (self) {
        .signal => {},
        .wire => |wire| allocator.free(wire),
      }
    }
  };

  const UnaryGate = struct {
    kind: []const u8,
    input: Atomic,
    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
      allocator.free(self.kind);
      self.input.deinit(allocator);
    }
  };

  const BinaryGate = struct {
    kind: []const u8,
    leftInput: Atomic,
    rightInput: Atomic,
    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
      allocator.free(self.kind);
      self.leftInput.deinit(allocator);
      self.rightInput.deinit(allocator);
    }
  };

  atomic: Atomic,
  unaryGate: UnaryGate,
  binaryGate: BinaryGate,

  fn deinit(self: @This(), allocator: std.mem.Allocator) void {
    switch (self) {
      .atomic => |atomic| atomic.deinit(allocator),
      .unaryGate => |gate| gate.deinit(allocator),
      .binaryGate => |gate| gate.deinit(allocator),
    }
  }
};

const InstructionParser = struct {
  allocator: std.mem.Allocator,
  stream: std.mem.SplitIterator(u8, std.mem.DelimiterType.scalar),
  tokens: std.ArrayList([]const u8),
  
  fn init(allocator: std.mem.Allocator, instruction: []const u8) @This() {
    return @This() {
      .allocator = allocator,
      .stream = std.mem.splitScalar(u8, instruction, ' '),
      .tokens = std.ArrayList([]const u8).init(allocator),
    };
  }

  fn deinit(self: *@This()) void {
    for (self.tokens.items) |item| {
      self.allocator.free(item);
    }
    self.tokens.deinit();
  }

  fn initTokens(self: *@This()) !void {
    while (self.stream.next()) |token| {
      const copy = try self.allocator.dupe(u8, token);
      try self.tokens.append(copy);
    }
  }

  const ParseResult = struct {
    wire: []const u8,
    dependency: Dependency,
  };

  fn parse(self: *@This()) !ParseResult {
    try self.initTokens();
    return try switch (self.tokens.items.len) {
      3 => self.parseAtomic(),
      4 => self.parseUnaryGate(),
      5 => self.parseBinaryGate(),
      else => error.NumberOfTokensMismatch,
    };
  }

  fn parseAtomicAt(self: *@This(), index: usize) !Dependency.Atomic {
    const atomic = self.tokens.items[index];
    return switch (atomic[0]) {
      'a' ... 'z' => Dependency.Atomic { 
        .wire = try self.allocator.dupe(u8, atomic)
      },
      '0' ... '9' => Dependency.Atomic {
        .signal = try std.fmt.parseUnsigned(u16, atomic, 10),
      },
      else => error.IllegalInput,
    };
  }

  fn parseAtomic(self: *@This()) !ParseResult {
    const dependency = try self.parseAtomicAt(0);
    const target = self.tokens.items[2];
    return ParseResult {
      .wire = try self.allocator.dupe(u8, target),
      .dependency = Dependency {
        .atomic = dependency,
      },
    };
  }

  fn parseUnaryGate(self: *@This()) !ParseResult {
    const kind = self.tokens.items[0];
    const input = try self.parseAtomicAt(1);
    const target = self.tokens.items[3];
    return ParseResult {
      .wire = try self.allocator.dupe(u8, target),
      .dependency = Dependency {
        .unaryGate = Dependency.UnaryGate {
          .kind = try self.allocator.dupe(u8, kind),
          .input = input,
        },
      },
    };
  }

  fn parseBinaryGate(self: *@This()) !ParseResult {
    const kind = self.tokens.items[1];
    const leftInput = try self.parseAtomicAt(0);
    const rightInput = try self.parseAtomicAt(2);
    const target = self.tokens.items[4];
    return ParseResult {
      .wire = try self.allocator.dupe(u8, target),
      .dependency = Dependency { 
        .binaryGate = Dependency.BinaryGate {
          .kind = try self.allocator.dupe(u8, kind),
          .leftInput = leftInput,
          .rightInput = rightInput,
        },
      },
    };
  }
};

const Circuit = struct {
  allocator: std.mem.Allocator,
  dependencies: std.StringHashMap(Dependency),

  fn init(allocator: std.mem.Allocator) @This() {
    return @This() {
      .allocator = allocator,
      .dependencies = std.StringHashMap(Dependency).init(allocator),
    };
  }

  fn addInstruction(self: *@This(), instruction: []const u8) !void {
    var parser = InstructionParser.init(self.allocator, instruction);
    defer parser.deinit();
    const result = try parser.parse();
    if (self.dependencies.contains(result.wire)) {
      return error.AmbiguousInput;
    }
    try self.dependencies.put(result.wire, result.dependency);
  }

  fn deinit(self: *@This()) void {
    var iterator = self.dependencies.iterator();
    while (iterator.next()) |entry| {
      entry.value_ptr.deinit(self.allocator);
      self.allocator.free(entry.key_ptr.*);
    }
    self.dependencies.deinit();
  }
};

fn compileCircuit(circuit: *Circuit) !void {
  const file = try std.fs.cwd().openFile("input.txt", .{});
  defer file.close();
  var buffered = std.io.bufferedReader(file.reader());
  const reader = buffered.reader();
  var buffer: [64]u8 = undefined;
  while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
    try circuit.addInstruction(line);
  }
}

const Engine = struct {
  circuit: *Circuit,
  cache: std.StringHashMap(u16),

  fn init(allocator: std.mem.Allocator, circuit: *Circuit) @This() {
    return @This() {
      .circuit = circuit,
      .cache = std.StringHashMap(u16).init(allocator),
    };
  }

  fn deinit(self: *@This()) void {
    self.cache.deinit();
  }

  const EngineError = error {
    WireNotFound,
    UnknownUnaryGate, 
    UnknownBinaryGate,
  };

  fn eval(self: *@This(), dep: Dependency) !u16 {
    return switch (dep) {
      .atomic => |atomic| self.evalAtomic(atomic),
      .unaryGate => |gate| self.evalUnaryGate(gate),
      .binaryGate => |gate| self.evalBinaryGate(gate),
    };
  }

  fn evalWire(self: *@This(), wire: []const u8) !u16 {
    if (self.cache.contains(wire)) {
      return self.cache.get(wire) orelse unreachable;
    }
    const dep = self.circuit.dependencies.get(wire)
      orelse return error.WireNotFound;
    const value = try self.eval(dep);
    try self.cache.put(wire, value);
    return value;
  }

  fn evalAtomic(self: *@This(), atomic: Dependency.Atomic) (std.mem.Allocator.Error || EngineError)!u16 {
    return switch (atomic) {
      .signal => |value| value,
      .wire => |wire| try self.evalWire(wire),
    };
  }

  fn evalUnaryGate(self: *@This(), gate: Dependency.UnaryGate) !u16 {
    const value = try self.evalAtomic(gate.input);
    if (std.mem.eql(u8, gate.kind, "NOT")) {
      return ~value;
    }
    return error.UnknownUnaryGate;
  }

  fn evalBinaryGate(self: *@This(), gate: Dependency.BinaryGate) !u16 {
    const leftValue = try self.evalAtomic(gate.leftInput);
    const rightValue = try self.evalAtomic(gate.rightInput);
    if (std.mem.eql(u8, gate.kind, "AND")) {
      return leftValue & rightValue;
    }
    if (std.mem.eql(u8, gate.kind, "OR")) {
      return leftValue | rightValue;
    }
    if (std.mem.eql(u8, gate.kind, "LSHIFT")) {
      return leftValue << @truncate(rightValue);
    }
    if (std.mem.eql(u8, gate.kind, "RSHIFT")) {
      return leftValue >> @truncate(rightValue);
    }
    return error.UnknownBinaryGate;
  }
};

pub fn main() !void {
  var gpa = std.heap.GeneralPurposeAllocator(.{}){};
  const allocator = gpa.allocator();

  var circuit = Circuit.init(allocator);
  defer circuit.deinit();
  try compileCircuit(&circuit);

  var engine = Engine.init(allocator, &circuit);
  defer engine.deinit();
  const value = try engine.evalWire("a");
  _ = try std.io.getStdIn().writer().print("Answer: {}\n", .{value});
}
