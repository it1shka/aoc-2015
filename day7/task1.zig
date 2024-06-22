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

  atomic: Atomic,

  unaryGate: struct {
    kind: []const u8,
    input: Atomic,
    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
      allocator.free(self.kind);
      self.input.deinit(allocator);
    }
  },

  binaryGate: struct {
    kind: []const u8,
    leftInput: Atomic,
    rightInput: Atomic,
    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
      allocator.free(self.kind);
      self.leftInput.deinit(allocator);
      self.rightInput.deinit(allocator);
    }
  },

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
    for (self.stream.next()) |token| {
      const copy = try self.allocator.dupe(token);
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
      .dependency = dependency,
    };
  }

  fn parseUnaryGate(self: *@This()) !ParseResult {
    const kind = self.tokens.items[0];
    const input = self.parseAtomicAt(1);
    const target = self.tokens.items[3];
    // TODO: 
  }

  fn parseBinaryGate(self: *@This()) !ParseResult {
    // TODO: 
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
    try self.dependencies.put(result.wire, result.dependency);
  }

  fn deinit(self: *@This()) void {
    for (self.dependencies.keyIterator()) |key| {
      if (self.dependencies.get(key)) |dependency| {
        dependency.deinit(self.allocator);
      }
      self.allocator.free(key);
    }
    self.dependencies.deinit();
  }
};

pub fn main() !void {
  const file = try std.fs.cwd().openFile("input.txt", .{});
  defer file.close();

  var buffered = std.io.bufferedReader(file.reader());
  const reader = buffered.reader();

  var buffer: [64]u8 = undefined;
  while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
    std.debug.print("{s}\n", .{line});
  }
}
