const std = @import("std");

const TokenType = enum {
  constant,
  wire,
  gate,
  arrow,

  fn name(self: @This()) []const u8 {
    return switch(self) {
      .constant => "constant",
      .wire => "wire",
      .gate => "gate",
      .arrow => "arrow",
    };
  }
};

const Token = struct {
  alloc: std.mem.Allocator,
  kind: TokenType,
  span: []const u8,

  fn init(allocator: std.mem.Allocator, kind: TokenType, span: []const u8) !@This() {
    return @This() {
      .alloc = allocator,
      .kind = kind,
      .span = try allocator.dupe(u8, span),
    };
  }

  fn deinit(self: *const @This()) void {
    self.alloc.free(self.span);
  }

  fn dump(self: *const @This()) void {
    std.debug.print("{s} \"{s}\"\n", .{ self.kind.name(), self.span });
  }
};

const TokenIterator = struct {
  alloc: std.mem.Allocator,
  chunkIterator: std.mem.SplitIterator(u8, std.mem.DelimiterType.scalar),

  fn init(allocator: std.mem.Allocator, source: []const u8) @This() {
    const it = std.mem.splitScalar(u8, source, ' ');
    return @This() { 
      .alloc = allocator,
      .chunkIterator = it,
    };
  }

  fn next(self: *@This()) !?Token {
    const chunk = self.chunkIterator.next() orelse return null;
    return try switch (chunk[0]) {
      'a' ... 'z' => Token.init(self.alloc, TokenType.wire, chunk),
      'A' ... 'Z' => Token.init(self.alloc, TokenType.gate, chunk),
      '0' ... '9' => Token.init(self.alloc, TokenType.constant, chunk),
      else => Token.init(self.alloc, TokenType.arrow, chunk),
    };
  }
};

const GateType = enum {
  AND,
  OR,
  LSHIFT,
  RSHIFT,

  fn fromString(source: []const u8) !@This() {
    const mapping = comptime [_]struct{ kind: GateType, span: []const u8 } {
      .{ .kind = .AND, .span = "AND" },
      .{ .kind = .OR,  .span = "OR"  },
      .{ .kind = .LSHIFT, .span = "LSHIFT" },
      .{ .kind = .RSHIFT, .span = "RSHIFT" },
    };
    inline for (mapping) |elem| {
      if (std.mem.eql(u8, source, elem.span)) {
        return elem.kind;
      }
    }
    return error.IllegalLiteral;
  }

  fn executeOn(self: @This(), left: u16, right: u16) u16 {
    return switch(self) {
      .AND => left & right,
      .OR => left | right,
      .LSHIFT => left << @truncate(right),
      .RSHIFT => left >> @truncate(right),
    };
  }
};

const Circuit = struct {
  alloc: std.mem.Allocator,
  breadboard: std.StringHashMap(u16),

  fn init(allocator: std.mem.Allocator) @This() {
    return @This() {
      .alloc = allocator,
      .breadboard = std.StringHashMap(u16).init(allocator),
    };
  }

  fn deinit(self: *@This()) void {
    var keys = self.breadboard.keyIterator();
    while (keys.next()) |key| {
      self.alloc.free(key.*);
    }
    self.breadboard.deinit();
  }

  fn acceptInstruction(self: *@This(), instruction: []const u8) !void {
    var it = TokenIterator.init(self.alloc, instruction);
    var token: Token = undefined;
    var value: u16 = undefined;

    token = try it.next() orelse return error.EmptyInstruction;
    if (std.mem.eql(u8, token.span, "NOT")) {
      token.deinit();
      token = try it.next() orelse return error.ExpectedOperand;
      defer token.deinit();
      value = ~try self.operand(token);
    }

    {
      defer token.deinit();
      value = try self.operand(token);
    }

    token = try it.next() orelse return error.ExpectedGateOrArrow;
    if (token.kind == TokenType.gate) {
      var gate: GateType = undefined;
      {
        defer token.deinit();
        gate = try GateType.fromString(token.span);
      }
      token = try it.next() orelse return error.ExpectedRightOperand;
      var rightValue: u16 = undefined;
      {
        defer token.deinit();
        rightValue = try self.operand(token);
      }
      value = gate.executeOn(value, rightValue);
      token = try it.next() orelse return error.ExpectedArrow;
    }

    {
      defer token.deinit();
      if (!std.mem.eql(u8, token.span, "->")) {
        return error.ExpectedArrow;
      }
    }

    token = try it.next() orelse return error.ExpectedOutputWire;
    {
      errdefer token.deinit();
      if (token.kind != TokenType.wire) {
        return error.ExpectedOutputWire;
      }
    }

    defer token.deinit();
    try self.breadboard.put(token.span, value);
  }

  fn operand(self: *const @This(), token: Token) !u16 {
    return try switch(token.kind) {
      .constant => std.fmt.parseUnsigned(u16, token.span, 10),
      .wire => self.breadboard.get(token.span) orelse error.WireNotFound,
      else => error.IllegalOperand,
    };
  }

  fn getWire(self: *const @This(), wire: []const u8) ?u16 {
    return self.breadboard.get(wire);
  }
};

pub fn main() !void {
  var file = try std.fs.cwd().openFile("input.txt", .{});
  defer file.close();

  var buffered = std.io.bufferedReader(file.reader());
  var reader = buffered.reader();

  var buffer: [100 * 1024]u8 = undefined;
  var fba = std.heap.FixedBufferAllocator.init(&buffer);
  const allocator = fba.allocator();

  var circuit = Circuit.init(allocator);
  defer circuit.deinit();

  var lineBuffer: [128]u8 = undefined;
  while (try reader.readUntilDelimiterOrEof(&lineBuffer, '\n')) |line| {
    try circuit.acceptInstruction(line);
  }

  const wireA = circuit.getWire("a");
  if (wireA) |value| {
    std.debug.print("Wire A: {}", .{value});
  } else {
    std.debug.print("Wire A doesn't have any assigned value", .{});
  }
}
