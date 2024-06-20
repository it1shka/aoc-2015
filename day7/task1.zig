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

  fn acceptInstruction(self: *@This(), instruction: []const u8) void {
    // TODO:
    _ = self;
    _ = instruction;
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
    circuit.acceptInstruction(line);
  }

  const wireA = circuit.getWire("a");
  if (wireA) |value| {
    std.debug.print("Wire A: {}", .{value});
  } else {
    std.debug.print("Wire A doesn't have any assigned value", .{});
  }
}
