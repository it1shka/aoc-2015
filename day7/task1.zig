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

pub fn main() !void {
  var file = try std.fs.cwd().openFile("input.txt", .{});
  defer file.close();

  var buffered = std.io.bufferedReader(file.reader());
  var reader = buffered.reader();

  var buffer: [256]u8 = undefined;
  var fba = std.heap.FixedBufferAllocator.init(&buffer);
  const allocator = fba.allocator();

  var lineBuffer: [128]u8 = undefined;
  while (try reader.readUntilDelimiterOrEof(&lineBuffer, '\n')) |line| {
    var it = TokenIterator.init(allocator, line);
    while (try it.next()) |token| {
      defer token.deinit();
      token.dump();
    }
  }

}
