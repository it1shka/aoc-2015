const std = @import("std");
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const gpaAllocator = gpa.allocator();

const TokenType = enum {
  constant,
  gate,
  wire,
  other,
};

const Token = struct {
  kind: TokenType,
  span: []const u8,

  fn init(allocator: std.mem.Allocator, kind: TokenType, span: []const u8) !@This() {
    return @This() {
      .kind = kind,
      .span = try allocator.dupe(u8, span),
    };
  }

  fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
    allocator.free(self.span);
  }
};

fn tokenizeInto(allocator: std.mem.Allocator, dest: *std.ArrayList(Token), source: []const u8) !void {
  var it = std.mem.splitScalar(u8, source, ' ');
  while (it.next()) |span| {
    const token = try switch (span[0]) {
      '0' ... '9' => Token.init(allocator, TokenType.constant, span),
      'a' ... 'z' => Token.init(allocator, TokenType.wire, span),
      'A' ... 'Z' => Token.init(allocator, TokenType.gate, span),
      else => Token.init(allocator, TokenType.other, span),
    };
    try dest.append(token);
  }
}

fn cleanTokens(allocator: std.mem.Allocator, dest: *std.ArrayList(Token)) void {
  for (dest.items) |*token| {
    token.deinit(allocator);
  }
  dest.clearAndFree();
}

const LogicGate = struct {
  left: []const u8,
  right: []const u8,

  fn init(allocator: std.mem.Allocator, left: []const u8, right: []const u8) !@This() {
    return @This() {
      .left = try allocator.dupe(u8, left),
      .right = try allocator.dupe(u8, right),
    };
  }

  fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
    allocator.free(self.left);
    allocator.free(self.right);
  }
};

const ShiftGate = struct {
  wire: []const u8,
  shift: u32,

  fn init(allocator: std.mem.Allocator, wire: []const u8, shift: u32) !@This() {
    return @This() {
      .wire = try allocator.dupe(wire),
      .shift = shift,
    };
  }

  fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
    allocator.free(self.wire);
  }
};

const WireInput = union(enum) {
  constant: u16,
  andGate: LogicGate,
  orGate: LogicGate,
  lshiftGate: ShiftGate,
  rshiftGate: ShiftGate,
  notGate: []const u8,
  // TODO: fn parse() and fn deinit()
};

pub fn main() !void {
  var file = try std.fs.cwd().openFile("input.txt", .{});
  defer file.close();

  var buffered = std.io.bufferedReader(file.reader());
  var reader = buffered.reader();

  var tokens = std.ArrayList(Token).init(gpaAllocator);
  defer tokens.deinit();

  var lineBuffer: [128]u8 = undefined;
  while (try reader.readUntilDelimiterOrEof(&lineBuffer, '\n')) |line| {
    defer cleanTokens(gpaAllocator, &tokens);
    try tokenizeInto(gpaAllocator, &tokens, line);
    // TODO: process tokens
  }
}
