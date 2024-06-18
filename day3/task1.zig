const std = @import("std");

const Santa = struct {
  x: i32,
  y: i32,
  houses: u32,
  visited: std.AutoHashMap([2]i32, bool),
  const Self = @This();
  fn new(allocator: std.mem.Allocator) !Self {
    var visited = std.AutoHashMap([2]i32, bool).init(allocator);
    try visited.put([2]i32{0, 0}, true);
    return Self {
      .x = 0,
      .y = 0,
      .houses = 1,
      .visited = visited,
    };
  }
  inline fn deinit(self: *Self) void {
    self.visited.deinit();
  }
  fn move(self: *Self, direction: u8) !void {
    switch (direction) {
      '>' => { self.x += 1; },
      'v' => { self.y -= 1; },
      '<' => { self.x -= 1; },
      '^' => { self.y += 1; },
      // else => return error.UnknownDirection,
      else => {},
    }
    try self.check_house();
  }
  fn check_house(self: *Self) !void {
    const position = [2]i32{self.x, self.y};
    if (self.visited.contains(position)) {
      return;
    }
    try self.visited.put(position, true);
    self.houses += 1;
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
  var santa = try Santa.new(allocator);
  defer santa.deinit();
  while (true) {
    const symbol = reader.readByte() catch |err| switch (err) {
      error.EndOfStream => break,
      else => return err,
    };
    try santa.move(symbol);
  }
  std.debug.print("Houses: {}", .{santa.houses});
}
