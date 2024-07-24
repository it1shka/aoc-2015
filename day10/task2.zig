const std = @import("std");
const Allocator = std.mem.Allocator;
const stdout = std.io.getStdOut().writer();

const input = "1113122113";
const iterations = 50;

pub fn main() !void {
  var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
  defer arena.deinit();
  const allocator = arena.allocator();
  
  var sequence: []const u8 = input;
  for (0..iterations) |i| {
    _ = try stdout.print("Iteration {} is running...\n", .{i});
    sequence = try lookAndSay(allocator, sequence);
  }

  _ = try stdout.print("Length of result: {}\n", .{sequence.len});
}

fn lookAndSay(alloc: Allocator, sequence: []const u8) ![]const u8 {
  var output = std.ArrayList(u8).init(alloc);
  var pointer = @as(usize, 0);
  while (pointer < sequence.len) {
    const current = sequence[pointer];
    var count = @as(usize, 0);
    while (pointer < sequence.len and sequence[pointer] == current) {
      pointer += 1;
      count += 1;
    }
    const countString = try std.fmt.allocPrint(alloc, "{}", .{count});
    try output.appendSlice(countString);
    try output.append(current);
  }
  const resultString = try output.toOwnedSlice();
  return resultString;
}
