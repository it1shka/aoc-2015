const std = @import("std");
const Allocator = std.mem.Allocator;

const input = "1113122113";
const iterations = 40;
const bufferSize = 5 * 1024 * 1024;

pub fn main() !void {
  var buffer: [bufferSize]u8 = undefined;
  var fba = std.heap.FixedBufferAllocator.init(&buffer);
  const allocator = fba.allocator();
  
  var sequence: []const u8 = try allocator.dupe(u8, input);
  for (0..iterations) |_| {
    var nextSequence: []const u8 = undefined;
    {
      defer allocator.free(sequence);
      nextSequence = try lookAndSay(allocator, sequence);
    }
    sequence = nextSequence;
  }

  defer allocator.free(sequence);
  const stdout = std.io.getStdOut().writer();
  _ = try stdout.print("Result: {s}\n", .{sequence});
  _ = try stdout.print("Length of result: {}\n", .{sequence.len});
}

fn lookAndSay(alloc: Allocator, sequence: []const u8) ![]const u8 {
  var output = std.ArrayList(u8).init(alloc);
  errdefer output.deinit();
  var pointer = @as(usize, 0);
  while (pointer < sequence.len) {
    const current = sequence[pointer];
    var count = @as(usize, 0);
    while (pointer < sequence.len and sequence[pointer] == current) {
      pointer += 1;
      count += 1;
    }
    {
      const countString = try std.fmt.allocPrint(alloc, "{}", .{count});
      defer alloc.free(countString);
      try output.appendSlice(countString);
    }
    try output.append(current);
  }
  const resultString = try output.toOwnedSlice();
  return resultString;
}
