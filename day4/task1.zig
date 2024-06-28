const std = @import("std");
const md5 = std.crypto.hash.Md5;

const BOUNDARY = @as(usize, 1e9);
const BUFFER_SIZE = @as(usize, 4096);

pub fn main() !void {
  const input = "yzbqklnj";
  var buffer: [BUFFER_SIZE]u8 = undefined;
  var fba = std.heap.FixedBufferAllocator.init(&buffer);
  const allocator = fba.allocator();
  var hashBuffer: [md5.digest_length]u8 = undefined;
  for (1..BOUNDARY) |index| {
    const key = try std.fmt.allocPrint(allocator, "{s}{}", .{ input, index });
    defer allocator.free(key);
    md5.hash(key, &hashBuffer, .{});
    const hexFormatter = std.fmt.fmtSliceHexLower(&hashBuffer);
    const hex = try std.fmt.allocPrint(allocator, "{}", .{hexFormatter});
    defer allocator.free(hex);
    if (std.mem.eql(u8, hex[0..5], "00000")) {
      std.debug.print("The answer is: {}\n", .{index});
    }
  }
  std.debug.print("No answer was found.\n", .{});
}
