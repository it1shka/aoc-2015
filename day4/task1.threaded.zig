const std = @import("std");
const md5 = std.crypto.hash.Md5;
const input = "yzbqklnj";

const BUF_SIZE = @as(usize, 256);
const THREADS_AMOUNT = 4;
const LIMIT = @as(usize, 1e9);

fn checkEveryNth(n: usize) !void {
  var buffer: [BUF_SIZE]u8 = undefined;
  var fba = std.heap.FixedBufferAllocator.init(&buffer);
  const allocator = fba.allocator();
  
  var hexBuffer: [md5.digest_length]u8 = undefined;

  for (0..LIMIT) |raw| {
    const index = (raw * THREADS_AMOUNT) + n;
    const key = try std.fmt.allocPrint(allocator, "{s}{}", .{input, index});
    defer allocator.free(key);
    md5.hash(key, &hexBuffer, .{});
    const hexFormatter = std.fmt.fmtSliceHexLower(&hexBuffer);
    const hex = try std.fmt.allocPrint(allocator, "{}", .{hexFormatter});
    defer allocator.free(hex);
    if (std.mem.eql(u8, hex[0..5], "00000")) {
      std.debug.print("Possible value is {}\n", .{index});
    }
  }
}

pub fn main() !void {
  var threads: [THREADS_AMOUNT]std.Thread = undefined;
  for (0..THREADS_AMOUNT) |i| {
    threads[i] = try std.Thread.spawn(.{}, checkEveryNth, .{i + 1});
  }
  for (&threads) |*thread| {
    thread.join();
  }
}
