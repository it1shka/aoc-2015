const std = @import("std");
const md5 = std.crypto.hash.Md5;
const input = "yzbqklnj";

const BUF_SIZE = @as(usize, 256);
const THREADS_AMOUNT = 4;
const LIMIT = @as(usize, 1e9);

const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();

fn checkEveryNth(n: usize, zeroes: u16) !void {
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
    var flag = true;
    for (hex[0..zeroes]) |symbol| {
      if (symbol != '0') {
        flag = false;
        break;
      }
    }
    if (flag) {
      _ = try stdout.print("Possible value is {}\n", .{index});
    }
  }
}

pub fn main() !void {
  _ = try stdout.write("Please, zeroes amount: ");
  var buffer: [256]u8 = undefined;
  var zeroes: u16 = 6;
  if (try stdin.readUntilDelimiterOrEof(&buffer, '\n')) |rawZeroes| {
    zeroes = try std.fmt.parseUnsigned(u16, rawZeroes, 10);
  }

  var threads: [THREADS_AMOUNT]std.Thread = undefined;
  for (0..THREADS_AMOUNT) |i| {
    threads[i] = try std.Thread.spawn(.{}, checkEveryNth, .{i + 1, zeroes});
  }
  for (&threads) |*thread| {
    thread.join();
  }
}
