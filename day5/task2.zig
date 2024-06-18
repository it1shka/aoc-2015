const std = @import("std");

inline fn getChar(haystack: []const u8, index: usize) ?u8 {
  if (index >= haystack.len) {
    return null;
  }
  return haystack[index];
}

fn isNice(string: []const u8) bool {
  var repetitivePair = false;
  var repetitiveLetter = false;
  for (string, 0..) |symbol, index| {
    if (!repetitivePair and index < string.len - 3) {
      const pair = string[index..index+2];
      const tail = string[index+2..];
      const pairIndex = std.mem.indexOf(u8, tail, pair);
      repetitivePair = pairIndex != null;
    }
    if (!repetitiveLetter) {
      repetitiveLetter = symbol == getChar(string, index + 2);
    }
    if (repetitivePair and repetitiveLetter) {
      return true;
    }
  }
  return false;
}

pub fn main() !void {
  var file = try std.fs.cwd().openFile("input.txt", .{});
  defer file.close();

  var buffered = std.io.bufferedReader(file.reader());
  var reader = buffered.reader();

  var lineBuffer: [17]u8 = undefined;
  var nice = @as(u32, 0);
  while (try reader.readUntilDelimiterOrEof(&lineBuffer, '\n')) |line| {
    nice += @intFromBool(isNice(line));
  }
  std.debug.print("Nice count: {}", .{nice});
}
