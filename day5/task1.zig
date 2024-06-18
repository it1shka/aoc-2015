const std = @import("std");

inline fn isVowel(symbol: u8) bool {
  const vowels = comptime "aeiou";
  inline for (vowels) |vowel| {
    if (symbol == vowel) {
      return true;
    }
  }
  return false;
}

inline fn isProhibited(string: []const u8) bool {
  const prohibited = comptime [_]([]const u8) {"ab", "cd", "pq", "xy"};
  inline for (prohibited) |elem| {
    if (std.mem.eql(u8, elem, string)) {
      return true;
    }
  }
  return false;
}

fn isNice(string: []const u8) bool {
  var vowelCount = @as(usize, 0);
  var twiceFlag = false;
  for (string, 0..) |symbol, index| {
    vowelCount += @intFromBool(isVowel(symbol));
    if (index < string.len - 1) {
      const slice = string[index..index+2];
      if (isProhibited(slice)) return false;
      twiceFlag = twiceFlag or slice[0] == slice[1];
    }
  }
  return vowelCount >= 3 and twiceFlag;
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
