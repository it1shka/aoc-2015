const std = @import("std");


fn getCodeLength(source: []const u8) usize {
  var output = @as(usize, 0);
  for (source) |symbol| {
    inline for ("\"\\") |toEscape| {
      if (toEscape == symbol) {
        output += 1;
      }
    }
  }
  return output + 2;
}

pub fn main() !void {
  const filename = "input.txt";
  const file = try std.fs.cwd().openFile(filename, .{});
  defer file.close();

  var buffered = std.io.bufferedReader(file.reader());
  const reader = buffered.reader();

  var buffer: [256]u8 = undefined;
  var codeLength = @as(usize, 0);
  while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
    codeLength += getCodeLength(line);
  }

  _ = try std.io.getStdOut().writer().print("Answer: {}\n", .{codeLength});
}
