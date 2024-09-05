const std = @import("std");

fn codeDifference(source: []const u8) usize {
  var difference = @as(usize, 0);
  var escaped = false;
  var skip = @as(usize, 0);
  for (source) |symbol| {
    if (skip > 0) {
      skip -= 1;
      if (skip == 0) {
        difference += 3;
      }
      continue;
    }

    if (escaped) {
      if (symbol == '\\' or symbol == '"') {
        difference += 1;
        escaped = false;
      }
      if (symbol == 'x') {
        skip = 2;
        escaped = false;
      }
      continue;
    }

    if (symbol == '\\') {
      escaped = true;
    }
  }
  return difference + 2;
}

pub fn main() !void {
  const filename = "input.txt";
  const file = try std.fs.cwd().openFile(filename, .{});
  defer file.close();

  var buffered = std.io.bufferedReader(file.reader());
  const reader = buffered.reader();

  var buffer: [256]u8 = undefined;
  var totalDifference = @as(usize, 0);
  while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
    totalDifference += codeDifference(line);
  }

  _ = try std.io.getStdOut().writer().print("Answer: {}\n", .{totalDifference});
}
