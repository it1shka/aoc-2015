const std = @import("std");

fn min(comptime T: type, slice: []const T) T {
  var output = slice[0];
  for (slice) |elem| {
    if (elem < output) {
      output = elem;
    }
  }
  return output;
}

fn volume(comptime T: type, slice: []const T) T {
  var output = @as(T, 1);
  for (slice) |elem| {
    output *= elem;
  }
  return output;
}

fn calculateArea(line: []const u8) !u32 {
  var it = std.mem.splitScalar(u8, line, 'x');
  var sides: [3]u32 = undefined;
  var pointer = @as(usize, 0);
  while (it.next()) |chunk| {
    const value = try std.fmt.parseUnsigned(u32, chunk, 10);
    sides[pointer] = value;
    pointer += 1;
  }

  const perimeters = [3]u32{
    2 * (sides[0] + sides[1]), 
    2 * (sides[0] + sides[2]),
    2 * (sides[1] + sides[2]),
  };

  return min(u32, &perimeters) + volume(u32, &sides);
}

pub fn main() !void {
  var file = try std.fs.cwd().openFile("input.txt", .{});
  defer file.close();
  
  var buffered = std.io.bufferedReader(file.reader());
  var reader = buffered.reader();

  var buffer: [1024]u8 = undefined;
  var totalArea = @as(u32, 0);
  while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
    totalArea += try calculateArea(line);
  }

  std.debug.print("Total ribbon: {}", .{totalArea});
}
