const std = @import("std");

fn getLinearIndex(r: usize, c: usize) usize {
  var idx = @as(usize, 0);
  var diag = @as(usize, 1);
  while (true) : (diag += 1) {
    var row = diag;
    while (row >= 1) : (row -= 1) {
      const column = diag - row + 1;
      if (row == r and column == c) {
        return idx;
      }
      idx += 1;
    }
  }
}

pub fn main() !void {
  const linearIndex = getLinearIndex(3010, 3019);
  var acc = @as(usize, 20151125);
  for (0..linearIndex) |_| {
    acc *= 252533;
    acc %= 33554393;
  }
  std.debug.print("Answer: {}\n", .{acc});
}
