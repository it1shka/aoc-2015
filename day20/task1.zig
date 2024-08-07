const std = @import("std");
const uint = u64;

fn housePresents(house: uint) uint {
  var total = @as(uint, 0);
  var i = @as(uint, 1);
  while (i * i <= house) {
    if (house % i == 0) {
      total += i;
      if (i * i != house) {
        total += house / i;
      }
    }
    i += 1;
  }
  return total;
}

pub fn main() !void {
  const limit = 3310000;
  var house = @as(uint, 1);
  while (housePresents(house) < limit) {
    house += 1;
  }
  std.debug.print("Answer: {}\n", .{house});
}
