const std = @import("std");

const REQUIRED_PRESENTS = 33100000;
const ELF_RANGE = 50;

pub fn main() !void {
  var presents = std.AutoHashMap(usize, usize).init(std.heap.c_allocator);
  defer presents.deinit();

  var minHouse: ?usize = null;

  var elf = @as(usize, 1);
  while (true) {
    for (1..ELF_RANGE + 1) |nthHouse| {
      const house = nthHouse * elf;
      const prev = presents.get(house) orelse 0;
      const next = prev + elf * 11;
      if (next >= REQUIRED_PRESENTS and house < (minHouse orelse house + 1)) {
        minHouse = house;
        std.debug.print("Min house: {?}\n", .{minHouse});
      }
      try presents.put(house, next);
    }
    elf += 1;
  }
}
