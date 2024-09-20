const std = @import("std");

fn sumOf(values: []const u16) u16 {
  var output = @as(u16, 0);
  for (values) |item| {
    output += item;
  }
  return output;
}

fn bitmaskSumOf(values: []const u16, bitmask: u32) u16 {
  var output = @as(u16, 0);
  for (0..@min(@sizeOf(u32) * 8, values.len)) |i| {
    const currentBit = (bitmask >> @intCast(i)) & 1;
    if (currentBit == 1) {
      output += values[i];
    }
  }
  return output;
}

fn bitmaskPowersetOf(allocator: std.mem.Allocator, size: usize) !std.ArrayList(u32) {
  var output = std.ArrayList(u32).init(allocator);
  errdefer output.deinit();
  try output.append(0x0);
  for (0..size) |i| {
    var next = std.ArrayList(u32).init(allocator);
    errdefer next.deinit();
    for (output.items) |bitmask| {
      try next.append(bitmask);
      const nextBitmask = bitmask | (@as(u32, 1) << @intCast(i));
      try next.append(nextBitmask);
    }
    output.deinit();
    output = next;
  }
  return output;
}

fn partitionsOf(allocator: std.mem.Allocator, values: []const u16, partitionSize: usize) !std.ArrayList(u32) {
  const bitmaskPowerset = try bitmaskPowersetOf(allocator, values.len);
  defer bitmaskPowerset.deinit();
  const partitionWeight = sumOf(values) / partitionSize;
  var output = std.ArrayList(u32).init(allocator);
  errdefer output.deinit();
  for (bitmaskPowerset.items) |bitmask| {
    if (bitmaskSumOf(values, bitmask) == partitionWeight) {
      try output.append(bitmask);
    }
  }
  return output;
}

pub fn main() !void {
  const weights = [_]u16 {
    1, 3, 5, 11, 13, 17,
    19, 23, 29, 31, 41, 
    43, 47, 53, 59, 61,
    67, 71, 73, 79, 83,
    89, 97, 101, 103,
    107, 109, 113
  };
  const partitions = 4;

  const groups = try partitionsOf(std.heap.c_allocator, &weights, partitions);
  defer groups.deinit();

  std.debug.print("Groups: {}\n", .{groups.items.len});
}
