const std = @import("std");

const Raindeer = struct {
  allocator: std.mem.Allocator,
  name: []const u8,
  speed: u16,
  flyTime: u16,
  restTime: u16,
  distance: u16,
  points: u16,

  fn parseFromLine(allocator: std.mem.Allocator, line: []const u8) !@This() {
    var raindeer: Raindeer = undefined;
    raindeer.allocator = allocator;
    raindeer.distance = 0;
    raindeer.points = 0;
    var index = @as(usize, 0);
    var it = std.mem.splitScalar(u8, line, ' ');
    while (it.next()) |chunk| {
      switch (index) {
        0 => raindeer.name = try allocator.dupe(u8, chunk),
        3 => {
          errdefer allocator.free(raindeer.name);
          raindeer.speed = try std.fmt.parseUnsigned(u16, chunk, 10);
        },
        6 => {
          errdefer allocator.free(raindeer.name);
          raindeer.flyTime = try std.fmt.parseUnsigned(u16, chunk, 10);
        },
        13 => {
          errdefer allocator.free(raindeer.name);
          raindeer.restTime = try std.fmt.parseUnsigned(u16, chunk, 10);
        },
        else => {}
      }
      index += 1;
    }
    return raindeer;
  }

  fn move(self: *@This(), second: u16) void {
    const local = second % (self.flyTime + self.restTime);
    if (local >= self.flyTime) return;
    self.distance += self.speed;
  }

  fn deinit(self: *const @This()) void {
    self.allocator.free(self.name);
  }
};

fn readRaindeers (
  allocator: std.mem.Allocator, 
  filename: []const u8
) !std.ArrayList(Raindeer) {
  var output = std.ArrayList(Raindeer).init(allocator);
  errdefer {
    for (output.items) |*raindeer| {
      raindeer.deinit();
    }
    output.deinit();
  }
  
  var file = try std.fs.cwd().openFile(filename, .{});
  defer file.close();
  var buffered = std.io.bufferedReader(file.reader());
  var reader = buffered.reader();
  var buffer: [256]u8 = undefined;

  while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
    const raindeer = try Raindeer.parseFromLine(allocator, line);
    try output.append(raindeer);
  }

  return output;
}

fn increasePoints(raindeers: []Raindeer) void {
  var maxDistance = @as(u16, 0);
  for (raindeers) |*raindeer| {
    maxDistance = @max(maxDistance, raindeer.distance);
  }
  for (raindeers) |*raindeer| {
    if (raindeer.distance == maxDistance) {
      raindeer.points += 1;
    }
  }
}

fn getLeader(raindeers: []const Raindeer) *const Raindeer {
  var leader: *const Raindeer = undefined;
  var maxPoints = @as(u16, 0);
  for (raindeers) |*raindeer| {
    if (raindeer.points > maxPoints) {
      maxPoints = raindeer.points;
      leader = raindeer;
    }
  }
  return leader;
}

const inputFile = "input.txt";
const seconds = 2503;
const memoryLimit = 2048;

pub fn main() !void {
  var buffer: [memoryLimit]u8 = undefined;
  var fba = std.heap.FixedBufferAllocator.init(&buffer);
  const allocator = fba.allocator();

  const raindeers = try readRaindeers(allocator, inputFile);
  defer {
    for (raindeers.items) |*raindeer| {
      raindeer.deinit();
    }
    raindeers.deinit();
  }

  for (0..seconds) |second| {
    for (raindeers.items) |*raindeer| {
      raindeer.move(@truncate(second));
    }
    increasePoints(raindeers.items);
  }

  const leader = getLeader(raindeers.items);
  const stdout = std.io.getStdOut().writer();
  _ = try stdout.print("Raindeer {s} scored {} points\n", .{leader.name, leader.points});
}
