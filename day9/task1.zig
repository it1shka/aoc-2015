const std = @import("std");
const Allocator = std.mem.Allocator;
const stdout = std.io.getStdOut().writer();

const Route = struct {
  allocator: Allocator,
  start: []const u8,
  end: []const u8,
  distance: u16,

  fn parse(allocator: Allocator, line: []const u8) !@This() {
    var route: Route = undefined;
    route.allocator = allocator;
    errdefer route.deinit();

    var iterator = std.mem.splitScalar(u8, line, ' ');
    inline for (0..5) |i| {
      const current = iterator.next() 
        orelse return error.NoToken;
      switch (i) {
        0 => route.start = try allocator.dupe(u8, current),
        2 => route.end = try allocator.dupe(u8, current),
        4 => route.distance = try std.fmt.parseUnsigned(u16, current, 10),
        else => {},
      }
    }

    return route;
  }

  fn deinit(self: *const @This()) void {
    self.allocator.free(self.start);
    self.allocator.free(self.end);
  }

  fn dump(self: *const @This()) !void {
    _ = try stdout.print("{s} to {s} = {}\n", .{
      self.start, 
      self.end, 
      self.distance,
    });
  }
};

fn deinitRoutes(routes: *const std.ArrayList(Route)) void {
  for (routes.items) |*item| {
    item.deinit();
  }
  routes.deinit();
}

fn readRoutes(allocator: Allocator, filename: []const u8) !std.ArrayList(Route) {
  var file = try std.fs.cwd().openFile(filename, .{});
  defer file.close();
  var buffered = std.io.bufferedReader(file.reader());
  var reader = buffered.reader();
  var buffer: [128]u8 = undefined;

  var output = std.ArrayList(Route).init(allocator);
  errdefer deinitRoutes(&output);

  while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
    const route = try Route.parse(allocator, line);
    try output.append(route);
  }

  return output;
}

pub fn main() !void {
  var gpa = std.heap.GeneralPurposeAllocator(.{}){};
  const allocator = gpa.allocator();

  const routes = try readRoutes(allocator, "input.txt");
  defer deinitRoutes(&routes);

  for (routes.items) |*item| {
    try item.dump();
  }
}
