const std = @import("std");

pub fn main() !void {
  const filename = "input.txt";
  var file = try std.fs.cwd().openFile(filename, .{});
  defer file.close();

  var buffered = std.io.bufferedReader(file.reader());
  var reader = buffered.reader();

  var floor = @as(i32, 0);

  while (true) {
    const symbol = reader.readByte() catch |err| switch(err) {
      error.EndOfStream => break,
      else => return err,
    };
    switch (symbol) {
      '(' => { floor += 1; },
      ')' => { floor -= 1; },
      else => {},
    }
  }

  std.debug.print("Floor is: {}", .{floor});
}
