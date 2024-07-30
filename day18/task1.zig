const std = @import("std");
const stdout = std.io.getStdOut().writer();

const Field = [100][100]bool;

fn readField(field: *Field, filename: []const u8) !void {
  const file = try std.fs.cwd().openFile(filename, .{});
  defer file.close();
  const reader = file.reader();
  var row = @as(usize, 0);
  var column = @as(usize, 0);
  while (true) {
    const symbol = reader.readByte() catch |err| switch (err) {
      error.EndOfStream => break,
      else => |e| return e,
    };
    switch (symbol) {
      '.' => {
        field[row][column] = false;
        column += 1;
      },
      '#' => {
        field[row][column] = true;
        column += 1;
      },
      '\n' => {
        row += 1;
        column = 0;
      },
      else => {},
    }
  }
}

fn stepSummary(field: *const Field) u16 {
  var output = @as(u16, 0);
  for (0..field.len) |row| {
    for (0..field[row].len) |col| {
      output += @intFromBool(field[row][col]);
    }
  }
  return output;
}

fn countNeighbors(field: *const Field, row: usize, col: usize) u16 {
  var output = @as(u16, 0);
  for ((if (row == 0) 0 else row - 1)..@min(field.len, row + 2)) |irow| {
    for ((if (col == 0) 0 else col - 1)..@min(field[row].len, col + 2)) |icol| {
      if (row == irow and col == icol) continue;
      output += @intFromBool(field[irow][icol]);
    }
  }
  return output;
}

fn stepNext(field: *Field) void {
  var next: Field = undefined;
  for (0..field.len) |row| {
    for (0..field[row].len) |col| {
      const turnedOn = field[row][col];
      const neighbors = countNeighbors(field, row, col);
      next[row][col] = (turnedOn and (neighbors == 2 or neighbors == 3))
        or (!turnedOn and neighbors == 3);
    }
  }
  for (0..field.len) |row| {
    @memcpy(field[row][0..], next[row][0..]);
  }
}

pub fn main() !void {
  var field: Field = undefined;
  try readField(&field, "input.txt");
  for (0..100) |_| {
    stepNext(&field);
  }
  const summary = stepSummary(&field);
  _ = try stdout.print("Lights: {}\n", .{summary});
}
