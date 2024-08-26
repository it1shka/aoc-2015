const std = @import("std");
const stdout = std.io.getStdOut().writer();

fn increment(password: []u8) void {
  var i = password.len;
  while (i > 0) {
    i -= 1;
    password[i] += 1;
    if (password[i] <= 'z') break;
    password[i] = 'a';
  }
}

fn correct(password: []const u8) bool {
  var straight = false;
  var pairs = false;
  
  var prev = @as(u8, 0);
  var straightLength = @as(usize, 0);
  var firstPair = @as(u8, 0);

  for (password) |letter| {
    // checking for banned letters
    inline for ("iol") |banned| {
      if (banned == letter) {
        return false;
      }
    }

    // checking for straight 3
    if (prev + 1 == letter) {
      straightLength += 1;
      if (straightLength >= 3) {
        straight = true;
      }
    } else {
      straightLength = 1;
    }

    // checking for pairs
    if (prev == letter) {
      if (firstPair == 0) {
        firstPair = letter;
      } else if (letter != firstPair) {
        pairs = true;
      }
    }
    
    prev = letter;
  }

  return straight and pairs;
}

pub fn main() !void {
  var password = "hxbxxyzz".*;
  increment(&password);
  while (!correct(&password)) {
    increment(&password);
    _ = try stdout.print("Checking {s}\n", .{ password });
  }
  _ = try stdout.print("Password: {s}\n", .{ password });
}
