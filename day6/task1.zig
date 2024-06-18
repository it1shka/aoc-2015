const std = @import("std");

const Field = [1000][1000]bool;

const InstructionType = enum {
  turnOn,
  toggle,
  turnOff,
};

fn parseRange(raw: []const u8) ![2]usize {
  var output: [2]usize = undefined;
  var pointer = @as(usize, 0);
  var it = std.mem.splitScalar(u8, raw, ',');
  while (it.next()) |part| {
    output[pointer] = 
      try std.fmt.parseUnsigned(usize, part, 10);
    pointer += 1;
  }
  return output;
}

const Instruction = struct {
  kind: InstructionType,
  from: [2]usize,
  to:   [2]usize,

  fn fromString(string: []const u8) !Instruction {
    var it = std.mem.splitScalar(u8, string, ' ');

    var mode: InstructionType = undefined;
    const modeStart = it.next() orelse
      return error.NoModeStartSpecified;
    if (std.mem.eql(u8, "toggle", modeStart)) {
      mode = InstructionType.toggle;
    } else {
      const modeEnd = it.next() orelse
        return error.NoModeEndSpecified;
      if (std.mem.eql(u8, "on", modeEnd)) {
        mode = InstructionType.turnOn;
      } else {
        mode = InstructionType.turnOff;
      }
    }

    const from = try parseRange(it.next() orelse 
      return error.NoFirstRange);
    _ = it.next() orelse 
      return error.NoThrough;
    const to = try parseRange(it.next() orelse 
      return error.NoSecondRange);

    return @This() {
      .kind = mode,
      .from = from,
      .to = to,
    };
  }

  fn executeOn(self: *const @This(), field: *Field) void {
    for (self.from[1] .. self.to[1] + 1) |row| {
      for (self.from[0] .. self.to[0] + 1) |col| {
        field[row][col] = switch (self.kind) {
          InstructionType.turnOn => true,
          InstructionType.turnOff => false,
          else => !field[row][col],
        };
      }
    }
  }
};

fn countLights(field: *const Field) u32 {
  var count = @as(u32, 0);
  for (field) |*row| {
    for (row) |value| {
      count += @intFromBool(value);
    }
  }
  return count;
}

pub fn main() !void {
  var file = try std.fs.cwd().openFile("input.txt", .{});
  defer file.close();

  var buffered = std.io.bufferedReader(file.reader());
  var reader = buffered.reader();

  var lineBuffer: [40]u8 = undefined;
  var field: Field = undefined;
  for (&field) |*row| {
    @memset(row, false);
  }

  while (try reader.readUntilDelimiterOrEof(&lineBuffer, '\n')) |line| {
    const instruction = try Instruction.fromString(line);
    instruction.executeOn(&field);
  }

  const lights = countLights(&field);
  std.debug.print("Lights: {}", .{lights});
}
