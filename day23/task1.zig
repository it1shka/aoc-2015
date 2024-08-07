const std = @import("std");
const stdin = std.io.getStdIn().reader();
const stdout = std.io.getStdOut().writer();

fn stringIn(comptime options: []const []const u8, tested: []const u8) bool {
  inline for (options) |option| {
    if (std.mem.eql(u8, option, tested)) {
      return true;
    }
  }
  return false;
}

const VMInstruction = union(enum) {
  hlf: u8,
  tpl: u8,
  inc: u8,
  jmp: i64,
  jie: struct {
    register: u8,
    offset: i64,
  },
  jio: struct {
    register: u8,
    offset: i64,
  },

  fn parse(line: []const u8) !@This() {
    var it = std.mem.splitScalar(u8, line, ' ');
    const instr = it.next() orelse
      return error.ExpectedInstructionName;

    if (stringIn(&[_][]const u8{"hlf", "tpl", "inc"}, instr)) {
      const register = it.next() orelse
        return error.ExpectedRegister;
      const reg = register[0];
      if (std.mem.eql(u8, "hlf", instr)) {
        return @This() { .hlf = reg };
      }
      if (std.mem.eql(u8, "tpl", instr)) {
        return @This() { .tpl = reg };
      }
      return @This() { .inc = reg };
    }

    if (std.mem.eql(u8, "jmp", instr)) {
      const offsetRaw = it.next() orelse
        return error.ExpectedOffset;
      const offset = try std.fmt.parseInt(i64, offsetRaw, 10);
      return @This() { .jmp = offset };
    }

    const register = it.next() orelse
      return error.ExpectedRegister;
    const reg = register[0];
    const offsetRaw = it.next() orelse
      return error.ExpectedOffset;
    const offset = try std.fmt.parseInt(i64, offsetRaw, 10);
    if (std.mem.eql(u8, "jie", instr)) {
      return @This() {
        .jie = .{
          .register = reg,
          .offset = offset,
        },
      };
    }
    return @This() {
      .jio = .{
        .register = reg,
        .offset = offset,
      },
    };
  }
};

const VMInstance = struct {
  registers: [2]u64,
  pointer: i64,
  instructions: []const VMInstruction,

  fn register(self: *@This(), reg: u8) *u64 {
    return switch (reg) {
      'a' => &self.registers[0],
      else => &self.registers[1],
    };
  }

  fn run(self: *@This()) void {
    while (self.pointer >= 0 
      and self.pointer < self.instructions.len) {
      self.executeUnderPointer();
    }
  }

  fn executeUnderPointer(self: *@This()) void {
    switch (self.instructions[@bitCast(self.pointer)]) {
      .hlf => |reg| {
        self.register(reg).* /= 2;
        self.pointer += 1;
      },
      .tpl => |reg| {
        self.register(reg).* *= 3;
        self.pointer += 1;
      },
      .inc => |reg| {
        self.register(reg).* += 1;
        self.pointer += 1;
      },
      .jmp => |offset| {
        self.pointer += offset;
      },
      .jie => |jie| {
        const regValue = self.register(jie.register).*;
        if (regValue % 2 == 0) {
          self.pointer += jie.offset;
        } else {
          self.pointer += 1;
        }
      },
      .jio => |jio| {
        const regValue = self.register(jio.register).*;
        if (regValue == 1) {
          self.pointer += jio.offset;
        } else {
          self.pointer += 1;
        }
      },
    }
  }
};

fn readInstructions(allocator: std.mem.Allocator) !std.ArrayList(VMInstruction) {
  var stdinBuffer: [256]u8 = undefined;
  _ = try stdout.write("Provide input file: ");
  const filename = try stdin.readUntilDelimiter(&stdinBuffer, '\n');
  const file = try std.fs.cwd().openFile(filename, .{});
  defer file.close();
  var buffered = std.io.bufferedReader(file.reader());
  const reader = buffered.reader();
  var output = std.ArrayList(VMInstruction).init(allocator);
  errdefer output.deinit();
  while (try reader.readUntilDelimiterOrEof(&stdinBuffer, '\n')) |line| {
    const instruction = try VMInstruction.parse(line);
    try output.append(instruction);
  }
  return output;
}

pub fn main() !void {
  var vmMemory: [10 * 1024]u8 = undefined;
  var fba = std.heap.FixedBufferAllocator.init(&vmMemory);
  const allocator = fba.allocator();

  const instructions = try readInstructions(allocator);
  defer instructions.deinit();

  var vm = VMInstance {
    // .registers = [_]u64{0} ** 2, // for task 1
    .registers = [_]u64{1, 0},      // for task 2
    .pointer = 0,
    .instructions = instructions.items,
  };
  vm.run();

  const bValue = vm.register('b').*;
  _ = try stdout.print("Register b: {}\n", .{bValue});
}
