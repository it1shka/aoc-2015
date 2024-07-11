{$mode objfpc}

uses strutils, sysutils, math;

type
  grid2d = array[0..999, 0..999] of integer;
  pair = array[1..2] of integer;
  command = record
    kind: (on, off, toggle);
    start: pair;
    finish: pair;
  end;

function parse_range(const raw: string): pair;
var
  parts: array of ansistring;
begin
  parts := splitstring(raw, ',');
  result[1] := strtoint(parts[0]);
  result[2] := strtoint(parts[1]);
end;

function parse_command(const line: string): command;
var
  parts: array of ansistring;
begin
  parts := splitstring(line, ' ');
  if comparestr(parts[0], 'toggle') = 0 then begin
    result.kind := toggle;
    result.start := parse_range(parts[1]);
    result.finish := parse_range(parts[3]);
    exit;
  end;
  if comparestr(parts[1], 'on') = 0 then
    result.kind := on
  else
    result.kind := off;
  result.start := parse_range(parts[2]);
  result.finish := parse_range(parts[4]);
end;

procedure init_grid(grid: grid2d);
var
  i: integer;
  j: integer;
begin
  for i := 0 to 999 do
    for j := 0 to 999 do
      grid[i][j] := 0;
end;

var
  input: textfile;
  line: string[64] = '';
  grid: grid2d;
  current_command: command;
  i: integer;
  j: integer;
  total: integer = 0;
begin
  init_grid(grid);
  assign(input, 'input.txt');
  reset(input);
  while not eof(input) do begin
    readln(input, line);
    current_command := parse_command(line);
    for i := current_command.start[1] to current_command.finish[1] do
      for j := current_command.start[2] to current_command.finish[2] do
        case current_command.kind of
          toggle: grid[i][j] := grid[i][j] + 2;
          on: grid[i][j] := grid[i][j] + 1;
          off: grid[i][j] := max(0, grid[i][j] - 1);
        end;
  end;
  close(input);
  for i := 0 to 999 do
    for j := 0 to 999 do
      total := total + grid[i][j];
  writeln(total);
end.
