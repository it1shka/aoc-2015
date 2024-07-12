{$mode objfpc}

type
  pair = array[0..1] of integer;
  positions = array of pair;
  positions_ref = ^positions;
  santa = class
    private
      position: pair;
      visited: positions_ref;
    public
      constructor init (
        const init_position: pair; 
        const init_visited: positions_ref
      );
      procedure follow(const direction: char);
  end;

constructor santa.init (
  const init_position: pair; 
  const init_visited: positions_ref
); 
begin
  self.position := init_position;
  self.visited := init_visited;
end;

procedure santa.follow(const direction: char);
var 
  i: integer;
begin
  case direction of
    '^': self.position[0] := self.position[0] - 1;
    '>': self.position[1] := self.position[1] + 1;
    'v': self.position[0] := self.position[0] + 1;
    '<': self.position[1] := self.position[1] - 1;
  end;
  for i := low(self.visited^) to high(self.visited^) do begin
    if (self.position[0] = self.visited^[i][0]) and (self.position[1] = self.visited^[i][1]) then
      exit;
  end;
  setlength(self.visited^, length(self.visited^) + 1);
  self.visited^[high(self.visited^)] := self.position;
end;

const init_position: pair = (0, 0);
var
  input: file of char;
  symbol: char;
  bob: santa;
  visited: positions;
begin
  visited := [init_position];
  bob := santa.init(init_position, @visited);
  assign(input, 'input.txt');
  reset(input);
  while not eof(input) do begin
    read(input, symbol);
    bob.follow(symbol);
  end;
  close(input);
  writeln(length(visited));
end.
