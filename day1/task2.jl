function entrance(stream :: IOStream) :: Int32
  floor :: Int32 = 0
  position :: Int32 = 1
  while !eof(stream)
    symbol :: Char = read(stream, Char)
    if symbol == '('
      floor += 1
    elseif symbol == ')'
      floor -= 1
    end
    if floor < 0 
      return position 
    end
    position += 1
  end
  return -1
end

open("input.txt") do file
  answer :: Int32 = entrance(file)
  println(answer)
end
