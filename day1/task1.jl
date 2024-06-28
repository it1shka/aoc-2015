open("input.txt") do file
  floor :: Int32 = 0
  while !eof(file)
    symbol :: Char = read(file, Char)
    if symbol == '('
      floor += 1
    elseif symbol == ')'
      floor -= 1
    end
  end
  println(floor)
end
