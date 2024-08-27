local function map(table, transform)
  local output = {}
  for index, value in pairs(table) do
    output[index] = transform(value, index)
  end
  return output
end

local function filter(table, selector)
  local output = {}
  for index, value in ipairs(table) do
    if selector(value, index) then
      output[#output + 1] = value
    end
  end
  return output
end

local function sum(table)
  local output = 0
  for _, each in pairs(table) do
    output = output + each
  end
  return output
end

local function concat_arrays(a, b)
  local output = {}
  for _, each in ipairs(a) do
    output[#output + 1] = each
  end
  for _, each in ipairs(b) do
    output[#output + 1] = each
  end
  return output
end

local function powerset(source)
  if #source <= 0 then
    return {{}}
  end
  local head = source[1]
  local tail = { select(2, table.unpack(source)) }
  local without = powerset(tail)
  local with = map(without, function (elem)
    return {head, table.unpack(elem)}
  end)
  return concat_arrays(without, with)
end

local function solution(liters, containers)
  local S = powerset(containers)
  local sums = map(S, sum)
  return #filter(sums, function (elem)
    return elem == liters
  end)
end

local function read_containers(filename)
  local containers = {}
  for line in io.lines(filename) do
    containers[#containers + 1] = tonumber(line, 10)
  end
  return containers
end


local function main ()
  io.write("Liters: ")
  local liters = io.read("n*")
  local containers = read_containers("input.txt")
  local answer = solution(liters, containers)
  io.write("Answer: " .. answer .. "\n")
end

main()
