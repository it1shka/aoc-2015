local function table_keys(tbl)
  local output = {}
  for key in pairs(tbl) do
    output[#output + 1] = key
  end
  return output
end

local function array_tail(array)
  local output = {}
  for index = 2, #array do
    output[#output + 1] = array[index]
  end
  return output
end

local function read_input(filename)
  local output = {}
  for line in io.lines(filename) do
    local pivot = string.match(line, "^%a+")
    local modifier, amount = string.match(line, "(%a+)%s(%d+)")
    local lateral = string.match(line, "to (%a+)")
    local coefficient
    if modifier == "gain" then
      coefficient = 1
    else
      coefficient = -1
    end
    if not output[pivot] then
      output[pivot] = {}
    end
    output[pivot][lateral] = coefficient * tonumber(amount, 10)
  end
  return output
end

local function permutations(array)
  if #array == 0 then
    return {}
  end
  if #array == 1 then
    return {{array[1]}}
  end
  local output = {}
  local base = permutations(array_tail(array))
  for _, each in pairs(base) do
    for insert = 1, #each + 1 do
      local copy = { table.unpack(each) }
      table.insert(copy, insert, array[1])
      output[#output + 1] = copy
    end
  end
  return output
end

local function rate_possibility(people, info)
  local output = 0
  for i = 1, #people do
    local prev, next
    if i == 1 then
      prev = #people
      next = i + 1
    elseif i == #people then
      prev = i - 1
      next = 1
    else
      prev = i - 1
      next = i + 1
    end
    local pivot = people[i]
    local lateral_prev = people[prev]
    local lateral_next = people[next]
    output =
      output +
      info[pivot][lateral_prev] +
      info[pivot][lateral_next]
  end
  return output
end

local function main()
  local input = read_input("input.txt")
  local people = table_keys(input)
  local possibilities = permutations(people)
  local max = 0
  for _, possibility in pairs(possibilities) do
    local rating = rate_possibility(possibility, input)
    max = math.max(max, rating)
  end
  print("Max: " .. max)
end

main()
