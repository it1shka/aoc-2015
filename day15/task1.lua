---@param tbl table
---@return table
function table.clone(tbl)
  local output = {}
  for key, value in pairs(tbl) do
    output[key] = value
  end
  return output
end

---@param ingredient table
local function dump_ingredient(ingredient)
  io.write("--" .. ingredient.name .. "--\n")
  for key, value in pairs(ingredient) do
    if key ~= "name" then
      io.write(key .. ": " .. value .. "\n")
    end
  end
end

---@param filename string
---@return table
local function read_ingredients(filename)
  local output = {}
  for line in io.lines(filename) do
    local name = string.match(line, "^%a+")
    local ingredient = { name = name }
    for key, value in string.gmatch(line, "(%a+) (%d+)") do
      ingredient[key] = tonumber(value, 10)
    end
    output[#output + 1] = ingredient
  end
  return output
end

---@param ingredients table
---@param recipe table
---@return integer
local function rate_recipe(ingredients, recipe)
  local rating = 1
  return rating
end

---@param recipe table
---@return integer
local function spoons_amount(recipe)
  local spoons = 0
  for _, value in pairs(recipe) do
    spoons = spoons + value
  end
  return spoons
end

---@param ingredients table
---@param spoons integer
---@return integer
local function highest_score(ingredients, spoons)
  local initial = {}
  for key in pairs(ingredients) do
    initial[key] = 0
  end
  local best = -1
  local stack = { initial }
  while #stack > 0 do
    local recipe = stack[#stack]
    stack[#stack] = nil
    if spoons_amount(recipe) >= spoons then
      local current_rating = rate_recipe(ingredients, recipe)
      best = math.max(best, current_rating)
      io.write("Best: " .. best .. "\n")
    else
      for ig in pairs(ingredients) do
        local new_recipe = table.clone(recipe)
        new_recipe[ig] = new_recipe[ig] + 1
        stack[#stack + 1] = new_recipe
      end
    end
  end
  return best
end

local function main()
  local ingredients = read_ingredients("input.txt")
  io.write("Loaded ingredients:\n\n")
  for _, each in ipairs(ingredients) do
    dump_ingredient(each)
    io.write("\n")
  end
  io.write("\n")

  io.write("Select spoons: ")
  local spoons = io.read("n*")
  assert(type(spoons) == "number")

  local score = highest_score(ingredients, spoons)
  io.write("Highest score: " .. score .. "\n")
end

main()
