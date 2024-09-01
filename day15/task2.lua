require("luarocks.loader")
local inspect = require("inspect")

function table.keys(tbl)
  local output = {}
  for key in pairs(tbl) do
    output[#output + 1] = key
  end
  return output
end

local function read_ingredients(filename)
  local output = {}
  for line in io.lines(filename) do
    local name = string.match(line, "^%a+")
    local ingredient = {}
    for key, value in string.gmatch(line, "(%a+) (%-?%d+)") do
      ingredient[key] = tonumber(value, 10)
    end
    output[name] = ingredient
  end
  return output
end

local function get_recipes(ingredient_names, spoons)
  if #ingredient_names <= 0 then
    return {}
  end
  if #ingredient_names == 1 then
    local output = { { [ingredient_names[1]] = spoons } }
    return output
  end
  local output = {}
  local ingredient = ingredient_names[1]
  local rest_ingredients = {}
  for i = 2, #ingredient_names do
    rest_ingredients[#rest_ingredients + 1] = ingredient_names[i]
  end
  for spoon = 0, spoons do
    local rest_recipes = get_recipes(rest_ingredients, spoons - spoon)
    for _, recipe in pairs(rest_recipes) do
      recipe[ingredient] = spoon
      output[#output + 1] = recipe
    end
  end
  return output
end

local function recipe_score(recipe, ingredients)
  local parameters = {}
  for ingredient, quantity in pairs(recipe) do
    for parameter, value in pairs(ingredients[ingredient]) do
      local delta = quantity * value
      parameters[parameter] = (parameters[parameter] or 0) + delta
    end
  end
  local score = 1
  for parameter, value in pairs(parameters) do
    if parameter == "calories" and value ~= 500 then
      return -1
    end
    if parameter ~= "calories" then
      if value < 0 then
        return 0
      end
      score = score * value
    end
  end
  return score
end

local function max_recipe_score(recipes, ingredients)
  local max_score = -1
  local best_recipe = nil
  for _, recipe in pairs(recipes) do
    local current_score = recipe_score(recipe, ingredients)
    if current_score > max_score then
      max_score = current_score
      best_recipe = recipe
    end
  end
  return best_recipe, max_score
end

local function main()
  local ingredients = read_ingredients("input.txt")
  io.write("Spoons: ")
  local spoons = io.read("n*")
  local ingredient_names = table.keys(ingredients)
  local recipes = get_recipes(ingredient_names, spoons)
  print('Found ' .. #recipes .. " recipes")
  local best_recipe, max_score = max_recipe_score(recipes, ingredients)
  print('Best recipe: ' .. inspect(best_recipe))
  print('Max score: ' .. max_score)
end

main()
