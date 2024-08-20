#include <fstream>
#include <functional>
#include <iostream>
#include <optional>
#include <queue>
#include <sstream>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#define LOGGING

auto levenshteinDist(const std::string &word1, const std::string &word2)
    -> size_t {
  static std::unordered_map<std::string, size_t> cache;
  const auto keyA = word1 + "," + word2;
  if (cache.contains(keyA)) {
    return cache[keyA];
  }
  const auto keyB = word2 + "," + word1;
  if (cache.contains(keyB)) {
    return cache[keyB];
  }

  const auto size1 = word1.size();
  const auto size2 = word2.size();
  size_t verif[size1 + 1][size2 + 1];

  if (size1 == 0)
    return size2;
  if (size2 == 0)
    return size1;

  for (size_t i = 0; i <= size1; i++)
    verif[i][0] = i;
  for (size_t j = 0; j <= size2; j++)
    verif[0][j] = j;

  for (size_t i = 1; i <= size1; i++) {
    for (size_t j = 1; j <= size2; j++) {
      size_t cost = (word2[j - 1] == word1[i - 1]) ? 0 : 1;
      verif[i][j] = std::min(std::min(verif[i - 1][j] + 1, verif[i][j - 1] + 1),
                             verif[i - 1][j - 1] + cost);
    }
  }

  const auto result = verif[size1][size2];
  cache[keyA] = result;
  cache[keyB] = result;
  return result;
}

using mapping = std::vector<std::tuple<std::string, std::string>>;

struct task {
  mapping mapping;
  std::string desired;
};

auto read_task(const std::string &filename) -> task {
  std::ifstream input(filename);
  task task;
  for (std::string line; std::getline(input, line);) {
    if (line.empty())
      break;
    std::istringstream line_stream(line);
    std::string key, value;
    line_stream >> key;
    line_stream >> value;
    line_stream >> value;
    task.mapping.push_back({key, value});
  }
  input >> task.desired;
  return task;
}

struct step {
  std::string expression;
  unsigned count;
};

using priority_queue =
    std::priority_queue<step, std::vector<step>,
                        std::function<bool(const step &, const step &)>>;

auto solve_task(const task &task) -> std::optional<unsigned> {
  priority_queue queue([&task](const step &a, const step &b) -> bool {
    const auto distA = levenshteinDist(a.expression, task.desired);
    const auto distB = levenshteinDist(b.expression, task.desired);
    return distA == distB ? a.count > b.count : distA > distB;
  });
  std::unordered_set<std::string> visited;
  queue.push({"e", 0});
  visited.insert("e");

  while (!queue.empty()) {
    const auto [expression, count] = queue.top();
    queue.pop();
#ifdef LOGGING
    std::cout 
      << expression << " (step " << count 
      << ", distance " << levenshteinDist(expression, task.desired) << ")" 
      << std::endl;
#endif
    if (expression == task.desired) {
      return count;
    }
    for (const auto &[original, replacement] : task.mapping) {
      for (auto i = expression.find(original, 0); i != std::string::npos;
           i = expression.find(original, i + 1)) {
        auto next_expression = expression;
        next_expression.replace(i, original.size(), replacement);
        if (visited.contains(next_expression))
          continue;
        queue.push({next_expression, count + 1});
        visited.insert(next_expression);
      }
    }
  }

  return {};
}

auto main() -> int {
  constexpr auto filename = "input.txt";
  const auto task = read_task(filename);
  const auto result = solve_task(task);
  if (!result.has_value()) {
    std::cout << "No result" << std::endl;
  } else {
    std::cout << "Result: " << result.value() << std::endl;
  }
}
