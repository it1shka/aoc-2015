#include <functional>
#include <vector>
#include <tuple>
#include <string>
#include <fstream>
#include <sstream>
#include <utility>
#include <iostream>
#include <optional>
#include <queue>
#include <set>

struct Task final {
  std::vector<std::tuple<std::string, std::string>> rules;
  std::string medicine;

  [[nodiscard]]
  static auto fromFile(const std::string& filename) -> Task {
    std::ifstream input(filename);
    Task task;
    for (std::string line; std::getline(input, line); ) {
      if (line.empty()) {
        break;
      }
      std::istringstream lineStream(line);
      std::string first, second;
      lineStream >> first >> second >> second;
      task.rules.push_back({std::move(first), std::move(second)});
    }
    input >> task.medicine;
    return task;
  }

  auto print() const noexcept -> void {
    std::cout << "Rules:\n";
    for (const auto& [first, second] : rules) {
      std::cout << first << " => " << second << '\n';
    }
    std::cout << "\nMedicine:\n";
    std::cout << medicine << std::endl;
  }

  [[nodiscard]]
  auto solve() const -> std::optional<unsigned> {
    using Step = std::tuple<std::string, unsigned>;
    using Queue = std::priority_queue<
      Step,
      std::vector<Step>,
      std::function<bool(const Step&, const Step&)>
    >;
    Queue queue([](const Step& a, const Step& b) -> bool {
      const auto sizeA = std::get<0>(a).size();
      const auto sizeB = std::get<0>(b).size();
      return sizeA == sizeB
        ? std::get<1>(a) - std::get<1>(b)
        : sizeA - sizeB;
    });
    std::set<std::string> visited;
    queue.push({medicine, 0});
    visited.insert(medicine);
    while (!queue.empty()) {
      const auto [formula, count] = queue.top();
      queue.pop();
      if (formula == "e") {
        return count;
      }
      for (const auto& [first, second] : rules) {
        for (auto i = formula.find(second, 0); i != std::string::npos; i = formula.find(second, i + 1)) {
          auto nextFormula = formula;
          nextFormula.replace(i, second.size(), first);
          if (visited.contains(nextFormula)) {
            continue;
          }
          queue.push({nextFormula, count + 1});
          visited.insert(nextFormula);
        }
      }
    }
    return {};
  }
};



auto main() -> int {
  const auto task = Task::fromFile("input.txt");
  task.print();
  const auto result = task.solve();
  if (result.has_value()) {
    std::cout << "\nResult: " << result.value() << std::endl;
  } else {
    std::cout << "\nResult: none" << std::endl; 
  }
}
