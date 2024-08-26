#include <cctype>
#include <cstdint>
#include <functional>
#include <iostream>
#include <optional>
#include <queue>
#include <vector>
#include <string>
#include <unordered_map>
#include <fstream>
#include <sstream>

using element = std::uint8_t;
using expression = std::vector<element>;

auto levenshteinDist(const expression& word1, const expression& word2)
    -> size_t {
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

  return verif[size1][size2];
}

class chemical_encoding {
  std::unordered_map<std::string, element> encode_table;
  std::unordered_map<element, std::string> decode_table;
  element encoding_index;
public:
  chemical_encoding() : encoding_index(0) {}
  [[nodiscard]]
  auto encode(const std::string& chemical_expression) noexcept -> expression {
    std::string buffer;
    expression element_vector;
    for (std::size_t i = 0; i < chemical_expression.size(); ) {
      buffer = chemical_expression.at(i);
      i++;
      while (i < chemical_expression.size() && std::islower(chemical_expression[i])) {
        buffer += chemical_expression[i];
        i++;
      }
      if (encode_table.contains(buffer)) {
        element_vector.push_back(encode_table[buffer]);
        continue;
      }
      encode_table[buffer] = encoding_index;
      element_vector.push_back(encoding_index);
      encoding_index++;
    }
    return element_vector;
  }
  [[nodiscard]]
  auto decode(const expression& elements) const noexcept -> std::string {
    std::string output;
    for (const auto each : elements) {
      if (decode_table.contains(each)) {
        continue;
      }
      output += decode_table.at(each);
    }
    return output;
  }
};

chemical_encoding global_encoding;

struct task {
  std::vector<std::tuple<element, expression>> rules;
  expression desired;

  [[nodiscard]]
  static auto read(const std::string& filename) -> task {
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
      auto original = global_encoding.encode(key).at(0);
      auto replacement = global_encoding.encode(value);
      task.rules.push_back({original, replacement});
    }
    std::string desired;
    input >> desired;
    task.desired = global_encoding.encode(desired);
    return task;
  }

  [[nodiscard]]
  auto solve() const -> std::optional<unsigned> {
    using step = std::tuple<expression, unsigned>;
    using priority_queue = std::priority_queue<
      step, 
      std::vector<step>,
      std::function<bool(const step&, const step&)>
    >;
    priority_queue queue;
    // TODO: ...
    return {};
  }
};

auto main() -> int {
  constexpr auto filename = "input.txt";
  const auto task_instance = task::read(filename);
  const auto result = task_instance.solve();
  if (!result.has_value()) {
    std::cout << "No solution" << std::endl;
  } else {
    std::cout << "Result: " << result.value() << std::endl;
  }
}
