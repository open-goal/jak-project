#pragma once
#include <nlohmann/json.hpp>

enum class category { order, cash_carry };

struct Product {
public:
  Product() : id(), price(), name(), cat() {}
  std::string id;
  double price;
  std::string name;
  category cat;
};

NLOHMANN_JSON_SERIALIZE_ENUM(category, {{category::order, "order"}, {category::cash_carry, "cc"}})

inline void to_json(nlohmann::json &j, const Product &p) {
  j = nlohmann::json{{"id", p.id},
		     {"price", p.price},
		     {"name", p.name},
		     {"category", p.cat}};
 }

inline void from_json(const nlohmann::json &j, Product &p) {
  j.at("name").get_to(p.name);
  j.at("id").get_to(p.id);
  j.at("price").get_to(p.price);
  j.at("category").get_to(p.cat);
}
