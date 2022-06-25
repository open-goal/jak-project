#include "doctest/doctest.h"
#include <iostream>
#include <jsonrpccxx/typemapper.hpp>
#include <limits>

using namespace jsonrpccxx;
using namespace std;

static string notifyResult = "";

int add(int a, int b) { return a + b; }
void notify(const std::string &hello) { notifyResult = string("Hello world: ") + hello; }

class SomeClass {
public:
  int add(int a, int b) { return a + b; }
  void notify(const std::string &hello) { notifyResult = string("Hello world: ") + hello; }
};

TEST_CASE("test function binding") {
  MethodHandle mh = GetHandle(&add);
  CHECK(mh(R"([3, 4])"_json) == 7);

  notifyResult = "";
  NotificationHandle mh2 = GetHandle(&notify);
  CHECK(notifyResult.empty());
  mh2(R"(["someone"])"_json);
  CHECK(notifyResult == "Hello world: someone");
}

TEST_CASE("test class member binding") {
  SomeClass instance;
  MethodHandle mh = GetHandle(&SomeClass::add, instance);
  CHECK(mh(R"([3, 4])"_json) == 7);

  notifyResult = "";
  NotificationHandle mh2 = GetHandle(&SomeClass::notify, instance);
  CHECK(notifyResult.empty());
  mh2(R"(["someone"])"_json);
  CHECK(notifyResult == "Hello world: someone");
}

TEST_CASE("test class member explicit binding") {
  SomeClass instance;
  MethodHandle mh = methodHandle(&SomeClass::add, instance);
  CHECK(mh(R"([3, 4])"_json) == 7);

  notifyResult = "";
  NotificationHandle mh2 = notificationHandle(&SomeClass::notify, instance);
  CHECK(notifyResult.empty());
  mh2(R"(["someone"])"_json);
  CHECK(notifyResult == "Hello world: someone");
}

TEST_CASE("test incorrect params") {
  SomeClass instance;
  MethodHandle mh = GetHandle(&SomeClass::add, instance);
  REQUIRE_THROWS_WITH(mh(R"(["3", "4"])"_json), "-32602: invalid parameter: must be integer, but is string, data: 0");
  REQUIRE_THROWS_WITH(mh(R"([true, true])"_json), "-32602: invalid parameter: must be integer, but is boolean, data: 0");
  REQUIRE_THROWS_WITH(mh(R"([null, 3])"_json), "-32602: invalid parameter: must be integer, but is null, data: 0");
  REQUIRE_THROWS_WITH(mh(R"([{"a": true}, 3])"_json), "-32602: invalid parameter: must be integer, but is object, data: 0");
  REQUIRE_THROWS_WITH(mh(R"([[2,3], 3])"_json), "-32602: invalid parameter: must be integer, but is array, data: 0");
  REQUIRE_THROWS_WITH(mh(R"([3.4, 3])"_json), "-32602: invalid parameter: must be integer, but is float, data: 0");
  REQUIRE_THROWS_WITH(mh(R"([4])"_json), "-32602: invalid parameter: expected 2 argument(s), but found 1");
  REQUIRE_THROWS_WITH(mh(R"([5, 6, 5])"_json), "-32602: invalid parameter: expected 2 argument(s), but found 3");

  notifyResult = "";
  NotificationHandle mh2 = GetHandle(&SomeClass::notify, instance);
  REQUIRE_THROWS_WITH(mh2(R"([33])"_json), "-32602: invalid parameter: must be string, but is unsigned integer, data: 0");
  REQUIRE_THROWS_WITH(mh2(R"([-33])"_json), "-32602: invalid parameter: must be string, but is integer, data: 0");
  REQUIRE_THROWS_WITH(mh2(R"(["someone", "anotherone"])"_json), "-32602: invalid parameter: expected 1 argument(s), but found 2");
  REQUIRE_THROWS_WITH(mh2(R"([])"_json), "-32602: invalid parameter: expected 1 argument(s), but found 0");
  REQUIRE_THROWS_WITH(mh2(R"([true])"_json), "-32602: invalid parameter: must be string, but is boolean, data: 0");
  REQUIRE_THROWS_WITH(mh2(R"([null])"_json), "-32602: invalid parameter: must be string, but is null, data: 0");
  REQUIRE_THROWS_WITH(mh2(R"([3.4])"_json), "-32602: invalid parameter: must be string, but is float, data: 0");
  REQUIRE_THROWS_WITH(mh2(R"([{"a": true}])"_json), "-32602: invalid parameter: must be string, but is object, data: 0");
  REQUIRE_THROWS_WITH(mh2(R"([["some string"]])"_json), "-32602: invalid parameter: must be string, but is array, data: 0");

  CHECK(notifyResult.empty());
}

enum class category { order, cash_carry };

struct product {
  product() : id(), price(), name(), cat() {}
  int id;
  double price;
  string name;
  category cat;
};

NLOHMANN_JSON_SERIALIZE_ENUM(category, {{category::order, "order"}, {category::cash_carry, "cc"}})

void to_json(json &j, const product &p) { j = json{{"id", p.id}, {"price", p.price}, {"name", p.name}, {"category", p.cat}}; }

product get_product(int id) {
  if (id == 1) {
    product p;
    p.id = 1;
    p.price = 22.50;
    p.name = "some product";
    p.cat = category::order;
    return p;
  }
  else if (id == 2) {
    product p;
    p.id = 2;
    p.price = 55.50;
    p.name = "some product 2";
    p.cat = category::cash_carry;
    return p;
  }
  throw JsonRpcException(-50000, "product not found");
}

TEST_CASE("test with custom struct return") {
  MethodHandle mh = GetHandle(&get_product);
  json j = mh(R"([1])"_json);
  CHECK(j["id"] == 1);
  CHECK(j["name"] == "some product");
  CHECK(j["price"] == 22.5);
  CHECK(j["category"] == category::order);

  j = mh(R"([2])"_json);
  CHECK(j["id"] == 2);
  CHECK(j["name"] == "some product 2");
  CHECK(j["price"] == 55.5);
  CHECK(j["category"] == category::cash_carry);

  REQUIRE_THROWS_WITH(mh(R"([444])"_json), "-50000: product not found");
}

void from_json(const json &j, product &p) {
  j.at("name").get_to(p.name);
  j.at("id").get_to(p.id);
  j.at("price").get_to(p.price);
  j.at("category").get_to(p.cat);
}

static vector<product> catalog;
bool add_products(const vector<product> &products) {
  std::copy(products.begin(), products.end(), std::back_inserter(catalog));
  return true;
}

string enumToString(const category& category) {
  switch (category) {
  case category::cash_carry: return "cash&carry";
  case category::order: return "online-order";
  default: return "unknown category";
  }
}

TEST_CASE("test with enum as top level parameter") {
  MethodHandle  mh = GetHandle(&enumToString);

  json params = R"(["cc"])"_json;
  CHECK(mh(params) == "cash&carry");
}

TEST_CASE("test with custom params") {
  MethodHandle mh = GetHandle(&add_products);
  catalog.clear();
  json params =
      R"([[{"id": 1, "price": 22.50, "name": "some product", "category": "order"}, {"id": 2, "price": 55.50, "name": "some product 2", "category": "cc"}]])"_json;

  CHECK(mh(params) == true);
  REQUIRE(catalog.size() == 2);

  CHECK(catalog[0].id == 1);
  CHECK(catalog[0].name == "some product");
  CHECK(catalog[0].price == 22.5);
  CHECK(catalog[0].cat == category::order);
  CHECK(catalog[1].id == 2);
  CHECK(catalog[1].name == "some product 2");
  CHECK(catalog[1].price == 55.5);
  CHECK(catalog[1].cat == category::cash_carry);

  REQUIRE_THROWS_WITH(mh(R"([[{"id": 1, "price": 22.50}]])"_json), "[json.exception.out_of_range.403] key 'name' not found");
  REQUIRE_THROWS_WITH(mh(R"([{"id": 1, "price": 22.50}])"_json), "-32602: invalid parameter: must be array, but is object, data: 0");
}

unsigned long unsigned_add(unsigned int a, int b) { return a + b; }
unsigned long unsigned_add2(unsigned short a, short b) { return a + b; }
float float_add(float a, float b) { return a+b; }

TEST_CASE("test number range checking") {
  MethodHandle mh = GetHandle(&unsigned_add);

  REQUIRE_THROWS_WITH(mh(R"([-3,3])"_json), "-32602: invalid parameter: must be unsigned integer, but is integer, data: 0");
  REQUIRE_THROWS_WITH(mh(R"([null,3])"_json), "-32602: invalid parameter: must be unsigned integer, but is null, data: 0");

  unsigned int max_us = numeric_limits<unsigned int>::max();
  unsigned int max_s = numeric_limits<int>::max();
  CHECK(mh({max_us, max_s}) == max_us + max_s);
  REQUIRE_THROWS_WITH(mh({max_us, max_us}), "-32602: invalid parameter: exceeds value range of integer, data: 1");

  MethodHandle mh2 = GetHandle(&unsigned_add2);
  unsigned short max_su = numeric_limits<unsigned short>::max();
  unsigned short max_ss = numeric_limits<short>::max();
  CHECK(mh2({max_su, max_ss}) == max_su + max_ss);
  REQUIRE_THROWS_WITH(mh2({max_su, max_su}), "-32602: invalid parameter: exceeds value range of integer, data: 1");
}

TEST_CASE("test auto conversion of float to int passed to float method") {
  MethodHandle mh = GetHandle(&float_add);
  CHECK(mh(R"([3,3])"_json) == 6.0);
  CHECK(mh(R"([3.0,3.0])"_json) == 6.0);
  CHECK(mh(R"([3.1,3.2])"_json) == doctest::Approx(6.3));
}

json arbitrary_json(const json& value) {
  return value;
}

void arbitrary_json_notification(const json& value) {
  to_string(value);
}

TEST_CASE("test json method handles without specific types") {
  MethodHandle mh = GetUncheckedHandle(&arbitrary_json);
  CHECK(mh(R"([3,"string"])"_json) == R"([3,"string"])"_json);
  auto param = R"({"a": "string"})"_json;
  CHECK(mh(param) == param);

  NotificationHandle nh = GetUncheckedNotificationHandle(&arbitrary_json_notification);
  nh(R"([3,"string"])"_json);
  nh(R"({"3": "string"})"_json);
}