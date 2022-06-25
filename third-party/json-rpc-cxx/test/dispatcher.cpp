#include "doctest/doctest.h"
#include <iostream>
#include <jsonrpccxx/dispatcher.hpp>

using namespace jsonrpccxx;
using namespace std;

static string procCache;
unsigned int add_function(unsigned int a, unsigned int b) { return a + b; }
void some_procedure(const string &param) { procCache = param; }

TEST_CASE("add and invoke positional") {
  Dispatcher d;
  CHECK(d.Add("some method", GetHandle(&add_function)));
  CHECK(!d.Add("some method", GetHandle(&add_function)));
  CHECK(d.InvokeMethod("some method", {11, 22}) == 33);

  procCache = "";
  CHECK(d.Add("some notification", GetHandle(&some_procedure)));
  CHECK(!d.Add("some notification", GetHandle(&some_procedure)));
  d.InvokeNotification("some notification", {"some string"});
  CHECK(procCache == "some string");
}

TEST_CASE("invoking supported named parameter") {
  Dispatcher d;
  CHECK(d.Add("some method", GetHandle(&add_function), {"a", "b"}));
  REQUIRE(d.InvokeMethod("some method", {{"a", 11}, {"b", 22}}) == 33);

  procCache = "";
  CHECK(d.Add("some notification", GetHandle(&some_procedure), {"param"}));
  json p = {{"param", "some string"}};
  d.InvokeNotification("some notification", p);
  CHECK(procCache == "some string");
}

TEST_CASE("invoking missing named parameter") {
  Dispatcher d;
  CHECK(d.Add("some method", GetHandle(&add_function), {"a", "b"}));
  REQUIRE_THROWS_WITH(d.InvokeMethod("some method", {{"a", 11}, {"xx", 22}}), "-32602: invalid parameter: missing named parameter \"b\"");

  procCache = "";
  CHECK(d.Add("some notification", GetHandle(&some_procedure), {"param"}));
  json p = {{"param2", "some string"}};
  REQUIRE_THROWS_WITH(d.InvokeNotification("some notification", p), "-32602: invalid parameter: missing named parameter \"param\"");
  CHECK(procCache.empty());
}

TEST_CASE("invoking wrong type namedparameter") {
  Dispatcher d;
  CHECK(d.Add("some method", GetHandle(&add_function), {"a", "b"}));
  REQUIRE_THROWS_WITH(d.InvokeMethod("some method", {{"a", "asdfasdf"}, {"b", -7}}), "-32602: invalid parameter: must be unsigned integer, but is string for parameter \"a\"");
  REQUIRE_THROWS_WITH(d.InvokeMethod("some method", {{"a", -10}, {"b", -7}}), "-32602: invalid parameter: must be unsigned integer, but is integer for parameter \"a\"");
}

TEST_CASE("error on invoking unsupported named parameter") {
  Dispatcher d;
  CHECK(d.Add("some method", GetHandle(&add_function)));
  REQUIRE_THROWS_WITH(d.InvokeMethod("some method", {{"a", 11}, {"b", 22}}), "-32602: invalid parameter: procedure doesn't support named parameter");

  CHECK(d.Add("some notification", GetHandle(&some_procedure)));
  json p = {{"param", "some string"}};
  REQUIRE_THROWS_WITH(d.InvokeNotification("some notification", p), "-32602: invalid parameter: procedure doesn't support named parameter");
}

TEST_CASE("passing invalid literal as param") {
    Dispatcher d;
    CHECK(d.Add("some method", GetHandle(&add_function)));
    REQUIRE_THROWS_WITH(d.InvokeMethod("some method", true), "-32600: invalid request: params field must be an array, object");
}

TEST_CASE("dispatching unknown procedures") {
    Dispatcher d;
    REQUIRE_THROWS_WITH(d.InvokeMethod("some method", {1}), "-32601: method not found: some method");
    REQUIRE_THROWS_WITH(d.InvokeNotification("some notification", {1}), "-32601: notification not found: some notification");
}

TEST_CASE("invalid param types") {
    Dispatcher d;
    CHECK(d.Add("some method", GetHandle(&add_function)));
    CHECK_THROWS_WITH(d.InvokeMethod("some method", {"string1", "string2"}), "-32602: invalid parameter: must be unsigned integer, but is string for parameter 0");
}

// TODO: avoid signed, unsigned bool invocations