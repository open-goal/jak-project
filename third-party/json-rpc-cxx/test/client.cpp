#include "doctest/doctest.h"
#include "testclientconnector.hpp"
#include <iostream>
#include <jsonrpccxx/client.hpp>

using namespace std;
using namespace jsonrpccxx;

struct F {
  TestClientConnector c;
  JsonRpcClient clientV1;
  JsonRpcClient clientV2;
  F() : c(), clientV1(c, version::v1), clientV2(c, version::v2) {}
};

TEST_CASE_FIXTURE(F, "v2_method_noparams") {
  c.SetResult(true);
  clientV2.CallMethod<json>("000-000-000", "some.method_1");
  c.VerifyMethodRequest(version::v2, "some.method_1", "000-000-000");
  CHECK(!has_key(c.request, "params"));
}

TEST_CASE_FIXTURE(F, "v1_method_noparams") {
  c.SetResult(true);
  clientV1.CallMethod<json>(37, "some.method_1");
  c.VerifyMethodRequest(version::v1, "some.method_1", 37);
  CHECK(has_key_type(c.request, "params", json::value_t::null));
}

TEST_CASE_FIXTURE(F, "v2_method_call_params_empty") {
  c.SetResult(true);
  clientV2.CallMethod<json>("1", "some.method_1", {});
  c.VerifyMethodRequest(version::v2, "some.method_1", "1");
  CHECK(c.request["params"].is_array());
  CHECK(c.request["params"].empty());
  CHECK(c.request["params"].dump() == "[]");

  c.SetResult(true);
  clientV2.CallMethod<json>("1", "some.method_1", json::array());
  c.VerifyMethodRequest(version::v2, "some.method_1", "1");
  CHECK(c.request["params"].is_array());
  CHECK(c.request["params"].empty());
  CHECK(c.request["params"].dump() == "[]");
}

TEST_CASE_FIXTURE(F, "v1_method_call_params_empty") {
  c.SetResult(true);
  clientV1.CallMethod<json>("1", "some.method_1", {});
  c.VerifyMethodRequest(version::v1, "some.method_1", "1");
  CHECK(c.request["params"].is_array());
  CHECK(c.request["params"].empty());
  CHECK(c.request["params"].dump() == "[]");

  c.SetResult(true);
  clientV1.CallMethod<json>("1", "some.method_1", json::array());
  c.VerifyMethodRequest(version::v1, "some.method_1", "1");
  CHECK(c.request["params"].is_array());
  CHECK(c.request["params"].empty());
  CHECK(c.request["params"].dump() == "[]");
}

TEST_CASE_FIXTURE(F, "v2_method_call_params_byname") {
  c.SetResult(true);
  clientV2.CallMethodNamed<json>("1", "some.method_1", {{"a", "hello"}, {"b", 77}, {"c", true}});
  c.VerifyMethodRequest(version::v2, "some.method_1", "1");
  CHECK(c.request["params"]["a"] == "hello");
  CHECK(c.request["params"]["b"] == 77);
  CHECK(c.request["params"]["c"] == true);
}

TEST_CASE_FIXTURE(F, "v1_method_call_params_byname") {
  c.SetResult(true);
  clientV1.CallMethodNamed<json>("1", "some.method_1", {{"a", "hello"}, {"b", 77}, {"c", true}});
  c.VerifyMethodRequest(version::v1, "some.method_1", "1");
  CHECK(c.request["params"]["a"] == "hello");
  CHECK(c.request["params"]["b"] == 77);
  CHECK(c.request["params"]["c"] == true);
}

TEST_CASE_FIXTURE(F, "v2_method_call_params_byposition") {
  c.SetResult(true);
  clientV2.CallMethod<json>("1", "some.method_1", {"hello", 77, true});
  c.VerifyMethodRequest(version::v2, "some.method_1", "1");
  CHECK(c.request["params"][0] == "hello");
  CHECK(c.request["params"][1] == 77);
  CHECK(c.request["params"][2] == true);
}

TEST_CASE_FIXTURE(F, "v1_method_call_params_byposition") {
  c.SetResult(true);
  clientV1.CallMethod<json>("1", "some.method_1", {"hello", 77, true});
  c.VerifyMethodRequest(version::v1, "some.method_1", "1");
  CHECK(c.request["params"][0] == "hello");
  CHECK(c.request["params"][1] == 77);
  CHECK(c.request["params"][2] == true);
}

TEST_CASE_FIXTURE(F, "v2_method_result_simple") {
  c.SetResult(23);
  int r = clientV2.CallMethod<int>("1", "some.method_1", {});
  c.VerifyMethodRequest(version::v2, "some.method_1", "1");
  CHECK(23 == r);
}

TEST_CASE_FIXTURE(F, "v1_method_result_simple") {
  c.SetResult(23);
  int r = clientV1.CallMethod<int>("1", "some.method_1", {});
  c.VerifyMethodRequest(version::v1, "some.method_1", "1");
  CHECK(23 == r);
}

TEST_CASE_FIXTURE(F, "v2_method_result_object") {
  c.SetResult({{"a", 3}, {"b", 4}});
  json r = clientV2.CallMethod<json>("1", "some.method_1", {});
  c.VerifyMethodRequest(version::v2, "some.method_1", "1");
  CHECK(r["a"] == 3);
  CHECK(r["b"] == 4);
}

TEST_CASE_FIXTURE(F, "v1_method_result_object") {
  c.SetResult({{"a", 3}, {"b", 4}});
  json r = clientV1.CallMethod<json>("1", "some.method_1", {});
  c.VerifyMethodRequest(version::v1, "some.method_1", "1");
  CHECK(r["a"] == 3);
  CHECK(r["b"] == 4);
}

TEST_CASE_FIXTURE(F, "v2_method_result_array") {
  c.SetResult({2, 3, 4});
  json r = clientV2.CallMethod<json>("1", "some.method_1", {});
  c.VerifyMethodRequest(version::v2, "some.method_1", "1");
  CHECK(r[0] == 2);
  CHECK(r[1] == 3);
  CHECK(r[2] == 4);
}

TEST_CASE_FIXTURE(F, "v1_method_result_array") {
  c.SetResult({2, 3, 4});
  json r = clientV1.CallMethod<json>("1", "some.method_1", {});
  c.VerifyMethodRequest(version::v1, "some.method_1", "1");
  CHECK(r[0] == 2);
  CHECK(r[1] == 3);
  CHECK(r[2] == 4);
}

TEST_CASE_FIXTURE(F, "v2_method_result_empty") {
  c.raw_response = "{}";
  REQUIRE_THROWS_WITH(clientV2.CallMethod<json>("1", "some.method_1", {}), "-32603: invalid server response: neither \"result\" nor \"error\" fields found");
  c.VerifyMethodRequest(version::v2, "some.method_1", "1");
  c.raw_response = "[]";
  REQUIRE_THROWS_WITH(clientV2.CallMethod<json>("1", "some.method_1", {}), "-32603: invalid server response: neither \"result\" nor \"error\" fields found");
  c.VerifyMethodRequest(version::v2, "some.method_1", "1");
}

/*
TEST_CASE_FIXTURE(F, "v1_method_result_empty") {
  c.raw_response = "{}";
  REQUIRE_THROWS_WITH(clientV1.CallMethod<json>("1", "some.method_1", {}),
                      Contains("result") && Contains("or") && Contains("error") && Contains("invalid server response"));
  c.VerifyMethodRequest(version::v1, "some.method_1", "1");
  c.raw_response = "[]";
  REQUIRE_THROWS_WITH(clientV1.CallMethod<json>("1", "some.method_1", {}),
                      Contains("result") && Contains("or") && Contains("error") && Contains("invalid server response"));
  c.VerifyMethodRequest(version::v1, "some.method_1", "1");
}

TEST_CASE_FIXTURE(F, "v2_method_error") {
  c.SetError(JsonRpcException{-32602, "invalid method name"});
  REQUIRE_THROWS_WITH(clientV2.CallMethod<json>("1", "some.method_1", {}), Contains("-32602") && Contains("invalid method name") && !Contains("data"));
  c.VerifyMethodRequest(version::v2, "some.method_1", "1");
}

TEST_CASE_FIXTURE(F, "v2_method_error_with_data") {
  c.SetError(JsonRpcException{-32602, "invalid method name", {1, 2}});
  REQUIRE_THROWS_WITH(clientV2.CallMethod<json>("1", "some.method_1", {}),
                      Contains("-32602") && Contains("invalid method name") && Contains("data") && Contains("[1,2]"));
  c.VerifyMethodRequest(version::v2, "some.method_1", "1");
}

TEST_CASE_FIXTURE(F, "v1_method_error") {
  c.SetError(JsonRpcException{-32602, "invalid method name"});
  REQUIRE_THROWS_WITH(clientV1.CallMethod<json>("1", "some.method_1", {}), Contains("-32602") && Contains("invalid method name") && !Contains("data"));
  c.VerifyMethodRequest(version::v1, "some.method_1", "1");
}

TEST_CASE_FIXTURE(F, "v1_method_error_with_data") {
  c.SetError(JsonRpcException{-32602, "invalid method name", {1, 2}});
  REQUIRE_THROWS_WITH(clientV1.CallMethod<json>("1", "some.method_1", {}),
                      Contains("-32602") && Contains("invalid method name") && Contains("data") && Contains("[1,2]"));
  c.VerifyMethodRequest(version::v1, "some.method_1", "1");
}

TEST_CASE_FIXTURE(F, "v2_method_error_invalid_json") {
  c.raw_response = "{asdfasdf,[}";
  REQUIRE_THROWS_WITH(clientV2.CallMethod<json>("1", "some.method_1", {}), Contains("-32700") && Contains("invalid") && Contains("JSON") && Contains("server"));
  c.VerifyMethodRequest(version::v2, "some.method_1", "1");
  c.raw_response = " ";
  REQUIRE_THROWS_WITH(clientV2.CallMethod<json>("1", "some.method_1", {}), Contains("-32700") && Contains("invalid") && Contains("JSON") && Contains("server"));
  c.VerifyMethodRequest(version::v2, "some.method_1", "1");
  c.raw_response = "";
  REQUIRE_THROWS_WITH(clientV2.CallMethod<json>("1", "some.method_1", {}), Contains("-32700") && Contains("invalid") && Contains("JSON") && Contains("server"));
  c.VerifyMethodRequest(version::v2, "some.method_1", "1");
}

TEST_CASE_FIXTURE(F, "v1_method_error_invalid_json") {
  c.raw_response = "{asdfasdf,[}";
  REQUIRE_THROWS_WITH(clientV1.CallMethod<json>("1", "some.method_1", {}), Contains("-32700") && Contains("invalid") && Contains("JSON") && Contains("server"));
  c.VerifyMethodRequest(version::v1, "some.method_1", "1");
  c.raw_response = " ";
  REQUIRE_THROWS_WITH(clientV1.CallMethod<json>("1", "some.method_1", {}), Contains("-32700") && Contains("invalid") && Contains("JSON") && Contains("server"));
  c.VerifyMethodRequest(version::v1, "some.method_1", "1");
  c.raw_response = "";
  REQUIRE_THROWS_WITH(clientV1.CallMethod<json>("1", "some.method_1", {}), Contains("-32700") && Contains("invalid") && Contains("JSON") && Contains("server"));
  c.VerifyMethodRequest(version::v1, "some.method_1", "1");
}

TEST_CASE_FIXTURE(F, "v2_notification_call_no_params") {
  c.raw_response = "";
  clientV2.CallNotification("some.notification_1", {});
  c.VerifyNotificationRequest(version::v2, "some.notification_1");
  CHECK(!has_key(c.request, "params"));

  c.raw_response = "";
  clientV2.CallNotification("some.notification_1");
  c.VerifyNotificationRequest(version::v2, "some.notification_1");
  CHECK(!has_key(c.request, "params"));
}

TEST_CASE_FIXTURE(F, "v1_notification_call_no_params") {
  c.raw_response = "";
  clientV1.CallNotification("some.notification_1", {});
  c.VerifyNotificationRequest(version::v1, "some.notification_1");
  CHECK(has_key_type(c.request, "params", json::value_t::null));

  c.raw_response = "";
  clientV1.CallNotification("some.notification_1");
  c.VerifyNotificationRequest(version::v1, "some.notification_1");
  CHECK(has_key_type(c.request, "params", json::value_t::null));
}

TEST_CASE_FIXTURE(F, "v2_notification_call_params_byname") {
  c.raw_response = "";
  clientV2.CallNotificationNamed("some.notification_1", {{"a", "hello"}, {"b", 77}, {"c", true}});
  c.VerifyNotificationRequest(version::v2, "some.notification_1");
  CHECK(c.request["params"]["a"] == "hello");
  CHECK(c.request["params"]["b"] == 77);
  CHECK(c.request["params"]["c"] == true);
}

TEST_CASE_FIXTURE(F, "v1_notification_call_params_byname") {
  c.raw_response = "";
  clientV1.CallNotificationNamed("some.notification_1", {{"a", "hello"}, {"b", 77}, {"c", true}});
  c.VerifyNotificationRequest(version::v1, "some.notification_1");
  CHECK(c.request["params"]["a"] == "hello");
  CHECK(c.request["params"]["b"] == 77);
  CHECK(c.request["params"]["c"] == true);
}

TEST_CASE_FIXTURE(F, "v2_notification_call_params_byposition") {
  c.raw_response = "";
  clientV2.CallNotification("some.notification_1", {"hello", 77, true});
  c.VerifyNotificationRequest(version::v2, "some.notification_1");
  CHECK(c.request["params"][0] == "hello");
  CHECK(c.request["params"][1] == 77);
  CHECK(c.request["params"][2] == true);
}

TEST_CASE_FIXTURE(F, "v1_notification_call_params_byposition") {
  c.raw_response = "";
  clientV1.CallNotification("some.notification_1", {"hello", 77, true});
  c.VerifyNotificationRequest(version::v1, "some.notification_1");
  CHECK(c.request["params"][0] == "hello");
  CHECK(c.request["params"][1] == 77);
  CHECK(c.request["params"][2] == true);
}*/

// TODO: test cases with return type mapping and param mapping for v1/v2 method and notification
