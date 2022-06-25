#pragma once
#include "doctest/doctest.h"
#include <jsonrpccxx/common.hpp>
#include <jsonrpccxx/client.hpp>
#include <jsonrpccxx/iclientconnector.hpp>
#include <nlohmann/json.hpp>
#include <string>

using namespace jsonrpccxx;
using namespace std;

class TestClientConnector : public IClientConnector {
public:
  TestClientConnector() : request(), raw_response() {}

  json request;
  string raw_response;

  std::string Send(const std::string &r) override {
    this->request = json::parse(r);
    return raw_response;
  }

  void SetBatchResult(const json &result) {
    raw_response = result.dump();
  }

  static json BuildResult(const json &result, int id) {
    return {{"jsonrpc", "2.0"}, {"id", id}, {"result", result}};
  }

  void SetResult(const json &result) {
    json response = {{"jsonrpc", "2.0"}, {"id", "1"}, {"result", result}};
    raw_response = response.dump();
  }

  void SetError(const JsonRpcException &e) {
    json response = {{"jsonrpc", "2.0"}, {"id", "1"}};
    if (!e.Data().empty()) {
      response["error"] = {{"code", e.Code()}, {"message", e.Message()}, {"data", e.Data()}};
    } else {
      response["error"] = {{"code", e.Code()}, {"message", e.Message()}};
    }
    raw_response = response.dump();
  }

  void VerifyMethodRequest(version version, const string &name, json id) {
    CHECK(request["method"] == name);
    CHECK(request["id"] == id);
    if (version == version::v2) {
      CHECK(request["jsonrpc"] == "2.0");
    } else {
      CHECK(!has_key(request, "jsonrpc"));
      CHECK(has_key(request, "params"));
    }
  }

  void VerifyNotificationRequest(version version, const string &name) {
    CHECK(request["method"] == name);
    if (version == version::v2) {
      CHECK(request["jsonrpc"] == "2.0");
      CHECK(!has_key(request, "id"));
    } else {
      CHECK(!has_key(request, "jsonrpc"));
      CHECK(request["id"].is_null());
      CHECK(has_key(request, "params"));
    }
  }
};
