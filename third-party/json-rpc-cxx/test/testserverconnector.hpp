#pragma once
#include "doctest/doctest.h"
#include <jsonrpccxx/server.hpp>

using namespace jsonrpccxx;
using namespace std;

class TestServerConnector {
public:
  explicit TestServerConnector(JsonRpcServer &handler) : handler(handler), raw_response() {}

    void SendRawRequest(const string &request) { this->raw_response = handler.HandleRequest(request); }
    void SendRequest(const json &request) { SendRawRequest(request.dump()); }
    static json BuildMethodCall(const json &id, const string &name, const json &params) { return {{"id", id}, {"method", name}, {"params", params}, {"jsonrpc", "2.0"}}; }
    void CallMethod(const json &id, const string &name, const json &params) { SendRequest(BuildMethodCall(id, name, params)); }
    static json BuildNotificationCall(const string &name, const json &params) { return {{"method", name}, {"params", params}, {"jsonrpc", "2.0"}}; }
    void CallNotification(const string &name, const json &params) { SendRequest(BuildNotificationCall(name, params)); }

    json VerifyMethodResult(const json &id) {
        json result = json::parse(this->raw_response);
        return VerifyMethodResult(id, result);
    }

    static json VerifyMethodResult(const json &id, json &result) {
        REQUIRE(!has_key(result, "error"));
        REQUIRE(result["jsonrpc"] == "2.0");
        REQUIRE(result["id"] == id);
        REQUIRE(has_key(result, "result"));
        return result["result"];
    }

    json VerifyBatchResponse() {
        json result = json::parse(raw_response);
        REQUIRE(result.is_array());
        return result;
    }

    void VerifyNotificationResult() { VerifyNotificationResult(this->raw_response); }

    static void VerifyNotificationResult(string &raw_response) { REQUIRE(raw_response.empty()); }

    json VerifyMethodError(int code, const string &message, const json &id) {
        json error = json::parse(this->raw_response);
        return VerifyMethodError(code, message, id, error);
    }

    static json VerifyMethodError(int code, const string &message, const json &id, json &result) {
        REQUIRE(!has_key(result, "result"));
        REQUIRE(result["jsonrpc"] == "2.0");
        REQUIRE(result["id"] == id);
        REQUIRE(has_key_type(result, "error", json::value_t::object));
        REQUIRE(has_key_type(result["error"], "code", json::value_t::number_integer));
        REQUIRE(result["error"]["code"] == code);
        REQUIRE(has_key_type(result["error"], "message", json::value_t::string));
        REQUIRE(result["error"]["message"].get<std::string>().find(message) != std::string::npos);

        return result["error"];
    }

private:
    JsonRpcServer &handler;
    string raw_response;
};
