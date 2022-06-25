#pragma once

#include "common.hpp"
#include "dispatcher.hpp"
#include <string>

namespace jsonrpccxx {
  class JsonRpcServer {
  public:
    JsonRpcServer() : dispatcher() {}
    virtual ~JsonRpcServer() = default;
    virtual std::string HandleRequest(const std::string &request) = 0;

    bool Add(const std::string &name, MethodHandle callback, const NamedParamMapping &mapping = NAMED_PARAM_MAPPING) {
      if (name.rfind("rpc.", 0) == 0)
        return false;
      return dispatcher.Add(name, callback, mapping);
    }
    bool Add(const std::string &name, NotificationHandle callback, const NamedParamMapping &mapping = NAMED_PARAM_MAPPING) {
      if (name.rfind("rpc.", 0) == 0)
        return false;
      return dispatcher.Add(name, callback, mapping);
    }

  protected:
    Dispatcher dispatcher;
  };

  class JsonRpc2Server : public JsonRpcServer {
  public:
    JsonRpc2Server() = default;
    ~JsonRpc2Server() override = default;

    std::string HandleRequest(const std::string &requestString) override {
      try {
        json request = json::parse(requestString);
        if (request.is_array()) {
          json result = json::array();
          for (json &r : request) {
            json res = this->HandleSingleRequest(r);
            if (!res.is_null()) {
              result.push_back(std::move(res));
            }
          }
          return result.dump();
        } else if (request.is_object()) {
          json res = HandleSingleRequest(request);
          if (!res.is_null()) {
            return res.dump();
          } else {
            return "";
          }
        } else {
          return json{{"id", nullptr}, {"error", {{"code", invalid_request}, {"message", "invalid request: expected array or object"}}}, {"jsonrpc", "2.0"}}.dump();
        }
      } catch (json::parse_error &e) {
        return json{{"id", nullptr}, {"error", {{"code", parse_error}, {"message", std::string("parse error: ") + e.what()}}}, {"jsonrpc", "2.0"}}.dump();
      }
    }

  private:
    json HandleSingleRequest(json &request) {
      json id = nullptr;
      if (valid_id(request)) {
        id = request["id"];
      }
      try {
        return ProcessSingleRequest(request);
      } catch (JsonRpcException &e) {
        json error = {{"code", e.Code()}, {"message", e.Message()}};
        if (!e.Data().is_null()) {
          error["data"] = e.Data();
        }
        return json{{"id", id}, {"error", error}, {"jsonrpc", "2.0"}};
      } catch (std::exception &e) {
        return json{{"id", id}, {"error", {{"code", internal_error}, {"message", std::string("internal server error: ") + e.what()}}}, {"jsonrpc", "2.0"}};
      } catch (...) {
        return json{{"id", id}, {"error", {{"code", internal_error}, {"message", std::string("internal server error")}}}, {"jsonrpc", "2.0"}};
      }
    }

    json ProcessSingleRequest(json &request) {
      if (!has_key_type(request, "jsonrpc", json::value_t::string) || request["jsonrpc"] != "2.0") {
        throw JsonRpcException(invalid_request, R"(invalid request: missing jsonrpc field set to "2.0")");
      }
      if (!has_key_type(request, "method", json::value_t::string)) {
        throw JsonRpcException(invalid_request, "invalid request: method field must be a string");
      }
      if (has_key(request, "id") && !valid_id(request)) {
        throw JsonRpcException(invalid_request, "invalid request: id field must be a number, string or null");
      }
      if (has_key(request, "params") && !(request["params"].is_array() || request["params"].is_object() || request["params"].is_null())) {
        throw JsonRpcException(invalid_request, "invalid request: params field must be an array, object or null");
      }
      if (!has_key(request, "params") || has_key_type(request, "params", json::value_t::null)) {
        request["params"] = json::array();
      }
      if (!has_key(request, "id")) {
        try {
          dispatcher.InvokeNotification(request["method"], request["params"]);
          return json();
        } catch (std::exception &) {
          return json();
        }
      } else {
        return {{"jsonrpc", "2.0"}, {"id", request["id"]}, {"result", dispatcher.InvokeMethod(request["method"], request["params"])}};
      }
    }
  };
}
