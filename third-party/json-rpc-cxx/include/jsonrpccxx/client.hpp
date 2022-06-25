#pragma once
#include "common.hpp"
#include "iclientconnector.hpp"
#include <exception>
#include <nlohmann/json.hpp>
#include <string>
#include <variant>

namespace jsonrpccxx {
  enum class version { v1, v2 };
  typedef std::vector<json> positional_parameter;
  typedef std::map<std::string, json> named_parameter;
  typedef std::variant<int, std::string> id_type;

  struct JsonRpcResponse {
    id_type id;
    json result;
  };

  class JsonRpcClient {
  public:
    JsonRpcClient(IClientConnector &connector, version v) : connector(connector), v(v) {}
    virtual ~JsonRpcClient() = default;

    template <typename T>
    T CallMethod(const id_type &id, const std::string &name) { return call_method(id, name, json::object()).result.get<T>(); }
     template <typename T>
    T CallMethod(const id_type &id, const std::string &name, const positional_parameter &params) { return call_method(id, name, params).result.get<T>(); }
    template <typename T>
    T CallMethodNamed(const id_type &id, const std::string &name, const named_parameter &params = {}) { return call_method(id, name, params).result.get<T>(); }

    void CallNotification(const std::string &name, const positional_parameter &params = {}) { call_notification(name, params); }
    void CallNotificationNamed(const std::string &name, const named_parameter &params = {}) { call_notification(name, params); }

  protected:
    IClientConnector &connector;

  private:
    version v;

    JsonRpcResponse call_method(const id_type &id, const std::string &name, const json &params) {
      json j = {{"method", name}};
      if (std::get_if<int>(&id) != nullptr) {
        j["id"] = std::get<int>(id);
      } else {
        j["id"] = std::get<std::string>(id);
      }
      if (v == version::v2) {
        j["jsonrpc"] = "2.0";
      }
      if (!params.empty() && !params.is_null()) {
        j["params"] = params;
      } else if (params.is_array()) {
        j["params"] = params;
      } else if (v == version::v1) {
        j["params"] = nullptr;
      }
      try {
        json response = json::parse(connector.Send(j.dump()));
        if (has_key_type(response, "error", json::value_t::object)) {
          throw JsonRpcException::fromJson(response["error"]);
        } else if (has_key_type(response, "error", json::value_t::string)) {
          throw JsonRpcException(internal_error, response["error"]);
        }
        if (has_key(response, "result") && has_key(response, "id")) {
          if (response["id"].type() == json::value_t::string)
            return JsonRpcResponse{response["id"].get<std::string>(), response["result"].get<json>()};
          else
            return JsonRpcResponse{response["id"].get<int>(), response["result"].get<json>()};
        }
        throw JsonRpcException(internal_error, R"(invalid server response: neither "result" nor "error" fields found)");
      } catch (json::parse_error &e) {
        throw JsonRpcException(parse_error, std::string("invalid JSON response from server: ") + e.what());
      }
    }

    void call_notification(const std::string &name, const nlohmann::json &params) {
      nlohmann::json j = {{"method", name}};
      if (v == version::v2) {
        j["jsonrpc"] = "2.0";
      } else {
        j["id"] = nullptr;
      }
      if (!params.empty() && !params.is_null()) {
        j["params"] = params;
      } else if (v == version::v1) {
        j["params"] = nullptr;
      }
      connector.Send(j.dump());
    }
  };
} // namespace jsonrpccxx
