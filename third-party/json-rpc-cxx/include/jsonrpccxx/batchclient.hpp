#pragma once

#include "client.hpp"

namespace jsonrpccxx {
  class BatchRequest {
  public:
    BatchRequest() : call(json::array()) {}
    BatchRequest &AddMethodCall(const id_type &id, const std::string &name, const positional_parameter &params = {}) {
      json request = {{"method", name}, {"params", params}, {"jsonrpc", "2.0"}};
      if (std::get_if<int>(&id) != nullptr) {
        request["id"] = std::get<int>(id);
      } else {
        request["id"] = std::get<std::string>(id);
      }
      call.push_back(request);
      return *this;
    }

    BatchRequest &AddNamedMethodCall(const id_type &id, const std::string &name, const named_parameter &params = {}) {
      json request = {{"method", name}, {"params", params}, {"jsonrpc", "2.0"}};
      if (std::get_if<int>(&id) != nullptr) {
        request["id"] = std::get<int>(id);
      } else {
        request["id"] = std::get<std::string>(id);
      }
      call.push_back(request);
      return *this;
    }

    BatchRequest &AddNotificationCall(const std::string &name, const positional_parameter &params = {}) {
      call.push_back({{"method", name}, {"params", params}, {"jsonrpc", "2.0"}});
      return *this;
    }

    BatchRequest &AddNamedNotificationCall(const std::string &name, const named_parameter &params = {}) {
      call.push_back({{"method", name}, {"params", params}, {"jsonrpc", "2.0"}});
      return *this;
    }

    const json &Build() const { return call; }

  private:
    json call;
  };

  class BatchResponse {
  public:
    explicit BatchResponse(json &&response) : response(response), results(), errors(), nullIds() {
      for (auto &[key, value] : response.items()) {
        if (value.is_object() && valid_id_not_null(value) && has_key(value, "result")) {
          results[value["id"]] = std::stoi(key);
        } else if (value.is_object() && valid_id_not_null(value) && has_key(value, "error")) {
          errors[value["id"]] = std::stoi(key);
        } else {
          nullIds.push_back(std::stoi(key));
        }
      }
    }

    template <typename T>
    T Get(const json &id) {
      if (results.find(id) != results.end()) {
        try {
          return response[results[id]]["result"].get<T>();
        } catch (json::type_error &e) {
          throw JsonRpcException(parse_error, "invalid return type: " + std::string(e.what()));
        }
      } else if (errors.find(id) != errors.end()) {
        throw JsonRpcException::fromJson(response[errors[id]]["error"]);
      }
      throw JsonRpcException(parse_error, std::string("no result found for id ") + id.dump());
    }

    bool HasErrors() { return !errors.empty() || !nullIds.empty(); }
    const std::vector<size_t> GetInvalidIndexes() { return nullIds; }
    const json& GetResponse() { return response; }

  private:
    json response;
    std::map<json, size_t> results;
    std::map<json, size_t> errors;
    std::vector<size_t> nullIds;
  };

  class BatchClient : public JsonRpcClient {
  public:
    explicit BatchClient(IClientConnector &connector) : JsonRpcClient(connector, version::v2) {}
    BatchResponse BatchCall(const BatchRequest &request) {
      try {
        json response = json::parse(connector.Send(request.Build().dump()));
        if (!response.is_array()) {
          throw JsonRpcException(parse_error, std::string("invalid JSON response from server: expected array"));
        }
        return BatchResponse(std::move(response));
      } catch (json::parse_error &e) {
        throw JsonRpcException(parse_error, std::string("invalid JSON response from server: ") + e.what());
      }
    }
  };
}
