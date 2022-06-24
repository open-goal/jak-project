#pragma once

#include <cpp-httplib/httplib.h>
#include <jsonrpccxx/iclientconnector.hpp>
#include <jsonrpccxx/server.hpp>
#include <string>

class CppHttpLibClientConnector : public jsonrpccxx::IClientConnector {
public:
  explicit CppHttpLibClientConnector(const std::string &host, int port) : httpClient(host.c_str(), port) {}
  std::string Send(const std::string &request) override {
    auto res = httpClient.Post("/jsonrpc", request, "application/json");
    if (!res || res->status != 200) {
      throw jsonrpccxx::JsonRpcException(-32003, "client connector error, received status != 200");
    }
    return res->body;
  }

private:
  httplib::Client httpClient;
};

class CppHttpLibServerConnector {
public:
  explicit CppHttpLibServerConnector(jsonrpccxx::JsonRpcServer &server, int port) :
    thread(),
    server(server),
    httpServer(),
    port(port) {
    httpServer.Post("/jsonrpc",
		    [this](const httplib::Request &req, httplib::Response &res) {
		      this->PostAction(req, res);
		    });
  }

  virtual ~CppHttpLibServerConnector() { StopListening(); }

  bool StartListening() {
    if (httpServer.is_running())
      return false;
    this->thread = std::thread([this]() { this->httpServer.listen("localhost", port); });
    return true;
  }

  void StopListening() {
    if (httpServer.is_running()) {
      httpServer.stop();
      this->thread.join();
    }
  }

private:
  std::thread thread;
  jsonrpccxx::JsonRpcServer &server;
  httplib::Server httpServer;
  int port;

  void PostAction(const httplib::Request &req,
		  httplib::Response &res) {
    res.status = 200;
    res.set_content(this->server.HandleRequest(req.body), "application/json");
  }
};
