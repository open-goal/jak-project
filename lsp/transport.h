#pragma once

#include <string>

#include <common/log/log.h>

#include <cpp-httplib/httplib.h>
#include <jsonrpccxx/iclientconnector.hpp>
#include <jsonrpccxx/server.hpp>
#include "third-party/fmt/core.h"

class HttpClientConnector {
 public:
  explicit HttpClientConnector(const std::string& host, int port)
      : httpClient(host.c_str(), port) {}

  bool connect_to_socket() {
    if (!httpClient.is_socket_open()) {
      auto res = httpClient.send({});
      return true;
    }
    return true;
  }

 private:
  httplib::Client httpClient;
};

class HttpServerConnector {
 public:
  explicit HttpServerConnector(jsonrpccxx::JsonRpcServer& server, int port)
      : server(server), httpServer(), port(port) {
    httpServer.Post(".*", [this](const httplib::Request& req, httplib::Response& res) {
      this->PostAction(req, res);
    });
  }

  virtual ~HttpServerConnector() { stop_listening(); }

  bool start_listening() {
    if (httpServer.is_running())
      return false;
    this->httpServer.listen("127.0.0.1", port);
    return true;
  }

  void stop_listening() {
    if (httpServer.is_running()) {
      httpServer.stop();
    }
  }

 private:
  jsonrpccxx::JsonRpcServer& server;
  httplib::Server httpServer;
  int port;

  void PostAction(const httplib::Request& req, httplib::Response& res) {
    // Log the body for debugging purposes (find out what i gotta implement)
    lg::info("[LSP] New Request: {}\n", req.body);
    printf("%s", fmt::format("[LSP] New Request: {}\n", req.body).c_str());
    fflush(stdout);
    res.status = 200;
    res.set_content(this->server.HandleRequest(req.body), "application/json");
  }
};
