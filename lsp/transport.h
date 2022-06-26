#pragma once

#include <string>

#include <cpp-httplib/httplib.h>
#include <jsonrpccxx/iclientconnector.hpp>
#include <jsonrpccxx/server.hpp>

class HttpServerConnector {
 public:
  explicit HttpServerConnector(jsonrpccxx::JsonRpcServer& server, int port)
      : server(server), httpServer(), port(port) {
    httpServer.Post("/", [this](const httplib::Request& req, httplib::Response& res) {
      this->PostAction(req, res);
    });
  }

  virtual ~HttpServerConnector() { stop_listening(); }

  bool start_listening() {
    if (httpServer.is_running())
      return false;
    this->httpServer.listen("localhost", port);
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
    res.status = 200;
    res.set_content(this->server.HandleRequest(req.body), "application/json");
  }
};
