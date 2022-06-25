#pragma once
#include <jsonrpccxx/iclientconnector.hpp>
#include <jsonrpccxx/server.hpp>

//This class is server and client connector at the same time.
class InMemoryConnector : public jsonrpccxx::IClientConnector {
public:
  explicit InMemoryConnector(jsonrpccxx::JsonRpcServer &server) : server(server) {}
  std::string Send(const std::string &request) override { return server.HandleRequest(request); }
private:
  jsonrpccxx::JsonRpcServer &server;
};