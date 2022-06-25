#pragma once

#include "../examples/inmemoryconnector.hpp"
#include <jsonrpccxx/client.hpp>
#include <jsonrpccxx/server.hpp>

using namespace jsonrpccxx;

struct IntegrationTest {
  IntegrationTest() : rpcServer(), connector(rpcServer), client(connector, version::v2) {}
  JsonRpc2Server rpcServer;
  InMemoryConnector connector;
  JsonRpcClient client;
};
