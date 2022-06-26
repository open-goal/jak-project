#include <iostream>
#include <vector>

#include "lsp_server.h"
#include "transport.h"
#include <chrono>
#include <thread>

#include <jsonrpccxx/client.hpp>
#include <jsonrpccxx/server.hpp>

using namespace std;

int main() {
  jsonrpccxx::JsonRpc2Server rpcServer;

  // Bindings
  LspServer lsp;
  rpcServer.Add("TestServer", jsonrpccxx::GetHandle(&LspServer::TestServer, lsp), {"id"});

  HttpServerConnector httpServer(rpcServer, 8484);

  httpServer.start_listening();

  return 0;
}

/*
void doWarehouseStuff(IClientConnector &clientConnector) {
  JsonRpcClient client(clientConnector, version::v2);
  WareHouseClient appClient(client);
  Product p;
  p.id = "0xff";
  p.price = 22.4;
  p.name = "Product 1";
  p.cat = category::cash_carry;
  cout << "Adding product: " << std::boolalpha << appClient.AddProduct(p) << "\n";

  Product p2 = appClient.GetProduct("0xff");
  cout << "Found product: " << p2.name << "\n";
  try {
    appClient.GetProduct("0xff2");
  } catch (JsonRpcException &e) {
    cerr << "Error finding product: " << e.what() << "\n";
  }

  auto all = appClient.AllProducts();
  for (const auto &p: all) {
    cout << p.name << endl;
  }
}
*/
