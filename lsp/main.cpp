#include <chrono>
#include <iostream>
#include <thread>
#include <vector>

#include "lsp_server.h"
#include "transport.h"

#include <jsonrpccxx/client.hpp>
#include <jsonrpccxx/server.hpp>

using namespace std;

void setup_logging() {
  lg::set_file(
      "C:\\Users\\xtvas\\Repositories\\opengoal\\jak-project\\decompiler\\config\\lsp.log");
  lg::set_file_level(lg::level::info);
  lg::set_stdout_level(lg::level::info);
  lg::set_flush_level(lg::level::info);
  lg::initialize();
}

/*
What needs to be understood is that for connection timing issues the server is actually a client and
the client is the server in terms of opening the ports.

When you specify a socket transport the client is listening on that port for a connection. The
socket port number is passed as --socket=${port} to the server process started.
*/

int main() {
  // TODO - the cpphttplib is basically a way better cross_socket implementation -- would be nice to
  // replace ours with it
  setup_logging();
  jsonrpccxx::JsonRpc2Server rpcServer;

  LspServer lsp;
  rpcServer.Add("initialize", jsonrpccxx::GetHandle(&LspServer::TestServer, lsp), {"id"});

  HttpServerConnector httpServer(rpcServer, 8484);
  HttpClientConnector httpClient("127.0.0.1", 8484);
  auto ye = httpClient.connect_to_socket();

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
