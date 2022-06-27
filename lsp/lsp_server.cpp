#include "lsp_server.h"

#include <jsonrpccxx/common.hpp>
#include <decompiler/util/DecompilerTypeSystem.h>

#include "third-party/fmt/core.h"
#include <common/log/log.h>

using namespace jsonrpccxx;

LspServer::LspServer() {
  // Decompiling
  // Read in all-types files
  // TODO - hard-coded for now, but need to enumerate the directory eventually i think?
  m_dts.parse_type_defs({"C:\\Users\\xtvas\\Repositories\\opengoal\\jak-project\\decompiler\\config\\all-types.gc"});

  auto info = m_dts.symbol_definition_info["vector"];
  lg::info("Loaded DTS");
}

bool LspServer::TestServer(const std::string& id) {
  return true;
}

/*
bool WarehouseServer::AddProduct(const Product& p) {
  if (products.find(p.id) != products.end())
    return false;
  products[p.id] = p;
  return true;
}

const Product& WarehouseServer::GetProduct(const std::string& id) {
  if (products.find(id) == products.end())
    throw JsonRpcException(-33000, "No product listed for id: " + id);
  return products[id];
}
std::vector<Product> WarehouseServer::AllProducts() {
  std::vector<Product> result;
  for (const auto& p : products)
    result.push_back(p.second);
  return result;
}
*/
