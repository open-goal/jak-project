
#include "../examples/warehouse/warehouseapp.hpp"
#include "doctest/doctest.h"
#include "integrationtest.hpp"

TEST_CASE_FIXTURE(IntegrationTest, "warehouse_test") {
  WarehouseServer app;
  rpcServer.Add("GetProduct", GetHandle(&WarehouseServer::GetProduct, app));
  rpcServer.Add("AddProduct", GetHandle(&WarehouseServer::AddProduct, app));

  Product p;
  p.id = "0xff";
  p.price = 22.4;
  p.name = "Product 1";
  p.cat = category::cash_carry;
  CHECK(client.CallMethod<bool>(1, "AddProduct", {p}));

  Product p2 = client.CallMethod<Product>(1, "GetProduct", {"0xff"});
  CHECK((p2.id == p.id && p2.name == p.name && p2.price == p.price && p2.cat == p.cat));
}
