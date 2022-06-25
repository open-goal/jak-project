#include "doctest/doctest.h"
#include <jsonrpccxx/common.hpp>

using namespace std;
using namespace jsonrpccxx;

TEST_CASE("exception error type") {
  CHECK(JsonRpcException(-32700, "").Type() == parse_error);
  CHECK(JsonRpcException(-32600, "").Type() == invalid_request);
  CHECK(JsonRpcException(-32601, "").Type() == method_not_found);
  CHECK(JsonRpcException(-32602, "").Type() == invalid_params);
  CHECK(JsonRpcException(-32603, "").Type() == internal_error);

  for(int c = -32000; c >= -32099; c--)
    CHECK(JsonRpcException(c, "").Type() == server_error);

  CHECK(JsonRpcException(0, "").Type() == invalid);
  CHECK(JsonRpcException(32700, "").Type() == invalid);
  CHECK(JsonRpcException(33000, "").Type() == invalid);
}