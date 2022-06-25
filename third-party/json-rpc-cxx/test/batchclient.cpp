#include "doctest/doctest.h"
#include "testclientconnector.hpp"
#include <iostream>
#include <jsonrpccxx/batchclient.hpp>

using namespace std;
using namespace jsonrpccxx;

TEST_CASE("batchresponse") {
  BatchResponse br({{{"jsonrpc", "2.0"}, {"id", "1"}, {"result", "someresultstring"}},
                    {{"jsonrpc", "2.0"}, {"id", "2"}, {"result", 33}},
                    {{"jsonrpc", "2.0"}, {"id", "3"}, {"error", {{"code", -111}, {"message", "the error message"}}}},
                    {{"jsonrpc", "2.0"}, {"id", nullptr}, {"error", {{"code", -112}, {"message", "the error message"}}}},
                    3});

  CHECK(br.HasErrors());
  CHECK(br.Get<string>("1") == "someresultstring");
  REQUIRE_THROWS_WITH(br.Get<string>(1), "-32700: no result found for id 1");
  CHECK(br.Get<int>("2") == 33);
  CHECK(br.Get<int>("2") == 33);
  REQUIRE_THROWS_WITH(br.Get<int>("1"), "-32700: invalid return type: [json.exception.type_error.302] type must be number, but is string");
  REQUIRE_THROWS_WITH(br.Get<string>("3"), "-111: the error message");
  REQUIRE_THROWS_WITH(br.Get<string>(nullptr), "-32700: no result found for id null");

  CHECK(br.GetInvalidIndexes().size() == 2);
  CHECK(br.GetResponse().size() == 5);
  CHECK(br.GetResponse()[br.GetInvalidIndexes()[0]]["error"]["code"] == -112);
  CHECK(br.GetResponse()[br.GetInvalidIndexes()[1]] == 3);
}

TEST_CASE("batchrequest") {
  BatchRequest br;
  TestClientConnector c;
  json request = br.AddMethodCall(1, "some_method1", {"value1"})
                     .AddMethodCall("1", "some_method1", {"value1"})
                     .AddNamedMethodCall(2, "some_method2", {{"param1", "value1"}})
                     .AddNamedMethodCall("2", "some_method2", {{"param1", "value1"}})
                     .AddNotificationCall("some_notification1", {"value2"})
                     .AddNamedNotificationCall("some_notification2", {{"param2", "value2"}})
                     .Build();

  CHECK(request.is_array());
  CHECK(request.size() == 6);
  c.Send(request[0].dump());
  c.VerifyMethodRequest(version::v2, "some_method1", 1);
  c.Send(request[1].dump());
  c.VerifyMethodRequest(version::v2, "some_method1", "1");
  c.Send(request[2].dump());
  c.VerifyMethodRequest(version::v2, "some_method2", 2);
  c.Send(request[3].dump());
  c.VerifyMethodRequest(version::v2, "some_method2", "2");
  c.Send(request[4].dump());
  c.VerifyNotificationRequest(version::v2, "some_notification1");
  c.Send(request[5].dump());
  c.VerifyNotificationRequest(version::v2, "some_notification2");
}

TEST_CASE("batchclient") {
  TestClientConnector c;
  BatchClient client(c);
  c.SetBatchResult({TestClientConnector::BuildResult("result1", 1), TestClientConnector::BuildResult(33, 2)});

  BatchRequest r;
  r.AddMethodCall(1, "some_method", {"value1"});
  r.AddMethodCall(2, "some_method", {"value2"});
  BatchResponse response = client.BatchCall(r);
  CHECK(response.Get<string>(1) == "result1");
  CHECK(response.Get<int>(2) == 33);

  c.SetBatchResult("{}");
  CHECK_THROWS_WITH(client.BatchCall(r), "-32700: invalid JSON response from server: expected array");
  c.raw_response = "somestring";
  CHECK_THROWS_WITH(client.BatchCall(r), "-32700: invalid JSON response from server: [json.exception.parse_error.101] parse error at line 1, column 1: syntax error while parsing value - invalid literal; last read: 's'");
}