#include "game/system/Deci2Server.h"
#include "goalc/listener/Listener.h"
#include "gtest/gtest.h"

using namespace listener;

namespace {
bool always_false() {
  return false;
}
}  // namespace

TEST(Listener, ListenerCreation) {
  listener::Listener l;
}

TEST(Listener, DeciCreation) {
  Deci2Server s(always_false, DECI2_PORT);
}

TEST(Listener, DeciInit) {
  Deci2Server s(always_false, DECI2_PORT);
  EXPECT_TRUE(s.init_server());
}

/*!
 * Try to connect when no Deci2Server is running
 */
TEST(Listener, ListenToNothing) {
  Listener l;
  if (l.connect_to_target()) {
    printf(
        "~~~~~~ Test connected to a runtime when there shouldn't be anything running! Check that "
        "you don't have gk running in the background!\n");
  }
  EXPECT_FALSE(l.connect_to_target());
  l.disconnect();
}

TEST(Listener, DeciCheckNoListener) {
  Deci2Server s(always_false, DECI2_PORT);
  EXPECT_TRUE(s.init_server());
  EXPECT_FALSE(s.is_client_connected());
  EXPECT_FALSE(s.is_client_connected());
  EXPECT_FALSE(s.is_client_connected());
}

TEST(Listener, CheckConnectionStaysAlive) {
  Deci2Server s(always_false, DECI2_PORT);
  EXPECT_TRUE(s.init_server());
  EXPECT_FALSE(s.is_client_connected());
  Listener l;
  EXPECT_FALSE(s.is_client_connected());
  bool connected = l.connect_to_target();
  EXPECT_TRUE(connected);
  // TODO - some sort of backoff and retry would be better
  while (connected && !s.is_client_connected()) {
  }

  EXPECT_TRUE(s.is_client_connected());
  std::this_thread::sleep_for(std::chrono::milliseconds(500));
  EXPECT_TRUE(s.is_client_connected());
  EXPECT_TRUE(l.is_connected());
}

TEST(Listener, DeciThenListener) {
  for (int i = 0; i < 3; i++) {
    Deci2Server s(always_false, DECI2_PORT);
    EXPECT_TRUE(s.init_server());
    EXPECT_FALSE(s.is_client_connected());
    EXPECT_FALSE(s.is_client_connected());
    EXPECT_FALSE(s.is_client_connected());
    Listener l;
    EXPECT_FALSE(s.is_client_connected());
    EXPECT_FALSE(s.is_client_connected());
    bool connected = l.connect_to_target();
    EXPECT_TRUE(connected);
    // TODO - some sort of backoff and retry would be better
    while (connected && !s.is_client_connected()) {
    }

    EXPECT_TRUE(s.is_client_connected());
  }
}

TEST(Listener, DeciThenListener2) {
  for (int i = 0; i < 3; i++) {
    Deci2Server s(always_false, DECI2_PORT);
    EXPECT_TRUE(s.init_server());
    EXPECT_FALSE(s.is_client_connected());
    EXPECT_FALSE(s.is_client_connected());
    EXPECT_FALSE(s.is_client_connected());
    Listener l;
    EXPECT_FALSE(s.is_client_connected());
    EXPECT_FALSE(s.is_client_connected());
    EXPECT_TRUE(l.connect_to_target());
  }
}

TEST(Listener, ListenerThenDeci) {
  for (int i = 0; i < 3; i++) {
    Listener l;
    EXPECT_FALSE(l.connect_to_target());
    Deci2Server s(always_false, DECI2_PORT);
    EXPECT_TRUE(s.init_server());
    EXPECT_FALSE(s.is_client_connected());
    bool connected = l.connect_to_target();
    EXPECT_TRUE(connected);
    // TODO - some sort of backoff and retry would be better
    while (connected && !s.is_client_connected()) {
    }
  }
}
