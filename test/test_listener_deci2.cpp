#include "gtest/gtest.h"
#include "goalc/listener/Listener.h"
#include "game/system/Deci2Server.h"

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
  Deci2Server s(always_false);
}

TEST(Listener, DeciInit) {
  Deci2Server s(always_false);
  EXPECT_TRUE(s.init());
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
  Deci2Server s(always_false);
  EXPECT_TRUE(s.init());
  EXPECT_FALSE(s.check_for_listener());
  EXPECT_FALSE(s.check_for_listener());
  EXPECT_FALSE(s.check_for_listener());
}

TEST(Listener, CheckConnectionStaysAlive) {
  Deci2Server s(always_false);
  EXPECT_TRUE(s.init());
  EXPECT_FALSE(s.check_for_listener());
  Listener l;
  EXPECT_FALSE(s.check_for_listener());
  bool connected = l.connect_to_target();
  EXPECT_TRUE(connected);
  // TODO - some sort of backoff and retry would be better
  while (connected && !s.check_for_listener()) {
  }

  EXPECT_TRUE(s.check_for_listener());
  std::this_thread::sleep_for(std::chrono::milliseconds(500));
  EXPECT_TRUE(s.check_for_listener());
  EXPECT_TRUE(l.is_connected());
}

TEST(Listener, DeciThenListener) {
  for (int i = 0; i < 3; i++) {
    Deci2Server s(always_false);
    EXPECT_TRUE(s.init());
    EXPECT_FALSE(s.check_for_listener());
    EXPECT_FALSE(s.check_for_listener());
    EXPECT_FALSE(s.check_for_listener());
    Listener l;
    EXPECT_FALSE(s.check_for_listener());
    EXPECT_FALSE(s.check_for_listener());
    bool connected = l.connect_to_target();
    EXPECT_TRUE(connected);
    // TODO - some sort of backoff and retry would be better
    while (connected && !s.check_for_listener()) {
    }

    EXPECT_TRUE(s.check_for_listener());
  }
}

TEST(Listener, DeciThenListener2) {
  for (int i = 0; i < 3; i++) {
    Deci2Server s(always_false);
    EXPECT_TRUE(s.init());
    EXPECT_FALSE(s.check_for_listener());
    EXPECT_FALSE(s.check_for_listener());
    EXPECT_FALSE(s.check_for_listener());
    Listener l;
    EXPECT_FALSE(s.check_for_listener());
    EXPECT_FALSE(s.check_for_listener());
    EXPECT_TRUE(l.connect_to_target());
  }
}

TEST(Listener, ListenerThenDeci) {
  for (int i = 0; i < 3; i++) {
    Listener l;
    EXPECT_FALSE(l.connect_to_target());
    Deci2Server s(always_false);
    EXPECT_TRUE(s.init());
    EXPECT_FALSE(s.check_for_listener());
    bool connected = l.connect_to_target();
    EXPECT_TRUE(connected);
    // TODO - some sort of backoff and retry would be better
    while (connected && !s.check_for_listener()) {
    }
  }
}
