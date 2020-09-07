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

// TEST(Listener, TwoDeciServers) {
//  Deci2Server s1, s2;
//  EXPECT_TRUE(s1.init());
//  EXPECT_TRUE(s2.init());
//}

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

TEST(Listener, DeciThenListener) {
  for (int i = 0; i < 10; i++) {
    Deci2Server s(always_false);
    EXPECT_TRUE(s.init());
    EXPECT_FALSE(s.check_for_listener());
    EXPECT_FALSE(s.check_for_listener());
    EXPECT_FALSE(s.check_for_listener());
    Listener l;
    EXPECT_FALSE(s.check_for_listener());
    EXPECT_FALSE(s.check_for_listener());
    EXPECT_TRUE(l.connect_to_target());
    // kind of a hack.
    while (!s.check_for_listener()) {
      // printf("...\n");
    }

    EXPECT_TRUE(s.check_for_listener());
  }
}

TEST(Listener, DeciThenListener2) {
  for (int i = 0; i < 10; i++) {
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
  for (int i = 0; i < 10; i++) {
    Listener l;
    EXPECT_FALSE(l.connect_to_target());
    Deci2Server s(always_false);
    EXPECT_TRUE(s.init());
    EXPECT_FALSE(s.check_for_listener());
    EXPECT_TRUE(l.connect_to_target());
    while (!s.check_for_listener()) {
      //      printf("...\n");
    }
  }
}

TEST(Listener, ListenerMultipleDecis) {
  Listener l;
  EXPECT_FALSE(l.connect_to_target());
  {
    Deci2Server s(always_false);
    EXPECT_TRUE(s.init());
    EXPECT_FALSE(s.check_for_listener());
    EXPECT_TRUE(l.connect_to_target());
    while (!s.check_for_listener()) {
      // printf("...\n");
    }
    l.disconnect();
  }

  {
    Deci2Server s(always_false);
    EXPECT_TRUE(s.init());
    EXPECT_FALSE(s.check_for_listener());
    EXPECT_TRUE(l.connect_to_target());
    while (!s.check_for_listener()) {
      // printf("...\n");
    }
    l.disconnect();
  }
}
