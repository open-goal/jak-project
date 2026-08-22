#include "game/system/IOP_Kernel.h"
#include "gtest/gtest.h"

TEST(IOPKernel, SystemTimeLowWraps) {
  EXPECT_EQ(iop::detail::system_time_low_from_microseconds(1000), 36864u);

  const auto before_wrap = iop::detail::system_time_low_from_microseconds(116508444);
  const auto after_wrap = iop::detail::system_time_low_from_microseconds(116508445);
  EXPECT_EQ(before_wrap, 0xffffffefu);
  EXPECT_EQ(after_wrap, 20u);
  EXPECT_EQ(after_wrap - before_wrap, 37u);
}
