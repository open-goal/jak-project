#include "common/math/geometry.h"

#include "gtest/gtest.h"

TEST(Math, RaySphereOrigin) {
  math::Vector<float, 3> sphere(0, 0, 0);
  math::Vector<float, 3> line_origin(0, 0, 0);
  math::Vector<float, 3> line_dir(1, 2, 3);  // shouldn't matter
  auto result = math::ray_sphere_intersect(line_origin, line_dir, sphere, 1.f);
  EXPECT_FLOAT_EQ(result.u[0], 1.f);
  EXPECT_FLOAT_EQ(result.u[1], -1.f);
}

TEST(Math, RaySphereOriginOffsetXProbeX) {
  math::Vector<float, 3> sphere(0, 0, 0);
  math::Vector<float, 3> line_origin(0.5, 0, 0);
  math::Vector<float, 3> line_dir(1, 0, 0);
  auto result = math::ray_sphere_intersect(line_origin, line_dir, sphere, 1.f);
  EXPECT_FLOAT_EQ(result.u[0], 0.5f);
  EXPECT_FLOAT_EQ(result.u[1], -1.5f);
}

TEST(Math, RaySphereOriginOffsetXProbeY) {
  math::Vector<float, 3> sphere(0, 0, 0);
  math::Vector<float, 3> line_origin(0.5, 0, 0);
  math::Vector<float, 3> line_dir(0, 1, 0);
  auto result = math::ray_sphere_intersect(line_origin, line_dir, sphere, 1.f);
  // should be +/- sqrt(3)/2, draw a triangle.
  EXPECT_FLOAT_EQ(result.u[0], std::sqrt(3.f) / 2.f);
  EXPECT_FLOAT_EQ(result.u[1], -std::sqrt(3.f) / 2.f);
}

TEST(Math, RaySphereMakeTestCaseHit) {
  math::Vector<float, 3> sphere(0.4, 0.3, 0.7);
  math::Vector<float, 3> line_origin(0.2, -3, 0);
  math::Vector<float, 3> line_dir(0, 1, 0);
  auto result = math::ray_sphere_intersect(line_origin, line_dir, sphere, 1.2f);
  fmt::print("Test case: {} {} {}\n", result.hit, result.u[0], result.u[1]);
}

TEST(Math, RaySphereMiss) {
  math::Vector<float, 3> sphere(1, 2, 3);
  fmt::print("{}\n", sphere.to_string_aligned());
}
