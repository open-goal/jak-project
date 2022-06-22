#pragma once

#include "common/math/Vector.h"

namespace math {

template <typename T>
T squared(const T& in) {
  return in * in;
}

template <typename T>
struct RaySphereResult {
  bool hit = false;
  T u[2] = {0, 0};
};

/*!
 * Check if a line intersects a sphere.
 */
template <typename T>
RaySphereResult<T> ray_sphere_intersect(const Vector3<T>& ray_origin,
                                        const Vector3<T>& ray_direction_in,
                                        const Vector3<T>& sphere_origin,
                                        T sphere_radius) {
  RaySphereResult<T> result;
  Vector3<T> ray_direction = ray_direction_in.normalized();

  Vector3<T> oc = ray_origin - sphere_origin;

  T value_under_sqrt =
      squared(ray_direction.dot(oc)) - (oc.squared_length() - squared(sphere_radius));

  if (value_under_sqrt < 0) {
    result.hit = false;
    return result;
  }

  T minus_b = -ray_direction.dot(oc);
  result.hit = true;
  T sqrt_val = std::sqrt(value_under_sqrt);

  result.u[0] = minus_b + sqrt_val;
  result.u[1] = minus_b - sqrt_val;
  return result;
}

math::Vector4f bsphere_of_triangle(const Vector3f* vertices);

inline bool point_in_bsphere(const Vector4f& sphere, const Vector3f& pt) {
  return (sphere.xyz() - pt).squared_length() <= (sphere.w() * sphere.w());
}
}  // namespace math