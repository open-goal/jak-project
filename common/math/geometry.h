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

template <typename T>
math::Matrix<T, 4, 4> affine_inverse(const math::Matrix<T, 4, 4>& in) {
  math::Matrix<T, 4, 4> result;

  // transpose rotation
  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 3; j++) {
      result(i, j) = in(j, i);
    }
  }

  result(3, 0) = 0;
  result(3, 1) = 0;
  result(3, 2) = 0;
  result(3, 3) = 1;
  result(0, 3) = 0;
  result(1, 3) = 0;
  result(2, 3) = 0;
  for (int rx = 0; rx < 3; rx++) {
    for (int cx = 0; cx < 3; cx++) {
      result(rx, 3) -= result(rx, cx) * in(cx, 3);
    }
  }

  return result;
}

template <typename T>
math::Matrix<T, 4, 4> inverse(const math::Matrix<T, 4, 4>& m) {
  math::Matrix<T, 4, 4> im;
  T A2323 = m(2, 2) * m(3, 3) - m(2, 3) * m(3, 2);
  T A1323 = m(2, 1) * m(3, 3) - m(2, 3) * m(3, 1);
  T A1223 = m(2, 1) * m(3, 2) - m(2, 2) * m(3, 1);
  T A0323 = m(2, 0) * m(3, 3) - m(2, 3) * m(3, 0);
  T A0223 = m(2, 0) * m(3, 2) - m(2, 2) * m(3, 0);
  T A0123 = m(2, 0) * m(3, 1) - m(2, 1) * m(3, 0);
  T A2313 = m(1, 2) * m(3, 3) - m(1, 3) * m(3, 2);
  T A1313 = m(1, 1) * m(3, 3) - m(1, 3) * m(3, 1);
  T A1213 = m(1, 1) * m(3, 2) - m(1, 2) * m(3, 1);
  T A2312 = m(1, 2) * m(2, 3) - m(1, 3) * m(2, 2);
  T A1312 = m(1, 1) * m(2, 3) - m(1, 3) * m(2, 1);
  T A1212 = m(1, 1) * m(2, 2) - m(1, 2) * m(2, 1);
  T A0313 = m(1, 0) * m(3, 3) - m(1, 3) * m(3, 0);
  T A0213 = m(1, 0) * m(3, 2) - m(1, 2) * m(3, 0);
  T A0312 = m(1, 0) * m(2, 3) - m(1, 3) * m(2, 0);
  T A0212 = m(1, 0) * m(2, 2) - m(1, 2) * m(2, 0);
  T A0113 = m(1, 0) * m(3, 1) - m(1, 1) * m(3, 0);
  T A0112 = m(1, 0) * m(2, 1) - m(1, 1) * m(2, 0);

  T det = m(0, 0) * (m(1, 1) * A2323 - m(1, 2) * A1323 + m(1, 3) * A1223) -
          m(0, 1) * (m(1, 0) * A2323 - m(1, 2) * A0323 + m(1, 3) * A0223) +
          m(0, 2) * (m(1, 0) * A1323 - m(1, 1) * A0323 + m(1, 3) * A0123) -
          m(0, 3) * (m(1, 0) * A1223 - m(1, 1) * A0223 + m(1, 2) * A0123);
  det = 1 / det;

  im(0, 0) = det * (m(1, 1) * A2323 - m(1, 2) * A1323 + m(1, 3) * A1223);
  im(0, 1) = det * -(m(0, 1) * A2323 - m(0, 2) * A1323 + m(0, 3) * A1223);
  im(0, 2) = det * (m(0, 1) * A2313 - m(0, 2) * A1313 + m(0, 3) * A1213);
  im(0, 3) = det * -(m(0, 1) * A2312 - m(0, 2) * A1312 + m(0, 3) * A1212);
  im(1, 0) = det * -(m(1, 0) * A2323 - m(1, 2) * A0323 + m(1, 3) * A0223);
  im(1, 1) = det * (m(0, 0) * A2323 - m(0, 2) * A0323 + m(0, 3) * A0223);
  im(1, 2) = det * -(m(0, 0) * A2313 - m(0, 2) * A0313 + m(0, 3) * A0213);
  im(1, 3) = det * (m(0, 0) * A2312 - m(0, 2) * A0312 + m(0, 3) * A0212);
  im(2, 0) = det * (m(1, 0) * A1323 - m(1, 1) * A0323 + m(1, 3) * A0123);
  im(2, 1) = det * -(m(0, 0) * A1323 - m(0, 1) * A0323 + m(0, 3) * A0123);
  im(2, 2) = det * (m(0, 0) * A1313 - m(0, 1) * A0313 + m(0, 3) * A0113);
  im(2, 3) = det * -(m(0, 0) * A1312 - m(0, 1) * A0312 + m(0, 3) * A0112);
  im(3, 0) = det * -(m(1, 0) * A1223 - m(1, 1) * A0223 + m(1, 2) * A0123);
  im(3, 1) = det * (m(0, 0) * A1223 - m(0, 1) * A0223 + m(0, 2) * A0123);
  im(3, 2) = det * -(m(0, 0) * A1213 - m(0, 1) * A0213 + m(0, 2) * A0113);
  im(3, 3) = det * (m(0, 0) * A1212 - m(0, 1) * A0212 + m(0, 2) * A0112);
  return im;
}
}  // namespace math