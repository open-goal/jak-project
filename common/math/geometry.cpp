#include "geometry.h"

namespace math {

Vector4f bsphere_of_triangle(const Vector3f* v) {
  Vector4f bsphere;
  auto& p1 = v[0];
  auto& p2 = v[1];
  auto& p3 = v[2];
  float A = (p1 - p2).length();
  float B = (p2 - p3).length();
  float C = (p3 - p1).length();

  const Vector3f *a = &p3, *b = &p1, *c = &p2;
  if (B < C)
    std::swap(B, C), std::swap(b, c);
  if (A < B)
    std::swap(A, B), std::swap(a, b);

  float r;
  math::Vector3f origin;
  if ((B * B) + (C * C) <= (A * A)) {
    r = A / 2.f;
    origin = (*b + *c) / 2.f;
  } else {
    float cos_a = (B * B + C * C - A * A) / (B * C * 2);
    r = A / (sqrt(1 - cos_a * cos_a) * 2.f);
    Vector3f alpha = *a - *c, beta = *b - *c;
    origin = (beta * alpha.dot(alpha) - alpha * beta.dot(beta)).cross(alpha.cross(beta)) /
                 (alpha.cross(beta).dot(alpha.cross(beta)) * 2.f) +
             *c;
  }
  bsphere.x() = origin.x();
  bsphere.y() = origin.y();
  bsphere.z() = origin.z();
  bsphere.w() = r;
  return bsphere;
}

}  // namespace math