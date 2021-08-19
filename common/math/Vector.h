#pragma once

#include <cmath>

#include "third-party/fmt/core.h"

namespace math {

template <typename T, int Size>
class Vector {
 public:
  Vector() = default;
  static Vector<T, Size> zero() {
    Vector<T, Size> result;
    for (auto& x : result.m_data) {
      x = T(0);
    }
    return result;
  }

  template <typename... Args>
  Vector(Args... args) : m_data{T(args)...} {}

  T* begin() { return &m_data[0]; }
  T* end() { return &m_data[Size]; }
  const T* begin() const { return &m_data[0]; }
  const T* end() const { return &m_data[Size]; }

  T& x() { return m_data[0]; }

  const T& x() const { return m_data[0]; }

  T& y() {
    static_assert(Size >= 1, "Out of bounds");
    return m_data[1];
  }

  const T& y() const {
    static_assert(Size >= 1, "Out of bounds");
    return m_data[1];
  }

  T& z() {
    static_assert(Size >= 2, "Out of bounds");
    return m_data[2];
  }

  const T& z() const {
    static_assert(Size >= 2, "Out of bounds");
    return m_data[2];
  }

  T& w() {
    static_assert(Size >= 3, "Out of bounds");
    return m_data[3];
  }

  const T& w() const {
    static_assert(Size >= 3, "Out of bounds");
    return m_data[3];
  }

  const T squared_length() const {
    T sum = T(0);
    for (auto val : m_data) {
      sum += val * val;
    }
    return sum;
  }

  const T length() const { return std::sqrt(squared_length()); }

  Vector<T, Size> operator+(const Vector<T, Size>& other) const {
    Vector<T, Size> result;
    for (int i = 0; i < Size; i++) {
      result[i] = m_data[i] + other[i];
    }
    return result;
  }

  Vector<T, Size> operator-(const Vector<T, Size>& other) const {
    Vector<T, Size> result;
    for (int i = 0; i < Size; i++) {
      result[i] = m_data[i] - other[i];
    }
    return result;
  }

  T dot(const Vector<T, Size>& other) const {
    T result(0);
    for (int i = 0; i < Size; i++) {
      result += m_data[i] * other[i];
    }
    return result;
  }

  T operator[](int idx) const { return m_data[idx]; }

  T& operator[](int idx) { return m_data[idx]; }

  Vector<T, Size> operator/(const T& val) const {
    Vector<T, Size> result;
    for (int i = 0; i < Size; i++) {
      result[i] = m_data[i] / val;
    }
    return result;
  }

  Vector<T, Size> operator*(const T& val) const {
    Vector<T, Size> result;
    for (int i = 0; i < Size; i++) {
      result[i] = m_data[i] * val;
    }
    return result;
  }

  Vector<T, Size> cross(const Vector<T, Size>& other) const {
    static_assert(Size == 3, "Size for cross");
    Vector<T, Size> result = {y() * other.z() - z() * other.y(), z() * other.x() - x() * other.z(),
                              x() * other.y() - y() * other.x()};
    return result;
  }

  Vector<T, Size> normalized(const T& norm = T(1)) const { return (*this) * (norm / length()); }

  void normalize(const T& norm = T(1)) { *this = normalized(norm); }

  std::string to_string_aligned() const {
    std::string result = "[";
    for (auto x : m_data) {
      result.append(fmt::format("{: 6.3f} ", x));
    }
    result.pop_back();
    return result + "]";
  }

  T* data() { return m_data; }
  const T* data() const { return m_data; }

  template <typename U>
  Vector<U, Size> cast() {
    Vector<U, Size> result;
    for (int i = 0; i < Size; i++) {
      result[i] = (U)m_data[i];
    }
    return result;
  }

 private:
  T m_data[Size];
};

template <typename T>
using Vector2 = Vector<T, 2>;

template <typename T>
using Vector3 = Vector<T, 3>;

template <typename T>
using Vector4 = Vector<T, 4>;

using Vector2f = Vector2<float>;
using Vector3f = Vector3<float>;
using Vector4f = Vector4<float>;
using Vector2d = Vector2<double>;
using Vector3d = Vector3<double>;
using Vector4d = Vector4<double>;
}  // namespace math
