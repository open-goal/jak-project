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
  constexpr Vector(Args... args) : m_data{T(args)...} {
    static_assert(sizeof...(args) == Size, "Incorrect number of args");
  }

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

  bool operator==(const Vector<T, Size>& other) const {
    for (int i = 0; i < Size; i++) {
      if (m_data[i] != other.m_data[i]) {
        return false;
      }
    }
    return true;
  }

  bool operator==(const T other) const {
    for (int i = 0; i < Size; i++) {
      if (m_data[i] != other) {
        return false;
      }
    }
    return true;
  }

  bool operator!=(const Vector<T, Size>& other) const { return !((*this) == other); }
  bool operator!=(const T other) const { return !((*this) == other); }

  const T length() const { return std::sqrt(squared_length()); }

  Vector<T, Size> operator+(const Vector<T, Size>& other) const {
    Vector<T, Size> result;
    for (int i = 0; i < Size; i++) {
      result[i] = m_data[i] + other[i];
    }
    return result;
  }

  Vector<T, Size> operator+(const T& other) const {
    Vector<T, Size> result;
    for (int i = 0; i < Size; i++) {
      result[i] = m_data[i] + other;
    }
    return result;
  }

  Vector<T, Size>& operator+=(const Vector<T, Size>& other) {
    for (int i = 0; i < Size; i++) {
      m_data[i] += other[i];
    }
    return *this;
  }

  Vector<T, Size>& operator-=(const Vector<T, Size>& other) {
    for (int i = 0; i < Size; i++) {
      m_data[i] -= other[i];
    }
    return *this;
  }

  Vector<T, Size>& operator-=(const T& other) {
    for (int i = 0; i < Size; i++) {
      m_data[i] -= other;
    }
    return *this;
  }

  Vector<T, Size>& operator+=(const T& other) {
    for (int i = 0; i < Size; i++) {
      m_data[i] += other;
    }
    return *this;
  }

  Vector<T, Size> elementwise_multiply(const Vector<T, Size>& other) const {
    Vector<T, Size> result;
    for (int i = 0; i < Size; i++) {
      result[i] = m_data[i] * other[i];
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

  Vector<T, Size> operator-(const T& other) const {
    Vector<T, Size> result;
    for (int i = 0; i < Size; i++) {
      result[i] = m_data[i] - other;
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

  Vector<T, Size>& operator*=(const T& val) {
    for (int i = 0; i < Size; i++) {
      m_data[i] *= val;
    }
    return *this;
  }

  Vector<T, Size>& operator/=(const T& val) {
    for (int i = 0; i < Size; i++) {
      m_data[i] /= val;
    }
    return *this;
  }

  Vector<T, Size> cross(const Vector<T, Size>& other) const {
    static_assert(Size == 3, "Size for cross");
    Vector<T, Size> result{y() * other.z() - z() * other.y(), z() * other.x() - x() * other.z(),
                           x() * other.y() - y() * other.x()};
    return result;
  }

  Vector<T, Size> normalized(const T& norm = T(1)) const { return (*this) * (norm / length()); }

  void normalize(const T& norm = T(1)) { *this = normalized(norm); }

  void max_in_place(const Vector<T, Size>& other) {
    for (int i = 0; i < Size; i++) {
      m_data[i] = std::max(m_data[i], other[i]);
    }
  }

  void min_in_place(const Vector<T, Size>& other) {
    for (int i = 0; i < Size; i++) {
      m_data[i] = std::min(m_data[i], other[i]);
    }
  }

  std::string to_string_aligned() const {
    std::string result = "[";
    for (auto x : m_data) {
      result.append(fmt::format("{:6.3f} ", x));
    }
    result.pop_back();
    return result + "]";
  }

  std::string to_string_hex_byte() const {
    std::string result = "[";
    for (auto x : m_data) {
      result.append(fmt::format("0x{:02x} ", x));
    }
    result.pop_back();
    return result + "]";
  }

  std::string to_string_hex_word() const {
    std::string result = "[";
    for (auto x : m_data) {
      result.append(fmt::format("0x{:08x} ", x));
    }
    result.pop_back();
    return result + "]";
  }

  T* data() { return m_data; }
  const T* data() const { return m_data; }

  template <typename U>
  Vector<U, Size> cast() const {
    Vector<U, Size> result;
    for (int i = 0; i < Size; i++) {
      result[i] = (U)m_data[i];
    }
    return result;
  }

  template <int len>
  Vector<T, len> head() const {
    static_assert(len < Size);
    Vector<T, len> result;
    for (int i = 0; i < len; i++) {
      result[i] = m_data[i];
    }
    return result;
  }

  Vector<T, 3> xyz() const { return head<3>(); }
  Vector<T, 2> xy() const { return head<2>(); }

  void fill(const T& val) {
    for (auto& x : m_data) {
      x = val;
    }
  }

  void set_zero() { fill(0); }

 private:
  T m_data[Size];
};

// column major
template <typename T, int Rows, int Cols>
struct Matrix {
  Matrix() = default;

  static Matrix zero() {
    Matrix result;
    for (auto& x : result.m_data) {
      x = 0;
    }
    return result;
  }

  static Matrix identity() {
    Matrix result;
    for (int c = 0; c < Cols; c++) {
      for (int r = 0; r < Rows; r++) {
        result(r, c) = r == c ? T(1) : T(0);
      }
    }
    return result;
  }

  void set_zero() {
    for (auto& x : m_data) {
      x = 0;
    }
  }

  T& operator()(int r, int c) { return m_data[r + c * Rows]; }
  const T& operator()(int r, int c) const { return m_data[r + c * Rows]; }

  Vector<T, Rows> col(int c) const {
    Vector<T, Rows> result;
    for (int i = 0; i < Rows; i++) {
      result[i] = m_data[c * Rows + i];
    }
    return result;
  }

  T* data() { return m_data; }
  const T* data() const { return m_data; }

  std::string to_string_aligned() const {
    std::string result;
    for (int row = 0; row < Rows; row++) {
      result += "[";
      for (int col = 0; col < Cols; col++) {
        result.append(fmt::format("{:6.3f} ", m_data[row + col * Rows]));
      }
      result.pop_back();
      result += "]\n";
    }

    return result;
  }

  template <int OtherCols>
  Matrix<T, Rows, OtherCols> operator*(const Matrix<T, Cols, OtherCols>& y) const {
    Matrix<T, Rows, OtherCols> result;
    result.set_zero();
    for (int rx = 0; rx < Rows; rx++) {
      for (int cx = 0; cx < Cols; cx++) {
        for (int yi = 0; yi < OtherCols; yi++) {
          result(rx, yi) += operator()(rx, cx) * y(cx, yi);
        }
      }
    }
    return result;
  }

  Vector<T, Rows> operator*(const Vector<T, Cols>& y) const {
    Vector<T, Rows> result;
    result.set_zero();
    for (int rx = 0; rx < Rows; rx++) {
      for (int cx = 0; cx < Cols; cx++) {
        result[rx] += operator()(rx, cx) * y[cx];
      }
    }
    return result;
  }

 private:
  T m_data[Rows * Cols];
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

using Matrix4f = Matrix<float, 4, 4>;
}  // namespace math
