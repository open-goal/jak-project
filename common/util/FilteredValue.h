#pragma once

template <typename T>
class Filtered {
 public:
  Filtered(const T& alpha = 0.9) : m_val(T(0)), m_alpha(alpha) {}
  Filtered(const T& v, const T& alpha) : m_val(v), m_alpha(alpha) {}
  const T& add(const T& v) {
    m_val = (m_val * m_alpha) + (v * (T(1) - m_alpha));
    return m_val;
  }
  const T& get() const { return m_val; }

 private:
  T m_val;
  T m_alpha;
};