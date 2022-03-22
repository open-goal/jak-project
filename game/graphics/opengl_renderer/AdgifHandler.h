#pragma once

#include "common/dma/gs.h"

class AdgifHelper {
 public:
  // takes the 5QW of adgif data
  explicit AdgifHelper(const u8* data) {
    memcpy(&m_data, data, 16 * 5);
    m_tex0 = GsTex0(m_data.tex0_data);
    m_tex1 = GsTex1(m_data.tex1_data);
    m_alpha = GsAlpha(m_data.alpha_data);
  }

  explicit AdgifHelper(const AdGifData& data) : m_data(data) {
    m_tex0 = GsTex0(m_data.tex0_data);
    m_tex1 = GsTex1(m_data.tex1_data);
    m_alpha = GsAlpha(m_data.alpha_data);
  }

  bool is_normal_adgif() const {
    return (u8)m_data.tex0_addr == (u32)GsRegisterAddress::TEX0_1 &&
           (u8)m_data.tex1_addr == (u32)GsRegisterAddress::TEX1_1 &&
           (u8)m_data.mip_addr == (u32)GsRegisterAddress::MIPTBP1_1 &&
           (u8)m_data.clamp_addr == (u32)GsRegisterAddress::CLAMP_1 &&
           (u8)m_data.alpha_addr == (u32)GsRegisterAddress::ALPHA_1;
  }

  std::string print() const {
    std::string result;
    result += fmt::format("[0] {}\n  {}", register_address_name(m_data.tex0_addr), m_tex0.print());
    result += fmt::format("[1] {}\n  {}", register_address_name(m_data.tex1_addr), m_tex1.print());
    result += fmt::format("[2] {}\n", register_address_name(m_data.mip_addr));
    result += fmt::format("[3] {}\n  0x{:x}\n", register_address_name(m_data.clamp_addr),
                          m_data.clamp_data);
    result +=
        fmt::format("[4] {}\n  {}\n", register_address_name(m_data.alpha_addr), m_alpha.print());
    return result;
  }

  const GsAlpha& alpha() const { return m_alpha; }

  const GsTex0& tex0() const { return m_tex0; }

 private:
  AdGifData m_data;
  GsTex0 m_tex0;
  GsTex1 m_tex1;
  GsAlpha m_alpha;
};