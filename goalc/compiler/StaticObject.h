#pragma once

#include <string>
#include <vector>
#include "common/type_system/TypeSpec.h"
#include "goalc/emitter/ObjectGenerator.h"
#include "goalc/compiler/ConstantValue.h"
#include "common/util/BitUtils.h"

class StaticObject {
 public:
  virtual std::string print() const = 0;

  struct LoadInfo {
    bool requires_load = false;
    int load_size = -1;
    bool load_signed = false;
    bool prefer_xmm = false;
  };

  virtual LoadInfo get_load_info() const = 0;
  virtual void generate(emitter::ObjectGenerator* gen) = 0;
  virtual int get_addr_offset() const = 0;
  virtual ~StaticObject() = default;

  emitter::StaticRecord rec;
};

class StaticFloat : public StaticObject {
 public:
  explicit StaticFloat(float _value, int _seg);
  float value = 0;
  int seg = -1;
  std::string print() const override;
  LoadInfo get_load_info() const override;
  void generate(emitter::ObjectGenerator* gen) override;
  int get_addr_offset() const override;
};

class StaticStructure : public StaticObject {
 public:
  StaticStructure(int _seg);
  std::vector<u8> data;
  int seg = -1;
  std::string print() const override;
  LoadInfo get_load_info() const override;
  void generate_structure(emitter::ObjectGenerator* gen);
  void generate(emitter::ObjectGenerator* gen) override;
  int get_addr_offset() const override;
  void set_offset(int offset) { m_offset = offset; }

  struct SymbolRecord {
    int offset = -1;
    std::string name;
  };

  struct PointerRecord {
    int offset_in_this = -1;
    StaticStructure* dest = nullptr;
    int offset_in_dest = -1;
  };

  std::vector<SymbolRecord> types;
  std::vector<SymbolRecord> symbols;
  std::vector<PointerRecord> pointers;

  void add_symbol_record(std::string name, int offset);
  void add_pointer_record(int offset_in_this, StaticStructure* dest, int offset_in_dest);
  void add_type_record(std::string name, int offset);

 private:
  int m_offset = 0;
};

class StaticBasic : public StaticStructure {
 public:
  std::string type_name;
  StaticBasic(int _seg, std::string _type_name);
  int get_addr_offset() const override;
};

class StaticString : public StaticBasic {
 public:
  explicit StaticString(std::string data, int _seg);
  std::string text;
  std::string print() const override;
  void generate(emitter::ObjectGenerator* gen) override;
};

/*!
 * Represents a "static value". Like a reference to a static structure (including pair, string,
 * basic), a symbol, or some constant (bitfield, integer, float).  Cannot be used to store a static
 * structure itself, just a reference to one, meaning you cannot set an inlined structure to a
 * StaticResult.
 */
class StaticResult {
 public:
  StaticResult() = default;

  static StaticResult make_structure_reference(StaticStructure* structure, TypeSpec ts);
  static StaticResult make_constant_data(const ConstantValue& data, TypeSpec ts);
  static StaticResult make_constant_data(u64 data, const TypeSpec& ts);
  static StaticResult make_symbol(const std::string& name);

  std::string print() const;

  const TypeSpec& typespec() const { return m_ts; }
  bool is_reference() const { return m_kind == Kind::STRUCTURE_REFERENCE; }
  bool is_constant_data() const { return m_kind == Kind::CONSTANT_DATA; }
  bool is_symbol() const { return m_kind == Kind::SYMBOL; }

  StaticStructure* reference() const {
    assert(is_reference());
    return m_struct;
  }

  s32 constant_s32() const {
    assert(is_constant_data() && m_constant_data && m_constant_data->size() == 8 &&
           integer_fits(m_constant_data->value_64(), 4, true));
    return (s32)m_constant_data->value_64();
  }

  const std::string& symbol_name() const {
    assert(is_symbol());
    return m_symbol;
  }

  u64 constant_u64() const {
    assert(is_constant_data() && m_constant_data && m_constant_data->size() == 8);
    return m_constant_data->value_64();
  }

  const ConstantValue& constant() const {
    assert(m_constant_data.has_value());
    return *m_constant_data;
  }

 private:
  // used for all types
  TypeSpec m_ts;

  // used for only STRUCTURE_REFERENCE
  StaticStructure* m_struct = nullptr;

  // used for only constant data
  std::optional<ConstantValue> m_constant_data;

  // used for only symbol
  std::string m_symbol;

  enum class Kind { STRUCTURE_REFERENCE, CONSTANT_DATA, SYMBOL, INVALID } m_kind = Kind::INVALID;
};

class StaticPair : public StaticStructure {
 public:
  StaticPair(StaticResult car, StaticResult cdr, int _seg);
  int get_addr_offset() const override;
  void generate(emitter::ObjectGenerator* gen) override;
  void generate_item(const StaticResult& item, int offset);

 private:
  StaticResult m_car, m_cdr;
};
