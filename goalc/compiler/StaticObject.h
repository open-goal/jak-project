#pragma once

#include <string>
#include <vector>

#include "common/type_system/TypeSpec.h"
#include "common/util/BitUtils.h"

#include "goalc/compiler/ConstantValue.h"
#include "goalc/emitter/ObjectGenerator.h"

class FunctionEnv;

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

  struct FunctionRecord {
    const FunctionEnv* func = nullptr;
    int offset_in_this = -1;
  };

  std::vector<SymbolRecord> types;
  std::vector<SymbolRecord> symbols;
  std::vector<PointerRecord> pointers;
  std::vector<FunctionRecord> functions;

  void add_symbol_record(std::string name, int offset);
  void add_pointer_record(int offset_in_this, StaticStructure* dest, int offset_in_dest);
  void add_type_record(std::string name, int offset);
  void add_function_record(const FunctionEnv* function, int offset);

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

class FunctionEnv;

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
  static StaticResult make_type_ref(const std::string& type_name, int method_count);
  static StaticResult make_func_ref(const FunctionEnv* func, const TypeSpec& ts);

  const TypeSpec& typespec() const { return m_ts; }
  bool is_reference() const { return m_kind == Kind::STRUCTURE_REFERENCE; }
  bool is_constant_data() const { return m_kind == Kind::CONSTANT_DATA; }
  bool is_symbol() const { return m_kind == Kind::SYMBOL; }
  bool is_type() const { return m_kind == Kind::TYPE; }
  bool is_func() const { return m_kind == Kind::FUNCTION_REFERENCE; }

  StaticStructure* reference() const {
    ASSERT(is_reference());
    return m_struct;
  }

  s32 constant_s32() const {
    ASSERT(is_constant_data() && m_constant_data && m_constant_data->size() == 8 &&
           integer_fits(m_constant_data->value_64(), 4, true));
    return (s32)m_constant_data->value_64();
  }

  const std::string& symbol_name() const {
    ASSERT(is_symbol() || is_type());
    return m_symbol;
  }

  const FunctionEnv* function() const {
    ASSERT(is_func());
    return m_func;
  }

  int method_count() const {
    ASSERT(is_type());
    return m_method_count;
  }

  u64 constant_u64() const {
    ASSERT(is_constant_data() && m_constant_data && m_constant_data->size() == 8);
    return m_constant_data->value_64();
  }

  const ConstantValue& constant() const {
    ASSERT(m_constant_data.has_value());
    return *m_constant_data;
  }

 private:
  // used for all types
  TypeSpec m_ts;

  // used for only STRUCTURE_REFERENCE
  StaticStructure* m_struct = nullptr;

  // used for only FUNCTION_REFERENCE
  const FunctionEnv* m_func = nullptr;

  // used for only constant data
  std::optional<ConstantValue> m_constant_data;

  // used for only symbol and type
  std::string m_symbol;

  // used only for type
  int m_method_count = -1;

  enum class Kind {
    STRUCTURE_REFERENCE,
    CONSTANT_DATA,
    SYMBOL,
    TYPE,
    FUNCTION_REFERENCE,
    INVALID
  } m_kind = Kind::INVALID;
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
