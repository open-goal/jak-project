/*!
 * @file: GoalPlace.h
 * A "Place" is a typed reference to a register, memory, etc...
 */

#ifndef JAK_GOALVAR_H
#define JAK_GOALVAR_H

#include <string>
#include <memory>

#include "GoalType.h"
#include "codegen/ColoringAssignment.h"
#include "GoalLambda.h"
#include "StaticObject.h"

// Top Level Place
class Place {
 public:
  Place(TypeSpec& c) : type(c) {}
  virtual std::string print() = 0;
  virtual bool is_register() { return false; }
  virtual ColoringInput get_assignment() {
    throw std::runtime_error("invalid Place get_assignment: " + print());
  }
  TypeSpec type;
};

// Special Place indicating "don't care" values.
// Reading the value of a none place is undefined.
class NonePlace : public Place {
 public:
  explicit NonePlace(TypeSpec none_ctype) : Place(none_ctype) {}
  std::string print() { return "none"; }
};

// Place for a General Purpose Register.
class GprPlace : public Place {
 public:
  GprPlace(int id, TypeSpec& ct) : Place(ct), identity(id) {}
  bool is_register() override { return true; }
  ColoringInput get_assignment() override {
    ColoringInput ass;
    ass.kind = RegisterKind::REG_GPR;
    ass.id = identity;
    return ass;
  }
  int identity;
  std::string print() { return "var-" + std::to_string(identity); }
};

// "Alias" for a GPR which allows a GPR to referenced as a different type.
class GprAliasPlace : public Place {
 public:
  GprAliasPlace(std::shared_ptr<Place> _parent, TypeSpec& ct) : Place(ct), parent(_parent) {}

  ColoringInput get_assignment() override { return parent->get_assignment(); }

  bool is_register() override { return true; }

  std::string print() { return std::string("alias-of ") + parent->print(); }

  std::shared_ptr<Place> parent;
};

// Place for an XMM Floating Point register
class XmmPlace : public Place {
 public:
  XmmPlace(int id, TypeSpec& ct) : Place(ct), identity(id) {}
  bool is_register() override { return true; }
  ColoringInput get_assignment() override {
    ColoringInput ass;
    ass.kind = RegisterKind::REG_XMM_FLOAT;
    ass.id = identity;
    return ass;
  }
  int identity;
  std::string print() { return "xvar-" + std::to_string(identity); }
};

// "Alias" for a XMM which allows a XMM to referenced as a different type.
class XmmAliasPlace : public Place {
 public:
  XmmAliasPlace(std::shared_ptr<Place> _parent, TypeSpec& ct) : Place(ct), parent(_parent) {}
  bool is_register() override { return true; }
  ColoringInput get_assignment() override { return parent->get_assignment(); }

  std::string print() { return std::string("alias-of ") + parent->print(); }

  std::shared_ptr<Place> parent;
};

// Place for a GOAL Symbol
class SymbolPlace : public Place {
 public:
  SymbolPlace(const std::string& s, TypeSpec& ct) : Place(ct), name(s) {}
  std::string name;
  std::string print() { return "<" + name + ">"; }
};

class FunctionEnv;

// Place for a GOAL Lambda.
// Contains both a GoalLambda, and the FunctionEnv, if the lambda generates a real function.
class LambdaPlace : public Place {
 public:
  LambdaPlace(TypeSpec ts) : Place(ts) {}
  std::string print() override {
    if (lambda.name.empty()) {
      return "~noname~";
    } else {
      return "lambda-" + lambda.name;
    }
  }
  GoalLambda lambda;
  std::shared_ptr<FunctionEnv> func = nullptr;
};

// Place for a static variable.
class StaticPlace : public Place {
 public:
  StaticPlace(TypeSpec ts, std::shared_ptr<StaticObject> o) : Place(ts), object(o) {}
  std::shared_ptr<StaticObject> object;

  std::string print() override { return "static place " + object->print(); }
};

// Place indicating a location in memory. Just a wrapper around some address that give it the right
// type (address may internally be an integer, but MemoryBasePlace should be something else).
class MemoryBasePlace : public Place {
 public:
  MemoryBasePlace(TypeSpec ts, std::shared_ptr<Place> _base) : Place(ts), base(_base) {}

  std::shared_ptr<Place> base;

  std::string print() override { return base->print(); }
};

// Place indicating a constant offset from a memory address.
class MemoryOffsetConstPlace : public MemoryBasePlace {
 public:
  MemoryOffsetConstPlace(TypeSpec ts, int32_t _offset, std::shared_ptr<Place> _base)
      : MemoryBasePlace(ts, _base), offset(_offset) {}

  int32_t offset;

  std::string print() override { return MemoryBasePlace::print() + "+" + std::to_string(offset); }
};

// Place indicating a variable offset from a memory address
class MemoryOffsetVarPlace : public MemoryBasePlace {
 public:
  MemoryOffsetVarPlace(TypeSpec ts, std::shared_ptr<Place> _offset, std::shared_ptr<Place> _base)
      : MemoryBasePlace(ts, _base), offset(_offset) {}

  std::shared_ptr<Place> offset;

  std::string print() override { return MemoryBasePlace::print() + "+" + offset->print(); }
};

// Place indicating the value stored in memory at a certain location.
class MemoryDerefPlace : public MemoryBasePlace {
 public:
  MemoryDerefPlace(TypeSpec ts, int32_t _size, bool _is_signed, std::shared_ptr<Place> _base)
      : MemoryBasePlace(ts, _base), size(_size), is_signed(_is_signed) {}

  int32_t size;
  bool is_signed;

  std::string print() override {
    return "[" + base->print() + "] s?" + (is_signed ? "y" : "f") + "sz: " + std::to_string(size);
  }
};

class PairPlace : public MemoryBasePlace {
 public:
  PairPlace(TypeSpec ts, bool _is_car, std::shared_ptr<Place> _base)
      : MemoryBasePlace(ts, _base), is_car(_is_car) {}

  std::string print() override {
    return std::string("(") + (is_car ? "car" : "cdr") + " " + base->print() + ")";
  }
  bool is_car;
};

class AliasPlace : public Place {
 public:
  AliasPlace(TypeSpec ts, std::shared_ptr<Place> _base) : Place(ts), base(_base) {}
  std::shared_ptr<Place> base;

  std::string print() override { return "alias-of " + base->print(); }
};

class IntegerConstantPlace : public Place {
 public:
  IntegerConstantPlace(TypeSpec ts, int64_t _value) : Place(ts), value(_value) {}
  int64_t value;

  std::string print() override { return "integer constant " + std::to_string(value); }
};

class BitfieldPlace : public Place {
 public:
  BitfieldPlace(GoalBitField _field, std::shared_ptr<Place> _base)
      : Place(_field.type), base(_base), field(_field) {}

  std::shared_ptr<Place> base;
  GoalBitField field;

  std::string print() override {
    return "[bit-field " + field.print() + " of " + base->print() + "]";
  }
};

std::shared_ptr<Place> get_none();

#endif  // JAK_GOALVAR_H
