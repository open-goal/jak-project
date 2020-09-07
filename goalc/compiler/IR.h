#ifndef JAK_IR_H
#define JAK_IR_H

#include <string>
#include "CodeGenerator.h"
#include "goalc/regalloc/allocate.h"
#include "Val.h"
#include "goalc/emitter/ObjectGenerator.h"

class IR {
 public:
  virtual std::string print() = 0;
  virtual RegAllocInstr to_rai() = 0;
  virtual void do_codegen(emitter::ObjectGenerator* gen,
                          const AllocationResult& allocs,
                          emitter::IR_Record irec) = 0;
  virtual void add_constraints(std::vector<IRegConstraint>* constraints, int my_id) {
    (void)constraints;
    (void)my_id;
  }
};

// class IR_Set : public IR {
// public:
//  std::string print() override;
//  RegAllocInstr to_rai() override;
//};

class IR_Return : public IR {
 public:
  IR_Return(const RegVal* return_reg, const RegVal* value);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void add_constraints(std::vector<IRegConstraint>* constraints, int my_id) override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;
  const RegVal* value() { return m_value; }

 protected:
  const RegVal* m_return_reg = nullptr;
  const RegVal* m_value = nullptr;
};

class IR_LoadConstant64 : public IR {
 public:
  IR_LoadConstant64(const RegVal* dest, u64 value);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 protected:
  const RegVal* m_dest = nullptr;
  u64 m_value = 0;
};

class IR_LoadSymbolPointer : public IR {
 public:
  IR_LoadSymbolPointer(const RegVal* dest, std::string name);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 protected:
  const RegVal* m_dest = nullptr;
  std::string m_name;
};

class IR_SetSymbolValue : public IR {
 public:
  IR_SetSymbolValue(const SymbolVal* dest, const RegVal* src);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 protected:
  const SymbolVal* m_dest = nullptr;
  const RegVal* m_src = nullptr;
};

class IR_GetSymbolValue : public IR {
 public:
  IR_GetSymbolValue(const RegVal* dest, const SymbolVal* src, bool sext);
  std::string print() override;
  RegAllocInstr to_rai() override;
  void do_codegen(emitter::ObjectGenerator* gen,
                  const AllocationResult& allocs,
                  emitter::IR_Record irec) override;

 protected:
  const RegVal* m_dest = nullptr;
  const SymbolVal* m_src = nullptr;
  bool m_sext = false;
};

#endif  // JAK_IR_H
