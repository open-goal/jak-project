#include "kscheme.h"

#include "common/common_types.h"
#include "common/goal_constants.h"
#include "common/symbols.h"

#include "game/kernel/common/Symbol4.h"
#include "game/kernel/common/fileio.h"
#include "game/kernel/common/klink.h"
#include "game/kernel/common/kmalloc.h"
#include "game/kernel/common/kprint.h"
#include "game/kernel/common/kscheme.h"
#include "game/kernel/jak3/fileio.h"
#include "game/kernel/jak3/klink.h"

namespace jak3 {

using namespace jak3_symbols;

Ptr<u32> SymbolString;
Ptr<Symbol4<u32>> CollapseQuote;
Ptr<Symbol4<u32>> LevelTypeList;
Ptr<String> UnknownName;
bool DebugSymbols = false;

void kscheme_init_globals() {
  LevelTypeList.offset = 0;
  SymbolString.offset = 0;
  CollapseQuote.offset = 0;
  UnknownName.offset = 0;
  DebugSymbols = false;
}

namespace {
u32 u32_in_fixed_sym(u32 offset) {
  return Ptr<Symbol4<u32>>(s7.offset + offset)->value();
}

void fixed_sym_set(u32 offset, u32 value) {
  Ptr<Symbol4<u32>>(s7.offset + offset)->value() = value;
}

template <typename T>
Ptr<Ptr<String>> sym_to_string_ptr(Ptr<Symbol4<T>> in) {
  return Ptr<Ptr<String>>(SymbolString.offset + in.offset - s7.offset);
}
template <typename T>
Ptr<String> sym_to_string(Ptr<Symbol4<T>> in) {
  return *sym_to_string_ptr(in);
}

}  // namespace

/*!
 * Get a pointer to a symbol. Can provide the symbol id, the name, or both.
 */
Ptr<Symbol4<u32>> find_symbol_from_c(uint16_t sym_id, const char* name) {
  // sign extend
  int extended_sym_id = (int16_t)sym_id;

  if (sym_id == 0xffff) {
    // the ID wasn't provided, so we have to use the name
    if (!name) {
      // always warn - no name or ID!
      MsgErr("dkernel: attempted to find symbol with NULL name and id #x%x\n", sym_id);
      return Ptr<Symbol4<u32>>(0);
    } else {
      // find the symbol
      Ptr<Symbol4<u32>> lookup_result = find_symbol_in_area(name, s7.offset, LastSymbol.offset);
      if (lookup_result.offset == 0) {
        lookup_result = find_symbol_in_area(name, SymbolTable2.offset, s7.offset - 0x10);
      }

      // do some sanity checking, but only in retail or if we've explicitly asked for it.
      if (!DebugSegment || u32_in_fixed_sym(FIX_SYM_KERNEL_SYMBOL_WARNINGS) != s7.offset) {
        if (lookup_result.offset == 0) {
          // lookup by the name failed.
          MsgWarn("dkernel: doing a string->symbol on %s, but could not find the name\n", name);
        } else {
          auto sym_string = sym_to_string(lookup_result);
          // not sure how you could get unknown name here...
          // but the second check sees if you were only saved by having the symbol string in the
          // debug heap. This would tell you that the lookup worked, but would fail in retail mode.
          if ((sym_string == UnknownName) || (kglobalheap->top_base.offset < sym_string.offset)) {
            MsgWarn(
                "dkernel: doing a string->symbol on %s, but the symbol has not been marked "
                "as symbol-export-string\n",
                name);
          }
        }
      }

      return lookup_result;
    }
  } else {
    // just use the ID. warn if there's a name conflict.
    Ptr<Symbol4<u32>> sym(s7.offset + extended_sym_id - 1);
    if (sym.offset != s7.offset + S7_OFF_FIX_SYM_EMPTY_PAIR) {
      auto existing_name = sym_to_string(sym);
      if (existing_name.offset && !strcmp(existing_name->data(), name)) {
        MsgWarn(
            "dkernel: WARNING: attempting to find symbol %s at id #x%x but symbol %s was "
            "already there.\n",
            name, sym_id, existing_name->data());
      }
    }
    return sym;
  }
}

/*!
 * Find or create a symbol.
 * New for Jak 3 is that there is no longer a symbol hash table. So there are some significant
 * changes to how this works. Also, many symbols do not store their name, to save memory.
 *
 * @param sym_id The symbol ID. This _must_ be provided if the symbol does not exist yet, or if the
 * symbol's name isn't known. Use -1 if the symbol ID is unknown.
 *
 * @param name The name. This can be used instead of the ID if the symbol's name is stored.
 *
 * @param flags Optional flag (0x40) can force the symbol's name to be stored. This uses memory.
 *
 */
Ptr<Symbol4<u32>> intern_from_c(int sym_id, int flags, const char* name) {
  // first, look up the symbol.
  Ptr<Symbol4<u32>> symbol = find_symbol_from_c(sym_id, name);
  kheaplogging = true;

  if (symbol.offset == 0) {
    // the function above can only fail if we didn't give an ID.
    MsgErr("dkernel: attempted to intern symbol %s using the name, but could not find it\n", name);
    kheaplogging = false;
    return Ptr<Symbol4<u32>>(0);
  }

  if (symbol.offset == s7.offset + S7_OFF_FIX_SYM_EMPTY_PAIR) {
    // in case it's the empty pair, just return and don't worry about names.
    kheaplogging = false;
    return symbol;
  }

  // if the symbol is new, then the name pointer will be 0, and we need to set it up.
  auto sptr = sym_to_string_ptr(symbol);
  auto current_string = *sptr;
  if (current_string.offset) {   // existing symbol
    if ((flags & 0x40U) == 0) {  // symbol-export-string not set
      // nothing to do!
      kheaplogging = false;
      return symbol;
    }

    // if the symbol-export-string flag is set, we need to make sure that there's a known name
    // and the name is stored in the global heap:
    if ((current_string != UnknownName) &&
        (current_string.offset <= kglobalheap->top_base.offset)) {
      // it is, nothing to do.
      kheaplogging = false;
      return symbol;
    }

    // "upgrade" from the debug heap to global. (this could also trigger if the name was previously
    // unknown)
    MsgWarn("dkernel: upgrading symbol %s (flags #x%x) from debug heap to global\n", name, flags);
    *sptr = make_string_from_c(name);
    kheaplogging = false;
    return symbol;
  }

  // setting up a new symbol case:
  Ptr<String> new_string;
  if (DebugSymbols == 0) {
    // normal mode
    if ((flags & 0x40U) != 0) {
      // if symbol-export-string is set, allocate it on the global heap.
      new_string = make_string_from_c(name);
    } else if (DebugSegment != 0) {
      // if debugsegment, always load all symbols to debug heap for easy debugging.
      new_string = make_debug_string_from_c(name);
    } else {
      // otherwise, no symbols!! save memory!
      new_string = UnknownName;
    }
  } else {
    // debug symbol mode is on - force it to the global heap no matter what.
    new_string = make_string_from_c(name);
  }
  *sptr = new_string;

  NumSymbols++;

  kheaplogging = 0;
  return symbol;
}

u64 intern(u32 name) {
  return intern_from_c(-1, 0x40, Ptr<String>(name)->data()).offset;
}

/*!
 * Configure a type.
 */
Ptr<Type> set_type_values(Ptr<Type> type, Ptr<Type> parent, u64 flags) {
  type->parent = parent;
  type->allocated_size = (flags & 0xffff);
  type->heap_base = (flags >> 16) & 0xffff;
  type->padded_size = ((type->allocated_size + 0xf) & 0xfff0);

  u16 new_methods = (flags >> 32) & 0xffff;  // i think this accidentally uses jak1 style flags.
  if (type->num_methods < new_methods) {
    type->num_methods = new_methods;
  }

  return type;
}

static bool in_valid_memory_for_new_type(u32 addr) {
  if (SymbolTable2.offset <= addr && addr < 0x8000000) {
    return true;
  }

  if (addr < 0x100000 && addr >= 0x84000) {
    return true;
  }
  return false;
}
u32 size_of_type(u32 method_count) {
  return (4 * method_count + 0x23) & 0xfffffff0;
}

static bool is_valid_type(u32 addr) {
  if ((addr & 7) != 4) {
    return false;
  }

  if (*Ptr<u32>(addr - 4) != u32_in_fixed_sym(FIX_SYM_TYPE_TYPE)) {
    return false;
  }

  return true;
}

/*!
 * Given a symbol for the type name, allocate memory for a type and add it to the symbol table.
 * New: in Jak 2, there's a level type list
 */
Ptr<Type> alloc_and_init_type(Ptr<Symbol4<Ptr<Type>>> sym,
                              u32 method_count,
                              bool force_global_type) {
  // number of bytes for this type
  u32 type_size = size_of_type(method_count);
  u32 type_mem = 0;
  ASSERT(sym.offset & 1);

  if (!force_global_type &&
      u32_in_fixed_sym(FIX_SYM_LOADING_LEVEL) != u32_in_fixed_sym(FIX_SYM_GLOBAL_HEAP)) {
    u32 type_list_ptr = LevelTypeList->value();
    if (type_list_ptr == 0) {
      // we don't have a type-list... just alloc on global
      MsgErr("dkernel: trying to init loading level type \'%s\' while type-list is undefined\n",
             sym_to_string(sym)->data());
      type_mem = alloc_heap_object(s7.offset + FIX_SYM_GLOBAL_HEAP,
                                   u32_in_fixed_sym(FIX_SYM_TYPE_TYPE), type_size, UNKNOWN_PP);
    } else {
      // we do have a type list! allocate on the level heap
      type_mem = alloc_heap_object(s7.offset + FIX_SYM_LOADING_LEVEL,
                                   u32_in_fixed_sym(FIX_SYM_TYPE_TYPE), type_size, UNKNOWN_PP);
      // link us!
      u32 old_head = *Ptr<u32>(type_list_ptr);
      *Ptr<u32>(type_list_ptr) = type_mem;
      // I guess we hide this in the memusage method.
      Ptr<Type>(type_mem)->memusage_method.offset = old_head;
    }
  } else {
    // normal global type
    type_mem = alloc_heap_object(s7.offset + FIX_SYM_GLOBAL_HEAP,
                                 u32_in_fixed_sym(FIX_SYM_TYPE_TYPE), type_size, UNKNOWN_PP);
  }

  Ptr<Type> the_type(type_mem);
  sym->value() = the_type;
  the_type->allocated_size = type_size;
  the_type->padded_size = ((type_size + 0xf) & 0xfff0);
  return the_type;
}

/*!
 * Like intern, but returns a type instead of a symbol. If the type doesn't exist, a new one is
 * allocated.
 */
Ptr<Type> intern_type_from_c(int a, int b, const char* name, u64 methods) {
  // there's a weird flag system used here.
  // if methods is a number that's not 0 or 1, its used as the desired number of methods.
  // If method is 0, and a new type needs to be created, it uses 12 methods
  // If method is 1, and a new type needs to be created, it uses 44 methods
  // If method is 0 or 1 and no new type needs to be created, there is no error.
  // Requesting a type to have fewer methods than the existing type has is ok.
  // Requesting a type to have more methods than the existing type is not ok and prints an error.

  auto symbol = intern_from_c(a, b, name);
  u32 sym_value = symbol->value();

  if (!sym_value) {
    // new type
    int n_methods = methods;

    if (methods == 0) {
      // some stupid types like user-defined children of integers have "0" as the method count
      n_methods = 0xc;
    } else if (methods == 1) {
      // whatever builds the v2/v4 object files (level data) doesn't actually know method counts.
      // so it just puts a 1.  In this case, we should put lots of methods, just in case.
      // I guess 44 was the number they picked.
      n_methods = 0x2c;
    }

    // create the type.
    auto casted_sym = symbol.cast<Symbol4<Ptr<Type>>>();
    auto type = alloc_and_init_type(casted_sym, n_methods, 0);  // allow level types
    type->symbol = casted_sym;
    type->num_methods = n_methods;
    return type;
  } else {
    // the type exists.
    auto type = Ptr<Type>(sym_value);
    // note - flags of 0 or 1 will pass through here without triggering the error.
    if (size_of_type(type->num_methods) < size_of_type(methods)) {
      MsgErr(
          "dkernel: trying to redefine a type '%s' with %d methods when it had %d, try "
          "restarting\n",
          name, (u32)methods, type->num_methods);
      ASSERT(false);
    }
    return type;
  }
}

/*!
 * Wrapper of intern_type_from_c to use with GOAL. It accepts a gstring as a name.
 */
u64 intern_type(u32 name, u64 methods) {
  return intern_type_from_c(-1, 0, Ptr<String>(name)->data(), methods).offset;
}

/*!
 * Setup a type which is located in a fixed spot of the symbol table.
 */
Ptr<Type> set_fixed_type(u32 offset,
                         const char* name,
                         Ptr<Symbol4<Ptr<Type>>> parent_symbol,
                         u64 flags,
                         u32 print,
                         u32 inspect) {
  Ptr<Symbol4<Ptr<Type>>> type_symbol(s7.offset + offset);
  Ptr<Type> symbol_value = type_symbol->value();

  // set the symbol's name and hash
  *sym_to_string_ptr(type_symbol) = Ptr<String>(make_string_from_c(name));
  NumSymbols++;

  if (symbol_value.offset == 0) {
    // no type memory exists, let's allocate it. force it global
    // the flag logic here multiplies the method count 2, hopefully
    // this will set up the symbol
    symbol_value = alloc_and_init_type(type_symbol, (flags >> 0x1f) & 0xffff, 1);
  }

  // remember our symbol
  symbol_value->symbol = type_symbol;
  // make our type a type (we're a basic)
  u32 type_of_type = u32_in_fixed_sym(FIX_SYM_TYPE_TYPE);
  *Ptr<u32>(symbol_value.offset - 4) = type_of_type;

  Ptr<Type> parent_type = parent_symbol->value();
  set_type_values(symbol_value, parent_type, flags);

  symbol_value->new_method = parent_type->new_method;
  symbol_value->delete_method = parent_type->delete_method;

  if (!print) {
    symbol_value->print_method = parent_type->print_method;
  } else {
    symbol_value->print_method.offset = print;
  }

  if (!inspect) {
    symbol_value->inspect_method = parent_type->inspect_method;
  } else {
    symbol_value->inspect_method.offset = inspect;
  }

  symbol_value->length_method.offset = u32_in_fixed_sym(FIX_SYM_ZERO_FUNC);
  symbol_value->asize_of_method = parent_type->asize_of_method;
  symbol_value->copy_method = parent_type->copy_method;
  return symbol_value;
}

u64 new_type(u32 symbol, u32 parent, u64 flags) {
  u32 n_methods = (flags >> 32) & 0xffff;
  if (n_methods == 0) {
    // 12 methods used as default, if the user has not provided us with a number
    n_methods = 12;
  }

  auto sym_string = sym_to_string(Ptr<Symbol4<Type>>(symbol));
  const char* sym_string_c = nullptr;
  if (sym_string.offset) {
    sym_string_c = sym_string->data();
  }

  u32 parent_num_methods = Ptr<Type>(parent)->num_methods;

  auto new_type_obj = intern_type_from_c(((symbol - s7.offset) + 1), 0x80, sym_string_c, n_methods);
  u32 original_type_list_value = new_type_obj->memusage_method.offset;
  Ptr<Function>* child_slots = &(new_type_obj->new_method);
  Ptr<Function>* parent_slots = &(Ptr<Type>(parent)->new_method);
  for (u32 i = 0; i < n_methods; i++) {
    if (i < parent_num_methods) {  // bug fix from jak 1
      child_slots[i] = parent_slots[i];
    } else {
      child_slots[i].offset = 0;
    }
  }

  // deal with loading-level types
  if (u32_in_fixed_sym(FIX_SYM_LOADING_LEVEL) == u32_in_fixed_sym(FIX_SYM_GLOBAL_HEAP)) {
    // not loading a level

    // we'll consider a type list if it's #f or a valid type
    if (original_type_list_value && (original_type_list_value == s7.offset ||
                                     (in_valid_memory_for_new_type(original_type_list_value) &&
                                      is_valid_type(original_type_list_value)))) {
      printf("case 1 for new_type level types\n");
      new_type_obj->memusage_method.offset = original_type_list_value;
    }
  } else {
    if (original_type_list_value == 0) {
      // loading a level, but the type is global
      MsgWarn("dkernel: loading-level init of type %s, but was interned global (this is okay)\n",
              sym_to_string(new_type_obj->symbol)->data());
    } else {
      new_type_obj->memusage_method.offset = original_type_list_value;
    }
  }
  return set_type_values(new_type_obj, Ptr<Type>(parent), flags);
}
/*!
 * Is t1 a t2?
 */
u64 type_typep(Ptr<Type> t1, Ptr<Type> t2) {
  if (t1 == t2) {
    return (s7 + FIX_SYM_TRUE).offset;
  }

  do {
    t1 = t1->parent;
    if (t1 == t2) {
      return (s7 + FIX_SYM_TRUE).offset;
    }
  } while (t1.offset && t1.offset != u32_in_fixed_sym(FIX_SYM_OBJECT_TYPE));
  return s7.offset;
}

u64 method_set(u32 type_, u32 method_id, u32 method) {
  Ptr<Type> type(type_);
  if (method_id > 255)
    printf("[METHOD SET ERROR] tried to set method %d\n", method_id);

  auto existing_method = type->get_method(method_id).offset;

  if (method == 1) {
    method = 0;
  } else if (method == 0) {
    return 0;
  } else if (method == 2) {
    method = type->parent->get_method(method_id).offset;
    printf("[Method Set] got 2, inheriting\n");
  }

  // do the set
  type->get_method(method_id).offset = method;

  // now, propagate to children
  // we don't track children directly, so we end up having to iterate the whole symbol to find all
  // types. This is slow, so we only do it in some cases

  // the condition is either setting *enable-method-set* in GOAL, or if we're debugging without the
  // disk boot. The point of doing this in debug is just to print warning messages.
  if (*EnableMethodSet || (!FastLink && MasterDebug && !DiskBoot)) {
    auto sym = Ptr<Symbol4<Ptr<Type>>>(s7.offset);
    for (; sym.offset < LastSymbol.offset; sym.offset += 4) {
      auto sym_value = sym->value();
      if (in_valid_memory_for_new_type(sym_value.offset) && (sym_value.offset & 7) == 4 &&
          *Ptr<u32>(sym_value.offset - 4) == u32_in_fixed_sym(FIX_SYM_TYPE_TYPE) &&
          method_id < sym_value->num_methods &&
          sym_value->get_method(method_id).offset == existing_method &&
          type_typep(sym_value, type) != s7.offset) {
        if (FastLink != 0) {
          printf("************ WARNING **************\n");
          printf("method %d of %s redefined - you must define class heirarchies in order now\n",
                 method_id, sym_to_string(sym)->data());
          printf("***********************************\n");
        }
        sym_value->get_method(method_id).offset = method;
      }
    }

    sym = Ptr<Symbol4<Ptr<Type>>>(SymbolTable2.offset);
    for (; sym.offset < s7.offset; sym.offset += 4) {
      auto sym_value = sym->value();
      if (in_valid_memory_for_new_type(sym_value.offset) && (sym_value.offset & 7) == 4 &&
          *Ptr<u32>(sym_value.offset - 4) == u32_in_fixed_sym(FIX_SYM_TYPE_TYPE) &&
          method_id < sym_value->num_methods &&
          sym_value->get_method(method_id).offset == existing_method &&
          type_typep(sym_value, type) != s7.offset) {
        if (FastLink != 0) {
          printf("************ WARNING **************\n");
          printf("method %d of %s redefined - you must define class heirarchies in order now\n",
                 method_id, sym_to_string(sym)->data());
          printf("***********************************\n");
        }
        sym_value->get_method(method_id).offset = method;
      }
    }
  }

  return method;
}

/*!
 * Call a GOAL method of a given type.
 */
u64 call_method_of_type(u32 arg, Ptr<Type> type, u32 method_id) {
  if (((type.offset < SymbolTable2.offset || 0x7ffffff < type.offset) &&  // not in normal memory
       (type.offset < 0x84000 || 0x100000 <= type.offset))                // not in kernel memory
      || ((type.offset & OFFSET_MASK) != BASIC_OFFSET)) {                 // invalid type
    cprintf("#<#%x has invalid type ptr #x%x>\n", arg, type.offset);
  } else {
    auto type_tag = Ptr<Ptr<Type>>(type.offset - 4);
    if ((*type_tag).offset == u32_in_fixed_sym(FIX_SYM_TYPE_TYPE)) {
      auto f = type->get_method(method_id);
      return call_goal(f, arg, 0, 0, s7.offset, g_ee_main_mem);
    } else {
      cprintf("#<#x%x has invalid type ptr #x%x, bad type #x%x>\n", arg, type.offset,
              (*type_tag).offset);
    }
  }
  printf("[ERROR] call_method_of_type failed!\n");
  return arg;
}

/*!
 * Call a GOAL function with 2 arguments.
 */
u64 call_goal_function_arg2(Ptr<Function> func, u64 a, u64 b) {
  return call_goal(func, a, b, 0, s7.offset, g_ee_main_mem);
}

/*!
 * Call a global GOAL function by name.
 */
u64 call_goal_function_by_name(const char* name) {
  return call_goal_function(Ptr<Function>(intern_from_c(-1, 0, name)->value()));
}

u64 print_object(u32 obj);
u64 print_pair(u32 obj);
u64 print_symbol(u32 obj);

/*!
 * Print an object with a newline after it to the GOAL PrintBuffer (not stdout)
 */
u64 sprint(u32 obj) {
  auto rv = print_object(obj);
  cprintf("\n");
  return rv;
}

/*!
 * Like call_method_of_type, but has two arguments. Used to "relocate" v2/s4 loads.
 */
u64 call_method_of_type_arg2(u32 arg, Ptr<Type> type, u32 method_id, u32 a1, u32 a2) {
  if (((type.offset < SymbolTable2.offset || 0x7ffffff < type.offset) &&  // not in normal memory
       (type.offset < 0x84000 || 0x100000 <= type.offset))                // not in kernel memory
      || ((type.offset & OFFSET_MASK) != BASIC_OFFSET)) {                 // invalid type
    cprintf("#<#%x has invalid type ptr #x%x>\n", arg, type.offset);
  } else {
    auto type_tag = Ptr<Ptr<Type>>(type.offset - 4);
    if ((*type_tag).offset == u32_in_fixed_sym(FIX_SYM_TYPE_TYPE)) {
      // return type->get_method(method_id).cast<u64 (u32,u32,u32)>().c()(arg,a1,a2);
      return call_goal(type->get_method(method_id), arg, a1, a2, s7.offset, g_ee_main_mem);
    } else {
      cprintf("#<#x%x has invalid type ptr #x%x, bad type #x%x>\n", arg, type.offset,
              (*type_tag).offset);
    }
  }
  ASSERT_MSG(false, "[ERROR] call_method_of_type_arg2 failed!");
  return arg;
}

/*!
 * Most generic printing method.
 * Does not correctly handle 64 bit boxed integers or object64's correctly.
 * It is important that no objects of type object actually exist or this will loop!
 */
u64 print_object(u32 obj) {
  if ((obj & OFFSET_MASK) == BINTEGER_OFFSET) {
    return print_binteger(s64(s32(obj)));
  } else {
    if ((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
        (obj < 0x84000 || 0x100000 <= obj)) {              // not in kernel memory
      cprintf("#<invalid object #x%x>", obj);
    } else if ((obj & OFFSET_MASK) == PAIR_OFFSET) {
      return print_pair(obj);
    } else if ((obj & 1) == SYMBOL_OFFSET && obj >= SymbolTable2.offset &&
               obj < LastSymbol.offset) {
      return print_symbol(obj);
    } else if ((obj & OFFSET_MASK) == BASIC_OFFSET) {
      return call_method_of_type(obj, Ptr<Type>(*Ptr<u32>(obj - 4)), GOAL_PRINT_METHOD);
    } else {
      cprintf("#<unknown type %d @ #x%x>", obj & OFFSET_MASK, obj);
    }
  }
  return obj;
}

/*!
 * Default print method a basic.
 * Confirms basic is valid and prints the type name.
 */
u64 print_basic(u32 obj) {
  if (((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
       (obj < 0x84000 || 0x100000 <= obj))                // not in kernel memory
      || ((obj & OFFSET_MASK) != BASIC_OFFSET)) {
    cprintf("#<invalid basic #x%x>", obj);
  } else {
    cprintf("#<%s @ #x%x>", sym_to_string(Ptr<Type>(*Ptr<u32>(obj - 4))->symbol)->data(), obj);
  }
  return obj;
}

/*!
 * Print a pair as a LISP list.  Don't try to print circular lists or it will get stuck
 * Can print improper lists
 */
u64 print_pair(u32 obj) {
  if (obj == s7.offset + S7_OFF_FIX_SYM_EMPTY_PAIR) {
    cprintf("()");
  } else {
    // clang-format off
    // we want to treat ('quote <foo>) as just '<foo> unless
    if (CollapseQuote->value() == s7.offset   // CollapseQuote is enabled
        || ((obj & 7) != 2)                   // this object isn't a pair
        || *Ptr<u32>(obj - 2) != s7.offset + FIX_SYM_QUOTE // the car isn't 'quote
        || (*Ptr<u32>(obj + 2) & 7) != 2                   // the cdr isn't a pair
        || *Ptr<u32>(*Ptr<u32>(obj + 2) + 2) != s7.offset + S7_OFF_FIX_SYM_EMPTY_PAIR  // the cddr isn't '()
        ) {
      // clang-format on
      cprintf("(");
      auto toPrint = obj;
      for (;;) {
        if ((toPrint & OFFSET_MASK) == PAIR_OFFSET) {
          // print CAR
          print_object(*Ptr<u32>(toPrint - 2));

          // load up CDR
          auto cdr = *Ptr<u32>(toPrint + 2);
          toPrint = cdr;
          if (cdr == s7.offset + S7_OFF_FIX_SYM_EMPTY_PAIR) {  // end of proper list
            cprintf(")");
            return obj;
          } else {  // continue list
            cprintf(" ");
          }
        } else {  // improper list
          cprintf(". ");
          print_object(toPrint);
          cprintf(")");
          return obj;
        }
      }
    } else {
      cprintf("'");
      print_object(*Ptr<u32>(*Ptr<u32>(obj + 2) - 2));
    }
  }
  return obj;
}

/*!
 * Print method for symbol.  Just prints the name without quotes or anything fancy.
 */
u64 print_symbol(u32 obj) {
  if (((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
       (obj < 0x84000 || 0x100000 <= obj))                // not in kernel memory
      || ((obj & 1) != 1) || obj < SymbolTable2.offset || obj >= LastSymbol.offset) {
    cprintf("#<invalid symbol #x%x>", obj);
  } else {
    char* str = sym_to_string(Ptr<Symbol4<u32>>(obj))->data();
    cprintf("%s", str);
  }
  return obj;
}

/*!
 * Print method for type.  Just prints the name without quotes
 */
u64 print_type(u32 obj) {
  if (((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
       (obj < 0x84000 || 0x100000 <= obj))                // not in kernel memory
      || ((obj & OFFSET_MASK) != BASIC_OFFSET) ||
      *Ptr<u32>(obj - 4) != u32_in_fixed_sym(FIX_SYM_TYPE_TYPE)) {
    cprintf("#<invalid type #x%x>", obj);
  } else {
    cprintf("%s", sym_to_string(Ptr<Type>(obj)->symbol)->data());
  }
  return obj;
}

/*!
 * Print method for string.  Prints the string in quotes.
 */
u64 print_string(u32 obj) {
  if (((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
       (obj < 0x84000 || 0x100000 <= obj))                // not in kernel memory
      || ((obj & OFFSET_MASK) != BASIC_OFFSET) ||
      *Ptr<u32>(obj - 4) != u32_in_fixed_sym(FIX_SYM_STRING_TYPE)) {
    if (obj == s7.offset) {
      cprintf("#f");  // new in jak 2.

    } else {
      cprintf("#<invalid string #x%x>", obj);
    }
  } else {
    cprintf("\"%s\"", Ptr<String>(obj)->data());
  }
  return obj;
}

/*!
 * Print method for function. Just prints the address because functions can't identify themselves.
 */
u64 print_function(u32 obj) {
  cprintf("#<compiled %s @ #x%x>", sym_to_string(Ptr<Type>(*Ptr<u32>(obj - 4))->symbol)->data(),
          obj);
  return obj;
}

/*!
 * Get the allocated size field of a basic.  By default we grab this from the type struct.
 * Dynamically sized basics should override this method.
 */
u64 asize_of_basic(u32 it) {
  return Ptr<Type>(*Ptr<u32>(it - BASIC_OFFSET))->allocated_size;
}

/*!
 * Create a copy of a basic.  If the destination isn't identified as a symbol, treat it as an
 * address. This seems a little bit unsafe to me, as it reads the 4-bytes before the given address
 * and checks it against the symbol type pointer to see if its a symbol. It seems possible to have a
 * false positive for this check.
 */
u64 copy_basic(u32 obj, u32 heap, u32 /*unused*/, u32 pp) {
  // determine size of basic. We call a method instead of using asize_of_basic in case the type has
  // overridden the default asize_of method.
  u32 size = call_method_of_type(obj, Ptr<Type>(*Ptr<u32>(obj - BASIC_OFFSET)), GOAL_ASIZE_METHOD);
  u32 result;

  if ((heap & 1) == 1) {
    // we think we're creating a new copy on a heap.  First allocate memory...
    result = alloc_heap_object(heap, *Ptr<u32>(obj - BASIC_OFFSET), size, pp);
    // then copy! (minus the type tag, alloc_heap_object already did it for us)
    memcpy(Ptr<u32>(result).c(), Ptr<u32>(obj).c(), size - BASIC_OFFSET);
  } else {
    printf("DANGER COPY BASIC!\n");
    // copy directly (including type tag)
    memcpy(Ptr<u32>(heap - BASIC_OFFSET).c(), Ptr<u32>(obj - BASIC_OFFSET).c(), size);
    result = heap;
  }
  return result;
}

u64 inspect_pair(u32 obj);
u64 inspect_symbol(u32 obj);
/*!
 * Highest level inspect method. Won't inspect 64-bit bintegers correctly.
 */
u64 inspect_object(u32 obj) {
  if ((obj & OFFSET_MASK) == BINTEGER_OFFSET) {
    return inspect_binteger(obj);
  } else {
    if ((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
        (obj < 0x84000 || 0x100000 <= obj)) {              // not in kernel memory
      cprintf("#<invalid object #x%x>", obj);
    } else if ((obj & OFFSET_MASK) == PAIR_OFFSET) {
      return inspect_pair(obj);
    } else if ((obj & 1) == SYMBOL_OFFSET && obj >= SymbolTable2.offset &&
               obj < LastSymbol.offset) {
      return inspect_symbol(obj);
    } else if ((obj & OFFSET_MASK) == BASIC_OFFSET) {
      return call_method_of_type(obj, Ptr<Type>(*Ptr<u32>(obj - BASIC_OFFSET)),
                                 GOAL_INSPECT_METHOD);
    } else {
      cprintf("#<unknown type %d @ #x%x>", obj & OFFSET_MASK, obj);
    }
  }
  return obj;
}

/*!
 * Inspect a pair.
 */
u64 inspect_pair(u32 obj) {
  cprintf("[%8x] pair ", obj);
  print_pair(obj);
  cprintf("\n");
  return obj;
}

/*!
 * Inspect a string. There's a typo in allocated_length (has underscore instead of dash).
 * This typo is fixed in later games.
 */
u64 inspect_string(u32 obj) {
  if (((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
       (obj < 0x84000 || 0x100000 <= obj))                // not in kernel memory
      || ((obj & OFFSET_MASK) != BASIC_OFFSET) ||
      *Ptr<u32>(obj - 4) != u32_in_fixed_sym(FIX_SYM_STRING_TYPE)) {
    cprintf("#<invalid string #x%x>\n", obj);
  } else {
    auto str = Ptr<String>(obj);
    cprintf("[%8x] string\n\tallocated-length: %d\n\tdata: \"%s\"\n", obj, str->len, str->data());
  }
  return obj;
}

/*!
 * Inspect a symbol.
 */
u64 inspect_symbol(u32 obj) {
  if (((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
       (obj < 0x84000 || 0x100000 <= obj))                // not in kernel memory
      || ((obj & 1) != 1) || obj < SymbolTable2.offset || obj >= LastSymbol.offset) {
    cprintf("#<invalid symbol #x%x>", obj);
  } else {
    auto sym = Ptr<Symbol4<u32>>(obj);
    cprintf("[%8x] symbol\n\tname: %s\n\tvalue: ", obj, sym_to_string(sym)->data());
    print_object(sym->value());
    cprintf("\n");
  }
  return obj;
}

/*!
 * Inspect a type.
 */
u64 inspect_type(u32 obj) {
  if (((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
       (obj < 0x84000 || 0x100000 <= obj))                // not in kernel memory
      || ((obj & OFFSET_MASK) != BASIC_OFFSET) ||
      *Ptr<u32>(obj - 4) != u32_in_fixed_sym(FIX_SYM_TYPE_TYPE)) {
    cprintf("#<invalid type #x%x>\n", obj);
  } else {
    auto typ = Ptr<Type>(obj);
    auto sym = typ->symbol;

    cprintf("[%8x] type\n\tname: %s\n\tparent: ", obj, sym_to_string(sym)->data());
    print_object(typ->parent.offset);
    cprintf("\n\tsize: %d/%d\n\theap-base: %d\n\tallocated-length: %d\n\tprint: ",
            typ->allocated_size, typ->padded_size, typ->heap_base, typ->num_methods);
    print_object(typ->print_method.offset);
    cprintf("\n\tinspect: ");
    print_object(typ->inspect_method.offset);
    cprintf("\n");
  }
  return obj;
}

/*!
 * Inspect a basic. This is just a fallback for basics which don't know how to inspect themselves.
 * We just use print_object.
 */
u64 inspect_basic(u32 obj) {
  if (((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
       (obj < 0x84000 || 0x100000 <= obj))                // not in kernel memory
      || ((obj & OFFSET_MASK) != BASIC_OFFSET)) {
    if (obj == s7.offset) {
      // added in jak2 (and inlined in jak 3, but only the final version?)
      return inspect_symbol(obj);
    } else {
      cprintf("#<invalid basic #x%x>\n", obj);
    }
  } else {
    cprintf("[%8x] ", obj);
    print_object(*Ptr<u32>(obj - 4));
    cprintf("\n");
  }
  return obj;
}

/*!
 * Inspect a link block. This link block doesn't seem to be used at all.
 */
u64 inspect_link_block(u32 ob) {
  struct LinkBlock {
    u32 length;
    u32 version;
  };

  auto lb = Ptr<LinkBlock>(ob);
  cprintf("[%8x] link-block\n\tallocated_length: %d\n\tversion: %d\n\tfunction: ", ob, lb->length,
          lb->version);
  print_object(ob + lb->length);
  cprintf("\n");
  return ob;
}

int InitHeapAndSymbol() {
  Ptr<u32> symbol_table =
      kmalloc(kglobalheap, 4 * GOAL_MAX_SYMBOLS, KMALLOC_MEMSET, "symbol-table").cast<u32>();
  SymbolString =
      kmalloc(kglobalheap, 4 * GOAL_MAX_SYMBOLS, KMALLOC_MEMSET, "string-table").cast<u32>();
  SymbolString.offset += 2 * GOAL_MAX_SYMBOLS;  // point to the middle
  LastSymbol = symbol_table + 0xff00;
  SymbolTable2 = symbol_table + 5;
  s7 = symbol_table + 0x8001;
  NumSymbols = 0;
  reset_output();
  // empty pair (this is extra confusing).
  *Ptr<u32>(s7.offset + FIX_SYM_EMPTY_CAR - 1) = s7.offset + S7_OFF_FIX_SYM_EMPTY_PAIR;
  *Ptr<u32>(s7.offset + FIX_SYM_EMPTY_CDR - 1) = s7.offset + S7_OFF_FIX_SYM_EMPTY_PAIR;
  fixed_sym_set(FIX_SYM_GLOBAL_HEAP, kglobalheap.offset);

  /*
  UnknownName = make_string_from_c("*unknown-symbol-name*");
  alloc_and_init_type((int)pvVar4 + 0x8019, 9, 1);
  alloc_and_init_type((int)pvVar4 + 0x8015, 9, 1);
  alloc_and_init_type((int)pvVar4 + 0x8011, 9, 1);
  alloc_and_init_type((int)pvVar4 + 0x8009, 9, 1);
  set_fixed_symbol(0, "#f", pvVar14);
  set_fixed_symbol(4, "#t", (void*)((int)pvVar4 + 0x8005));
  pvVar12 = (void*)make_nothing_func();
  set_fixed_symbol(0x94, "nothing", pvVar12);
  pvVar12 = (void*)make_zero_func();
  set_fixed_symbol(0xc0, "zero-func", pvVar12);
  pvVar12 = (void*)make_function_from_c(&LAB_001064c0);
  set_fixed_symbol(0xc4, "asize-of-basic-func", pvVar12);
  pvVar12 = (void*)make_function_from_c(FUN_001064e0);
  set_fixed_symbol(200, "asize-of-basic-func", pvVar12);
  pvVar12 = (void*)make_function_from_c(FUN_001048a8);
  set_fixed_symbol(0x98, "delete-basic", pvVar12);
  set_fixed_symbol(0xa0, "global", kglobalheap);
  set_fixed_symbol(0xa4, "debug", kdebugheap);
  set_fixed_symbol(0x9c, "static", (void*)((int)pvVar4 + 0x809d));
  set_fixed_symbol(0xa8, "loading-level", kglobalheap);
  set_fixed_symbol(0xac, "loading-package", kglobalheap);
  set_fixed_symbol(0xb0, "process-level-heap", kglobalheap);
  set_fixed_symbol(0xb4, "stack", (void*)((int)pvVar4 + 0x80b5));
  set_fixed_symbol(0xb8, "scratch", (void*)((int)pvVar4 + 0x80b9));
  set_fixed_symbol(0xbc, "*scratch-top*", (void*)0x70000000);
  set_fixed_symbol(0xcc, "level", (void*)0x0);
  set_fixed_symbol(0xd0, "art-group", (void*)0x0);
  set_fixed_symbol(0xd4, "texture-page-dir", (void*)0x0);
  set_fixed_symbol(0xd8, "texture-page", (void*)0x0);
  set_fixed_symbol(0xdc, "sound", (void*)0x0);
  set_fixed_symbol(0xe0, "dgo", (void*)0x0);
  set_fixed_symbol(0xe4, "top-level", *(void**)((int)pvVar4 + 0x8094));
  set_fixed_symbol(0xe8, "quote", (void*)((int)pvVar4 + 0x80e9));
  set_fixed_symbol(0xec, "*listener-link-block*", (void*)0x0);
  set_fixed_symbol(0xf0, "*listener-function*", (void*)0x0);
  set_fixed_symbol(0xf4, "*stack-top*", (void*)0x0);
  set_fixed_symbol(0xf8, "*stack-base*", (void*)0x0);
  set_fixed_symbol(0xfc, "*stack-size*", (void*)0x0);
  set_fixed_symbol(0x100, "*kernel-function*", (void*)0x0);
  set_fixed_symbol(0x104, "*kernel-packages*", (void*)0x0);
  set_fixed_symbol(0x108, "*kernel-boot-message*", (void*)0x0);
  set_fixed_symbol(0x10c, "*kernel-boot-mode*", (void*)0x0);
  set_fixed_symbol(0x110, "*kernel-boot-level*", (void*)0x0);
  set_fixed_symbol(0x114, "*kernel-boot-art-group*", (void*)0x0);
  set_fixed_symbol(0x118, "*kernel-debug*", (void*)0x0);
  set_fixed_symbol(0x11c, "*kernel-version*", (void*)0x0);
  set_fixed_symbol(0x120, "kernel-dispatcher", (void*)0x0);
  set_fixed_symbol(0x124, "sync-dispatcher", (void*)0x0);
  set_fixed_symbol(0x128, "*print-collumn*", (void*)0x0);
  set_fixed_symbol(300, "*debug-segment*", (void*)0x0);
  set_fixed_symbol(0x130, "*enable-method-set*", (void*)0x0);
  set_fixed_symbol(0x134, "*sql-result*", (void*)0x0);
  set_fixed_symbol(0x138, "*collapse-quote*", (void*)0x0);
  set_fixed_symbol(0x13c, "*level-type-list*", (void*)0x0);
  set_fixed_symbol(0x140, "*deci-count*", (void*)0x0);
  set_fixed_symbol(0x144, "*user*", (void*)0x0);
  set_fixed_symbol(0x148, "*video-mode*", (void*)0x0);
  set_fixed_symbol(0x14c, "*boot-video-mode*", (void*)0x0);
  set_fixed_symbol(0x150, "boot", (void*)0x0);
  set_fixed_symbol(0x154, "demo", (void*)0x0);
  set_fixed_symbol(0x158, "demo-shared", (void*)0x0);
  set_fixed_symbol(0x15c, "preview", (void*)0x0);
  set_fixed_symbol(0x160, "kiosk", (void*)0x0);
  set_fixed_symbol(0x164, "play-boot", (void*)0x0);
  set_fixed_symbol(0x168, "sin", (void*)0x0);
  set_fixed_symbol(0x16c, "cos", (void*)0x0);
  set_fixed_symbol(0x170, "put-display-env", (void*)0x0);
  set_fixed_symbol(0x174, "syncv", (void*)0x0);
  set_fixed_symbol(0x178, "sync-path", (void*)0x0);
  set_fixed_symbol(0x17c, "reset-path", (void*)0x0);
  set_fixed_symbol(0x180, "reset-graph", (void*)0x0);
  set_fixed_symbol(0x184, "dma-sync", (void*)0x0);
  set_fixed_symbol(0x188, "gs-put-imr", (void*)0x0);
  set_fixed_symbol(0x18c, "gs-get-imr", (void*)0x0);
  set_fixed_symbol(400, "gs-store-image", (void*)0x0);
  set_fixed_symbol(0x194, "flush-cache", (void*)0x0);
  set_fixed_symbol(0x198, "cpad-open", (void*)0x0);
  set_fixed_symbol(0x19c, "cpad-get-data", (void*)0x0);
  set_fixed_symbol(0x1a0, "mouse-get-data", (void*)0x0);
  set_fixed_symbol(0x1a4, "keybd-get-data", (void*)0x0);
  set_fixed_symbol(0x1a8, "install-handler", (void*)0x0);
  set_fixed_symbol(0x1ac, "install-debug-handler", (void*)0x0);
  set_fixed_symbol(0x1b0, "file-stream-open", (void*)0x0);
  set_fixed_symbol(0x1b4, "file-stream-close", (void*)0x0);
  set_fixed_symbol(0x1b8, "file-stream-length", (void*)0x0);
  set_fixed_symbol(0x1bc, "file-stream-seek", (void*)0x0);
  set_fixed_symbol(0x1c0, "file-stream-read", (void*)0x0);
  set_fixed_symbol(0x1c4, "file-stream-write", (void*)0x0);
  set_fixed_symbol(0x1c4, "file-stream-write", (void*)0x0);
  set_fixed_symbol(0x1c8, "scf-get-language", (void*)0x0);
  set_fixed_symbol(0x1cc, "scf-get-time", (void*)0x0);
  set_fixed_symbol(0x1d0, "scf-get-aspect", (void*)0x0);
  set_fixed_symbol(0x1d4, "scf-get-volume", (void*)0x0);
  set_fixed_symbol(0x1d8, "scf-get-territory", (void*)0x0);
  set_fixed_symbol(0x1dc, "scf-get-timeout", (void*)0x0);
  set_fixed_symbol(0x1e0, "scf-get-inactive-timeout", (void*)0x0);
  set_fixed_symbol(0x1e4, "dma-to-iop", (void*)0x0);
  set_fixed_symbol(0x1e8, "kernel-shutdown", (void*)0x0);
  set_fixed_symbol(0x1ec, "aybabtu", (void*)0x0);
  set_fixed_symbol(0x1f0, "string->symbol", (void*)0x0);
  set_fixed_symbol(500, "symbol->string", (void*)0x0);
  set_fixed_symbol(0x1f8, "print", (void*)0x0);
  set_fixed_symbol(0x1fc, "inspect", (void*)0x0);
  set_fixed_symbol(0x200, "load", (void*)0x0);
  set_fixed_symbol(0x204, "loadb", (void*)0x0);
  set_fixed_symbol(0x208, "loado", (void*)0x0);
  set_fixed_symbol(0x20c, "unload", (void*)0x0);
  set_fixed_symbol(0x210, "_format", (void*)0x0);
  set_fixed_symbol(0x214, "malloc", (void*)0x0);
  set_fixed_symbol(0x218, "kmalloc", (void*)0x0);
  set_fixed_symbol(0x21c, "kmemopen", (void*)0x0);
  set_fixed_symbol(0x220, "kmemclose", (void*)0x0);
  set_fixed_symbol(0x224, "new-dynamic-structure", (void*)0x0);
  set_fixed_symbol(0x228, "method-set!", (void*)0x0);
  set_fixed_symbol(0x22c, "link", (void*)0x0);
  set_fixed_symbol(0x230, "link-busy?", (void*)0x0);
  set_fixed_symbol(0x234, "link-reset", (void*)0x0);
  set_fixed_symbol(0x238, "link-begin", (void*)0x0);
  set_fixed_symbol(0x23c, "link-resume", (void*)0x0);
  set_fixed_symbol(0x240, "dgo-load", (void*)0x0);
  set_fixed_symbol(0x244, "sql-query", (void*)0x0);
  set_fixed_symbol(0x248, "mc-run", (void*)0x0);
  set_fixed_symbol(0x24c, "mc-format", (void*)0x0);
  set_fixed_symbol(0x250, "mc-unformat", (void*)0x0);
  set_fixed_symbol(0x254, "mc-create-file", (void*)0x0);
  set_fixed_symbol(600, "mc-save", (void*)0x0);
  set_fixed_symbol(0x25c, "mc-load", (void*)0x0);
  set_fixed_symbol(0x260, "mc-check-result", (void*)0x0);
  set_fixed_symbol(0x264, "mc-get-slot-info", (void*)0x0);
  set_fixed_symbol(0x268, "mc-makefile", (void*)0x0);
  set_fixed_symbol(0x26c, "kset-language", (void*)0x0);
  set_fixed_symbol(0x270, "rpc-call", (void*)0x0);
  set_fixed_symbol(0x274, "rpc-busy?", (void*)0x0);
  set_fixed_symbol(0x278, "test-load-dgo-c", (void*)0x0);
  set_fixed_symbol(0x27c, "symlink2", (void*)0x0);
  set_fixed_symbol(0x280, "symlink3", (void*)0x0);
  set_fixed_symbol(0x284, "ultimate-memcpy", (void*)0x0);
  set_fixed_symbol(0x288, "play", (void*)0x0);
  set_fixed_symbol(0x28c, "*symbol-string*", SymbolString);
  set_fixed_symbol(0x290, "*kernel-symbol-warnings*", (void*)((int)pvVar4 + 0x8005));
  set_fixed_symbol(0x294, "network-bootstrap", (void*)0x0);
  uVar5 = make_function_from_c(FUN_00104610);
  uVar6 = make_function_from_c(&LAB_00104650);
  iVar10 = make_function_from_c(print_object);
  iVar11 = make_function_from_c(inspect_object);
  set_fixed_type(0x1c, "object", (longlong)((int)pvVar4 + 0x801d), 0x900000004, iVar10, iVar11);
  iVar9 = *(int*)((int)pvVar4 + 0x801c);
  uVar7 = *(undefined4*)((int)pvVar4 + 0x80c0);
  *(undefined4*)(iVar9 + 0x10) = *(undefined4*)((int)pvVar4 + 0x8094);
  *(undefined4*)(iVar9 + 0x14) = uVar6;
  *(undefined4*)(iVar9 + 0x24) = uVar7;
  uVar7 = make_function_from_c(&LAB_001064d0);
  *(undefined4*)(*(int*)((int)pvVar4 + 0x801c) + 0x28) = uVar7;
  iVar10 = make_function_from_c(FUN_00105e30);
  iVar11 = make_function_from_c(FUN_00106ac0);
  set_fixed_type(0x6c, "structure", (longlong)((int)pvVar4 + 0x801d), 0x900000004, iVar10, iVar11);
  uVar7 = make_function_from_c(FUN_00104840);
  *(undefined4*)(*(int*)((int)pvVar4 + 0x806c) + 0x10) = uVar7;
  uVar7 = make_function_from_c(&LAB_00104870);
  *(undefined4*)(*(int*)((int)pvVar4 + 0x806c) + 0x14) = uVar7;
  iVar10 = make_function_from_c(FUN_00105e68);
  iVar11 = make_function_from_c(FUN_00106af8);
  set_fixed_type(0xc, "basic", (longlong)((int)pvVar4 + 0x806d), 0x900000004, iVar10, iVar11);
  uVar8 = make_function_from_c(FUN_00104890);
  iVar9 = *(int*)((int)pvVar4 + 0x800c);
  uVar7 = *(undefined4*)((int)pvVar4 + 0x8098);
  uVar1 = *(undefined4*)((int)pvVar4 + 0x80c4);
  uVar2 = *(undefined4*)((int)pvVar4 + 0x80c8);
  *(undefined4*)(iVar9 + 0x10) = uVar8;
  *(undefined4*)(iVar9 + 0x14) = uVar7;
  *(undefined4*)(iVar9 + 0x24) = uVar1;
  *(undefined4*)(iVar9 + 0x28) = uVar2;
  iVar10 = make_function_from_c((void*)0x1061d8);
  iVar11 = make_function_from_c((void*)0x1068a8);
  set_fixed_type(0x14, "symbol", (longlong)((int)pvVar4 + 0x801d), 0x900000004, iVar10, iVar11);
  iVar9 = *(int*)((int)pvVar4 + 0x8014);
  *(undefined4*)(iVar9 + 0x10) = uVar5;
  *(undefined4*)(iVar9 + 0x14) = uVar6;
  iVar10 = make_function_from_c(FUN_001062a8);
  iVar11 = make_function_from_c(FUN_00106998);
  set_fixed_type(0x18, "type", (longlong)((int)pvVar4 + 0x800d), 0x900000038, iVar10, iVar11);
  uVar7 = make_function_from_c(FUN_001054d0);
  iVar9 = *(int*)((int)pvVar4 + 0x8018);
  *(undefined4*)(iVar9 + 0x10) = uVar7;
  *(undefined4*)(iVar9 + 0x14) = uVar6;
  iVar10 = make_function_from_c(FUN_00106368);
  iVar11 = make_function_from_c(FUN_001067f8);
  set_fixed_type(0x10, "string", (longlong)((int)pvVar4 + 0x800d), 0x900000008, iVar10, iVar11);
  iVar10 = make_function_from_c(FUN_00106430);
  set_fixed_type(8, "function", (longlong)((int)pvVar4 + 0x800d), 0x900000004, iVar10, 0);
  iVar9 = *(int*)((int)pvVar4 + 0x8008);
  *(undefined4*)(iVar9 + 0x10) = uVar5;
  *(undefined4*)(iVar9 + 0x14) = uVar6;
  iVar10 = make_function_from_c(FUN_00106488);
  iVar11 = make_function_from_c(FUN_00106c20);
  set_fixed_type(0x80, "vu-function", (longlong)((int)pvVar4 + 0x806d), 0x900000010, iVar10,
                 iVar11);
  *(undefined4*)(*(int*)((int)pvVar4 + 0x8080) + 0x14) = uVar6;
  iVar10 = make_function_from_c(FUN_00106bc8);
  set_fixed_type(0x20, "link-block", (longlong)((int)pvVar4 + 0x800d), 0x90000000c, 0, iVar10);
  iVar9 = *(int*)((int)pvVar4 + 0x8020);
  *(undefined4*)(iVar9 + 0x10) = uVar5;
  *(undefined4*)(iVar9 + 0x14) = uVar6;
  iVar10 = make_function_from_c(kheapstatus);
  set_fixed_type(0x90, "kheap", (longlong)((int)pvVar4 + 0x806d), 0x900000010, 0, iVar10);
  set_fixed_type(0x7c, "array", (longlong)((int)pvVar4 + 0x800d), 0x900000010, 0, 0);
  iVar10 = make_function_from_c(FUN_00105f48);
  iVar11 = make_function_from_c((void*)0x1066b8);
  set_fixed_type(0x70, "pair", (longlong)((int)pvVar4 + 0x801d), 0x900000008, iVar10, iVar11);
  uVar7 = make_function_from_c(new_pair);
  *(undefined4*)(*(int*)((int)pvVar4 + 0x8070) + 0x10) = uVar7;
  uVar7 = make_function_from_c(FUN_00104918);
  *(undefined4*)(*(int*)((int)pvVar4 + 0x8070) + 0x14) = uVar7;
  set_fixed_type(0x60, "process-tree", (longlong)((int)pvVar4 + 0x800d), 0xe00000024, 0, 0);
  set_fixed_type(100, "process", (longlong)((int)pvVar4 + 0x8061), 0xe00000080, 0, 0);
  set_fixed_type(0x68, "thread", (longlong)((int)pvVar4 + 0x800d), 0xc00000028, 0, 0);
  set_fixed_type(0x84, "connectable", (longlong)((int)pvVar4 + 0x806d), 0x900000010, 0, 0);
  set_fixed_type(0x88, "stack-frame", (longlong)((int)pvVar4 + 0x800d), 0x90000000c, 0, 0);
  set_fixed_type(0x8c, "file-stream", (longlong)((int)pvVar4 + 0x800d), 0x900000014, 0, 0);
  set_fixed_type(0x74, "pointer", (longlong)((int)pvVar4 + 0x801d), 0x900000004, 0, 0);
  *(undefined4*)(*(int*)((int)pvVar4 + 0x8074) + 0x10) = uVar5;
  iVar10 = make_function_from_c(FUN_00106090);
  iVar11 = make_function_from_c(FUN_00106700);
  set_fixed_type(0x78, "number", (longlong)((int)pvVar4 + 0x801d), 0x900000008, iVar10, iVar11);
  *(undefined4*)(*(int*)((int)pvVar4 + 0x8078) + 0x10) = uVar5;
  iVar10 = make_function_from_c(FUN_00106170);
  iVar11 = make_function_from_c(FUN_00106770);
  set_fixed_type(0x5c, "float", (longlong)((int)pvVar4 + 0x8079), 0x900000004, iVar10, iVar11);
  set_fixed_type(0x24, "integer", (longlong)((int)pvVar4 + 0x8079), 0x900000008, 0, 0);
  iVar10 = make_function_from_c(FUN_00106100);
  iVar11 = make_function_from_c(FUN_00106738);
  set_fixed_type(0x30, "binteger", (longlong)((int)pvVar4 + 0x8025), 0x900000008, iVar10, iVar11);
  set_fixed_type(0x28, "sinteger", (longlong)((int)pvVar4 + 0x8025), 0x900000008, 0, 0);
  set_fixed_type(0x34, "int8", (longlong)((int)pvVar4 + 0x8029), 0x900000001, 0, 0);
  set_fixed_type(0x38, "int16", (longlong)((int)pvVar4 + 0x8029), 0x900000002, 0, 0);
  set_fixed_type(0x3c, "int32", (longlong)((int)pvVar4 + 0x8029), 0x900000004, 0, 0);
  set_fixed_type(0x40, "int64", (longlong)((int)pvVar4 + 0x8029), 0x900000008, 0, 0);
  set_fixed_type(0x44, "int128", (longlong)((int)pvVar4 + 0x8029), 0x900000010, 0, 0);
  set_fixed_type(0x2c, "uinteger", (longlong)((int)pvVar4 + 0x8025), 0x900000008, 0, 0);
  set_fixed_type(0x48, "uint8", (longlong)((int)pvVar4 + 0x802d), 0x900000001, 0, 0);
  set_fixed_type(0x4c, "uint16", (longlong)((int)pvVar4 + 0x802d), 0x900000002, 0, 0);
  set_fixed_type(0x50, "uint32", (longlong)((int)pvVar4 + 0x802d), 0x900000004, 0, 0);
  set_fixed_type(0x54, "uint64", (longlong)((int)pvVar4 + 0x802d), 0x900000008, 0, 0);
  set_fixed_type(0x58, "uint128", (longlong)((int)pvVar4 + 0x802d), 0x900000010, 0, 0);
  uVar7 = make_function_from_c(FUN_00104808);
  *(undefined4*)(*(int*)((int)pvVar4 + 0x801c) + 0x10) = uVar7;
  make_function_symbol_from_c("string->symbol", FUN_00105148);
  make_function_symbol_from_c("symbol->string", FUN_00104c18);
  make_function_symbol_from_c("print", FUN_00105cb8);
  make_function_symbol_from_c("inspect", inspect_object);
  make_function_symbol_from_c("load", load);
  make_function_symbol_from_c("loadb", loadb);
  make_function_symbol_from_c("loado", loado);
  make_function_symbol_from_c("unload", unload);
  make_function_symbol_from_c("_format", FUN_00101860);
  make_function_symbol_from_c("malloc", FUN_001047e8);
  make_function_symbol_from_c("kmalloc", FUN_00104678);
  make_function_symbol_from_c("kmemopen", &LAB_001144e8);
  make_function_symbol_from_c("kmemclose", kmemclose);
  make_function_symbol_from_c("new-dynamic-structure", FUN_00104858);
  make_function_symbol_from_c("method-set!", FUN_00105738);
  make_function_symbol_from_c("link", link_and_exec);
  make_function_symbol_from_c("link-busy?", FUN_001092d0);
  make_function_symbol_from_c("link-reset", &LAB_001092e0);
  make_function_symbol_from_c("dgo-load", load_and_link_dgo);
  make_raw_function_symbol_from_c("ultimate-memcpy", (void*)0x0);
  make_raw_function_symbol_from_c("symlink2", (void*)0x0);
  make_raw_function_symbol_from_c("symlink3", (void*)0x0);
  make_function_symbol_from_c("link-begin", link_begin);
  make_function_symbol_from_c("link-resume", link_resume);
  make_function_symbol_from_c("sql-query", sql_query_sync);
  make_function_symbol_from_c("mc-run", MC_run);
  make_function_symbol_from_c("mc-format", &LAB_0010aba8);
  make_function_symbol_from_c("mc-unformat", &LAB_0010abe8);
  make_function_symbol_from_c("mc-create-file", &LAB_0010ac28);
  make_function_symbol_from_c("mc-save", &LAB_0010ac68);
  make_function_symbol_from_c("mc-load", &LAB_0010acb0);
  make_function_symbol_from_c("mc-check-result", &LAB_0010ae20);
  make_function_symbol_from_c("mc-get-slot-info", &LAB_0010ae30);
  make_function_symbol_from_c("mc-makefile", FUN_0010acf8);
  make_function_symbol_from_c("kset-language", FUN_0010ab70);
  iVar9 = intern_from_c(0xffff, 0, "*debug-segment*");
  pvVar12 = (void*)((int)pvVar4 + 0x8005);
  if (DebugSegment == 0) {
    pvVar12 = pvVar14;
  }
  *(void**)(iVar9 + -1) = pvVar12;
  iVar9 = intern_from_c(0xffff, 0, "*enable-method-set*");
  *(undefined4*)(iVar9 + -1) = 0;
  EnableMethodSet = (int*)(iVar9 + -1);
  DAT_001297bc = intern_from_c(0xffff, 0, "*kernel-debug*");
  *(undefined4*)(DAT_001297bc + -1) = 0;
  DAT_001297bc = DAT_001297bc + -1;
  iVar9 = intern_from_c(0xffff, 0, "*boot-video-mode*");
  *(undefined4*)(iVar9 + -1) = 0;
  iVar9 = intern_from_c(0xffff, 0, "*video-mode*");
  *(undefined4*)(iVar9 + -1) = 0;
  SqlResult = intern_from_c(0xffff, 0, "*sql-result*");
  *(void**)(SqlResult + -1) = pvVar14;
  DAT_001297e4 = intern_from_c(0xffff, 0, "*collapse-quote*");
  *(int*)(DAT_001297e4 + -1) = (int)pvVar4 + 0x8005;
  DAT_001297e8 = intern_from_c(0xffff, 0, "*level-type-list*");
  *EnableMethodSet = *EnableMethodSet + 1;
  load_and_link_dgo_from_c(0x130388, (longlong)(int)kglobalheap, 0xd, 0x400000);
  *EnableMethodSet = *EnableMethodSet + -1;
  iVar9 = intern_from_c(0xffff, 0, "*kernel-version*");
  uVar3 = *(uint*)(iVar9 + -1);
  uVar13 = uVar3 >> 0x13;
  if (uVar3 == 0) {
    uVar13 = 0;
  } else {
    if (uVar13 == 0x16) {
      DAT_001292b0 = intern_from_c(0xffff, 0, "*deci-count*");
      InitListener();
      InitMachineScheme();
      kmemclose();
      return 0;
    }
  }
  MsgErr("\n");
  MsgErr(
      "dkernel: compiled C kernel version is %d.%d but the goal kernel is %d.%d!\n\tfrom thegoal> "
      "prompt (:mch) then mkee your kernel in linux.\n",
      0x16, 0, uVar13, uVar3 >> 3 & 0xffff);
  */
  return -1;
}

u64 load(u32 /*file_name_in*/, u32 /*heap_in*/) {
  ASSERT(false);
  return 0;
}

u64 loadb(u32 /*file_name_in*/, u32 /*heap_in*/, u32 /*param3*/) {
  ASSERT(false);
  return 0;
}

u64 loadc(const char* /*file_name*/, kheapinfo* /*heap*/, u32 /*flags*/) {
  ASSERT(false);
  return 0;
}

u64 loado(u32 file_name_in, u32 heap_in) {
  char loadName[272];
  Ptr<String> file_name(file_name_in);
  Ptr<kheapinfo> heap(heap_in);
  printf("****** CALL TO loado(%s) ******\n", file_name->data());
  kstrcpy(loadName, MakeFileName(DATA_FILE_TYPE, file_name->data(), 0));
  s32 returnValue = load_and_link(file_name->data(), loadName, heap.c(), LINK_FLAG_PRINT_LOGIN);

  if (returnValue < 0) {
    return s7.offset;
  } else {
    return returnValue;
  }
}

/*!
 * "Unload". Doesn't free memory, just informs listener we unloaded.
 */
u64 unload(u32 name) {
  output_unload(Ptr<String>(name)->data());
  return 0;
}

s64 load_and_link(const char* filename, char* decode_name, kheapinfo* heap, u32 flags) {
  (void)filename;
  s32 sz;
  auto rv = FileLoad(decode_name, make_ptr(heap), Ptr<u8>(0), KMALLOC_ALIGN_64, &sz);
  if (((s32)rv.offset) > -1) {
    return (s32)link_and_exec(rv, decode_name, sz, make_ptr(heap), flags, false).offset;
  }
  return (s32)rv.offset;
}

}  // namespace jak3
