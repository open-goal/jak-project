#include "third-party/fmt/core.h"
#include "TypeSystem.h"

bool debug_reverse_lookup = true;

std::string FieldReverseLookupOutput::Token::print() const {
  switch (kind) {
    case Kind::FIELD:
      return name;
    case Kind::CONSTANT_IDX:
      return std::to_string(idx);
    case Kind::VAR_IDX:
      return "__VAR__";
    default:
      assert(false);
  }
}

FieldReverseLookupOutput TypeSystem::reverse_field_lookup(
    const FieldReverseLookupInput& input) const {
  FieldReverseLookupOutput result;
  result.success = try_reverse_lookup(input, &result.tokens, &result.addr_of, &result.result_type);
  // todo check for only one var lookup.
  return result;
}

bool TypeSystem::try_reverse_lookup(const FieldReverseLookupInput& input,
                                    std::vector<FieldReverseLookupOutput::Token>* path,
                                    bool* addr_of,
                                    TypeSpec* result_type) const {
  if (debug_reverse_lookup) {
    fmt::print("try_reverse_lookup on {} offset {} deref {} stride {}", input.base_type.print(),
               input.offset, input.deref.has_value(), input.stride);
  }

  //  // base case:
  //  if (input.offset == 0 && !input.deref.has_value() && input.stride == 0) {
  //    // note, could also be getting the address of the first thing in the struct??
  //    *addr_of = false;
  //    *result_type = input.base_type;
  //    return true;
  //  }

  auto base_input_type = input.base_type.base_type();
  if (base_input_type == "pointer") {
    return try_reverse_lookup_pointer(input, path, addr_of, result_type);
  } else if (base_input_type == "inline-array") {
    return try_reverse_lookup_inline_array(input, path, addr_of, result_type);
  } else if (base_input_type == "array") {
    return try_reverse_lookup_array(input, path, addr_of, result_type);
  } else {
    return try_reverse_lookup_other(input, path, addr_of, result_type);
  }
  return false;
}

bool deref_matches(const DerefInfo& expected, const DerefKind& actual) {
  assert(expected.mem_deref);
  assert(expected.can_deref);
  if (actual.is_store) {
    // don't check sign extension
    return expected.load_size == actual.size && expected.reg == actual.reg_kind;
  } else {
    return expected.load_size == actual.size && expected.sign_extend == actual.sign_extend &&
           expected.reg == actual.reg_kind;
  }
}

bool TypeSystem::try_reverse_lookup_pointer(const FieldReverseLookupInput& input,
                                            std::vector<FieldReverseLookupOutput::Token>* path,
                                            bool* addr_of,
                                            TypeSpec* result_type) const {
  auto di = get_deref_info(input.base_type);
  assert(di.mem_deref);  // it's accessing a pointer.
  auto elt_type = di.result_type;
  if (input.stride) {
    // variable access to the array.
    if (input.stride != di.stride) {
      // mismatched array strides, fail!
      return false;
    }
    if (input.offset != 0) {
      // can't access within an element of a pointer array.
      // todo - this could be some sort of constant folding for the next operation.
      return false;
    }

    FieldReverseLookupOutput::Token token;
    token.kind = FieldReverseLookupOutput::Token::Kind::VAR_IDX;
    path->push_back(token);
    if (input.deref.has_value()) {
      if (deref_matches(di, input.deref.value())) {
        // access element of array
        *addr_of = false;
        *result_type = elt_type;
        return true;
      } else {
        // this isn't the right type of dereference.
        return false;
      }
    } else {
      // get the address of a variable indexed element of a pointer-style array
      *addr_of = true;
      *result_type = make_pointer_typespec(elt_type);
      return true;
    }
  } else {
    // either access array or just plain deref a pointer.
    int elt_idx = input.offset / di.stride;
    int offset_into_elt = input.offset - (elt_idx * di.stride);
    if (offset_into_elt) {
      // should line up correctly.
      return false;
    }

    FieldReverseLookupOutput::Token token;
    token.kind = FieldReverseLookupOutput::Token::Kind::CONSTANT_IDX;
    token.idx = elt_idx;
    if (input.deref.has_value()) {
      if (!deref_matches(di, input.deref.value())) {
        // this isn't the right type of dereference
        return false;
      }
      if (elt_idx != 0) {
        // (-> thing) is probably better than (-> thing 0), in the case thing is just a pointer
        // to a single thing that we're dereferencing.
        path->push_back(token);
      }
      // access constant idx element of array
      *addr_of = false;
      *result_type = elt_type;
      return true;
    } else {
      // we want (&-> arr 0)
      path->push_back(token);
      // get address of constant idx element of array
      *addr_of = true;
      *result_type = make_pointer_typespec(elt_type);
      return true;
    }
  }
}

bool TypeSystem::try_reverse_lookup_array(const FieldReverseLookupInput& input,
                                          std::vector<FieldReverseLookupOutput::Token>* path,
                                          bool* addr_of,
                                          TypeSpec* result_type) const {
  return false;
}

bool TypeSystem::try_reverse_lookup_inline_array(const FieldReverseLookupInput& input,
                                                 std::vector<FieldReverseLookupOutput::Token>* path,
                                                 bool* addr_of,
                                                 TypeSpec* result_type) const {
  return false;
}

bool TypeSystem::try_reverse_lookup_other(const FieldReverseLookupInput& input,
                                          std::vector<FieldReverseLookupOutput::Token>* path,
                                          bool* addr_of,
                                          TypeSpec* result_type) const {
  return false;
}