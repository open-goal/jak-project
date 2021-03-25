/*!
 * @file TypeFieldLookup.cpp
 * Reverse field lookup used in the decompiler.
 */

#include "third-party/fmt/core.h"
#include "TypeSystem.h"

namespace {
// debug prints for the reverse lookup
bool debug_reverse_lookup = false;

/*!
 * Is the actual dereference compatible with the expected?
 */
bool deref_matches(const DerefInfo& expected,
                   const DerefKind& actual,
                   bool is_integer,
                   bool is_basic) {
  assert(expected.mem_deref);
  assert(expected.can_deref);
  if (actual.is_store || actual.size >= 8 || !is_integer) {
    // don't check sign extension
    return expected.load_size == actual.size;
  } else if (is_basic) {
    // this is kinda weird, but it seems like GOAL uses lw and lwu for loading basics.
    return expected.load_size == actual.size;
  } else {
    return expected.load_size == actual.size && expected.sign_extend == actual.sign_extend;
  }
}
}  // namespace

/*!
 * Convert a Token in a field path to a string for debugging.
 */
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
      return {};
  }
}

/*!
 * Main reverse lookup. Check the success field of the result to see if it was successful.
 * Will return the type of result as well as the path taken to get there.
 * The path can be arbitrarily long because we could be looking through nested inline structures.
 * The result is _always_ an actual dereference and there is no way for this to return "no deref".
 * The "offset" should always be the actual memory offset in the load instruction.
 * This is the "offset into memory" - "boxed offset"
 */
FieldReverseLookupOutput TypeSystem::reverse_field_lookup(
    const FieldReverseLookupInput& input) const {
  if (debug_reverse_lookup) {
    fmt::print("reverse_field_lookup on {} offset {} deref {} stride {}\n", input.base_type.print(),
               input.offset, input.deref.has_value(), input.stride);
  }
  FieldReverseLookupOutput result;
  result.success = try_reverse_lookup(input, &result.tokens, &result.addr_of, &result.result_type);
  // todo check for only one var lookup.
  return result;
}

/*!
 * Reverse lookup helper. Returns true if successful. It's okay to call this an have it fail.
 * Will set path/addr_of/result_type if successful.
 */
bool TypeSystem::try_reverse_lookup(const FieldReverseLookupInput& input,
                                    std::vector<FieldReverseLookupOutput::Token>* path,
                                    bool* addr_of,
                                    TypeSpec* result_type) const {
  if (debug_reverse_lookup) {
    fmt::print(" try_reverse_lookup on {} offset {} deref {} stride {}\n", input.base_type.print(),
               input.offset, input.deref.has_value(), input.stride);
  }

  auto base_input_type = input.base_type.base_type();
  if (base_input_type == "pointer") {
    return try_reverse_lookup_pointer(input, path, addr_of, result_type);
  } else if (base_input_type == "inline-array") {
    return try_reverse_lookup_inline_array(input, path, addr_of, result_type);
  } else if (base_input_type == "array" && input.base_type.has_single_arg()) {
    return try_reverse_lookup_array(input, path, addr_of, result_type);
  } else {
    return try_reverse_lookup_other(input, path, addr_of, result_type);
  }
  return false;
}

/*!
 * Handle a dereference of a pointer. This can be:
 * - just dereferencing a pointer
 * - accessing a variable element of a pointer-style array
 * - getting the address of a variable element of a pointer-style array
 * - accessing a constant element of a pointer-style array
 * - getting the address of a constant element of a pointer-style array
 */
bool TypeSystem::try_reverse_lookup_pointer(const FieldReverseLookupInput& input,
                                            std::vector<FieldReverseLookupOutput::Token>* path,
                                            bool* addr_of,
                                            TypeSpec* result_type) const {
  if (!input.base_type.has_single_arg()) {
    return false;
  }
  auto di = get_deref_info(input.base_type);
  bool is_integer = tc(TypeSpec("integer"), input.base_type.get_single_arg());
  bool is_basic = tc(TypeSpec("basic"), input.base_type.get_single_arg());
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
      if (deref_matches(di, input.deref.value(), is_integer, is_basic)) {
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
      if (!deref_matches(di, input.deref.value(), is_integer, is_basic)) {
        // this isn't the right type of dereference
        return false;
      }

      // always push back an index so we're never ambiguous.
      path->push_back(token);

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

/*!
 * Handle a dereference with an "array" type.
 * This has two cases:
 * - accessing a field of the array class, not including the array data. Like any other structure.
 * - accessing the data array part of the array class, very similar to the pointer-style array.
 */
bool TypeSystem::try_reverse_lookup_array(const FieldReverseLookupInput& input,
                                          std::vector<FieldReverseLookupOutput::Token>* path,
                                          bool* addr_of,
                                          TypeSpec* result_type) const {
  // type should be (array elt-type)
  if (!input.base_type.has_single_arg()) {
    return false;
  }

  if (input.offset < ARRAY_DATA_OFFSET) {
    // we are accessing a field in an array so we can treat this like any other structure.
    // this will add the basic offset, so we can pass in the input unchanged.
    return try_reverse_lookup_other(input, path, addr_of, result_type);
  }

  // this is the data type - (pointer elt-type). this is stored at an offset of ARRAY_DATA_OFFSET.
  auto array_data_type = make_pointer_typespec(input.base_type.get_single_arg());
  auto di = get_deref_info(array_data_type);
  bool is_integer = tc(TypeSpec("integer"), input.base_type.get_single_arg());
  bool is_basic = tc(TypeSpec("basic"), input.base_type.get_single_arg());
  assert(di.mem_deref);  // it's accessing a pointer.
  auto elt_type = di.result_type;
  if (input.stride) {
    if (input.offset != ARRAY_DATA_OFFSET) {
      // might be constant propagated other offsets here?
      return false;
    }

    // variable access to the array.
    if (input.stride != di.stride) {
      // mismatched array strides, fail!
      return false;
    }

    FieldReverseLookupOutput::Token token;
    token.kind = FieldReverseLookupOutput::Token::Kind::VAR_IDX;
    path->push_back(token);
    if (input.deref.has_value()) {
      if (deref_matches(di, input.deref.value(), is_integer, is_basic)) {
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
    int elt_idx = (input.offset - ARRAY_DATA_OFFSET) / di.stride;
    int offset_into_elt = (input.offset - ARRAY_DATA_OFFSET) - (elt_idx * di.stride);
    if (offset_into_elt) {
      // should line up correctly.
      return false;
    }

    FieldReverseLookupOutput::Token token;
    token.kind = FieldReverseLookupOutput::Token::Kind::CONSTANT_IDX;
    token.idx = elt_idx;
    // always put array index, even if it's zero.
    path->push_back(token);
    if (input.deref.has_value()) {
      if (!deref_matches(di, input.deref.value(), is_integer, is_basic)) {
        // this isn't the right type of dereference
        return false;
      }

      // access constant idx element of array
      *addr_of = false;
      *result_type = elt_type;
      return true;
    } else {
      // get address of constant idx element of array
      *addr_of = true;
      *result_type = make_pointer_typespec(elt_type);
      return true;
    }
  }
}

/*!
 * Handle a reverse deref of an inline-array.  This assumes that the array contains inlined
 * reference type objects.  It can handle
 * - get a reference object (variable idx)
 * - get something inside an object (variable idx)
 * - get a constant idx reference object (we pick this over just getting the array for idx = 0)
 * - get something inside a constant idx reference object
 */
bool TypeSystem::try_reverse_lookup_inline_array(const FieldReverseLookupInput& input,
                                                 std::vector<FieldReverseLookupOutput::Token>* path,
                                                 bool* addr_of,
                                                 TypeSpec* result_type) const {
  auto di = get_deref_info(input.base_type);
  assert(di.can_deref);
  assert(!di.mem_deref);  // if we make integer arrays allowed to be inline-array, this will break.

  if (input.stride) {
    if (input.stride != di.stride) {
      return false;
    }

    if (input.offset >= di.stride) {
      return false;
    }

    // variable lookup.
    FieldReverseLookupOutput::Token token;
    token.kind = FieldReverseLookupOutput::Token::Kind::VAR_IDX;
    path->push_back(token);

    if (input.offset == 0 && !input.deref.has_value()) {
      *addr_of = false;
      *result_type = di.result_type;
      return true;
    }

    FieldReverseLookupInput next_input;
    next_input.deref = input.deref;
    next_input.stride = 0;
    next_input.offset = input.offset;
    next_input.base_type = di.result_type;
    return try_reverse_lookup(next_input, path, addr_of, result_type);
  } else {
    // constant lookup, or accessing within the first one
    // which element we are in
    int elt_idx = input.offset / di.stride;
    // how many bytes into the element we look
    int offset_into_elt = input.offset - (elt_idx * di.stride);
    // the expected number of bytes into the element we would look to grab a ref to the elt.
    int expected_offset_into_elt = lookup_type(di.result_type)->get_offset();

    FieldReverseLookupOutput::Token token;
    token.kind = FieldReverseLookupOutput::Token::Kind::CONSTANT_IDX;
    token.idx = elt_idx;

    if (offset_into_elt == expected_offset_into_elt && !input.deref.has_value()) {
      // just get an element (possibly zero, and we want to include the 0 if so)
      // for the degenerate inline-array case, it seems more likely that we get the zeroth object
      // rather than the array?  Either way, this code should be compatible with both approaches.
      path->push_back(token);
      *addr_of = false;
      *result_type = di.result_type;
      return true;
    }

    // otherwise access within the element
    path->push_back(token);

    FieldReverseLookupInput next_input;
    next_input.deref = input.deref;
    next_input.stride = 0;
    // try_reverse_lookup expects "offset_into_field - boxed_offset"
    next_input.offset = offset_into_elt - expected_offset_into_elt;
    next_input.base_type = di.result_type;
    return try_reverse_lookup(next_input, path, addr_of, result_type);
  }
}

/*!
 * Handle a deref for fields of a structure.
 * - Access a field which requires mem deref.
 * - Get address of a field which requires mem deref.
 */
bool TypeSystem::try_reverse_lookup_other(const FieldReverseLookupInput& input,
                                          std::vector<FieldReverseLookupOutput::Token>* path,
                                          bool* addr_of,
                                          TypeSpec* result_type) const {
  auto type_info = lookup_type(input.base_type);
  auto structure_type = dynamic_cast<StructureType*>(type_info);
  if (!structure_type) {
    return false;
  }

  auto corrected_offset = input.offset + type_info->get_offset();
  // loop over fields. We may need to try multiple fields.
  for (auto& field : structure_type->fields()) {
    auto field_deref = lookup_field_info(type_info->get_name(), field.name());

    // how many bytes do we look at? In the case where we're just getting an address, we assume
    // one byte, so we'll always pass the size check.
    auto effective_load_size = 1;
    if (input.deref.has_value()) {
      effective_load_size = input.deref->size;
    }

    if (corrected_offset >= field.offset() &&
        (corrected_offset + effective_load_size <= field.offset() + get_size_in_type(field) ||
         field.is_dynamic())) {
      // the field size looks okay.
      int offset_into_field = corrected_offset - field.offset();

      FieldReverseLookupOutput::Token token;
      token.kind = FieldReverseLookupOutput::Token::Kind::FIELD;
      token.name = field.name();

      if (field_deref.needs_deref) {
        if (offset_into_field == 0) {
          if (input.deref.has_value()) {
            // needs deref, offset is 0, did a deref.
            // Check the deref is right...
            // (pointer <field-type>)
            TypeSpec loc_type = make_pointer_typespec(field_deref.type);
            auto di = get_deref_info(loc_type);
            bool is_integer = tc(TypeSpec("integer"), field_deref.type);
            bool is_basic = tc(TypeSpec("basic"), field_deref.type);
            if (!deref_matches(di, input.deref.value(), is_integer, is_basic)) {
              continue;  // try another field!
            }
            // it's a match, just access the field like normal!
            if (input.stride) {
              continue;
            }
            path->push_back(token);
            *addr_of = false;
            *result_type = field_deref.type;
            return true;
          } else {
            // needs a deref, offset is 0, didn't do a deref.
            // we're taking the address
            if (input.stride) {
              continue;
            }
            path->push_back(token);
            *addr_of = true;
            *result_type = make_pointer_typespec(field_deref.type);
            return true;
          }
        } else {
          if (input.deref.has_value()) {
            // needs deref, offset != 0, did a deref.
            // try a different field.
            continue;
          } else {
            // needs deref, offset != 0, didn't deref.
            // try a different field
            continue;
          }
        }
      } else {
        // no deref needed
        int expected_offset_into_field = 0;
        if (field.is_inline()) {
          expected_offset_into_field = lookup_type(field.type())->get_offset();
        }
        if (offset_into_field == expected_offset_into_field && !input.deref.has_value() &&
            !input.stride) {
          // get the inline field exactly
          path->push_back(token);
          *result_type = field_deref.type;
          *addr_of = false;
          return true;
        } else {
          FieldReverseLookupInput next_input;
          next_input.deref = input.deref;
          next_input.offset = offset_into_field - expected_offset_into_field;
          next_input.stride = input.stride;
          next_input.base_type = field_deref.type;
          auto old_path = *path;
          path->push_back(token);
          if (try_reverse_lookup(next_input, path, addr_of, result_type)) {
            return true;
          } else {
            *path = old_path;
            continue;
          }
        }
      }
    }
  }
  return false;
}