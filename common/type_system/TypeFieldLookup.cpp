/*!
 * @file TypeFieldLookup.cpp
 * Reverse field lookup used in the decompiler.
 */

#include <algorithm>

#include "TypeSystem.h"

#include "common/log/log.h"

#include "fmt/core.h"

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
  ASSERT(expected.mem_deref);
  ASSERT(expected.can_deref);
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
 * Convert the linked list to a vector of tokens.
 */
std::vector<FieldReverseLookupOutput::Token> ReverseLookupNode::to_vector() const {
  std::vector<const ReverseLookupNode*> nodes_reversed = {};
  std::vector<FieldReverseLookupOutput::Token> result;
  const ReverseLookupNode* node = this;
  while (node) {
    nodes_reversed.push_back(node);
    node = node->prev;
  }

  for (auto it = nodes_reversed.rbegin(); it != nodes_reversed.rend(); it++) {
    result.push_back((*it)->token);
  }
  return result;
}

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
      ASSERT(false);
      return {};
  }
}

bool FieldReverseLookupOutput::has_variable_token() const {
  for (const auto& tok : tokens) {
    if (tok.kind == Token::Kind::VAR_IDX) {
      return true;
    }
  }
  return false;
}

namespace {

void try_reverse_lookup(const FieldReverseLookupInput& input,
                        const TypeSystem& ts,
                        const ReverseLookupNode* parent,
                        FieldReverseMultiLookupOutput* output,
                        int max_count);

void try_reverse_lookup_other(const FieldReverseLookupInput& input,
                              const TypeSystem& ts,
                              const ReverseLookupNode* parent,
                              FieldReverseMultiLookupOutput* output,
                              int max_count);

std::vector<FieldReverseLookupOutput::Token> parent_to_vector(const ReverseLookupNode* parent) {
  if (!parent) {
    return {};
  }
  return parent->to_vector();
}

/*!
 * Handle a dereference of a pointer/boxed array. This can be:
 * - just dereferencing a pointer
 * - accessing a variable element of a pointer-style array
 * - getting the address of a variable element of a pointer-style array
 * - accessing a constant element of a pointer-style array
 * - getting the address of a constant element of a pointer-style array
 */
void try_reverse_lookup_array_like(const FieldReverseLookupInput& input,
                                   const TypeSystem& ts,
                                   const ReverseLookupNode* parent,
                                   FieldReverseMultiLookupOutput* output,
                                   int max_count,
                                   bool boxed_array) {
  if ((int)output->results.size() >= max_count) {
    return;
  }

  if (!input.base_type.has_single_arg()) {
    // not a typed pointer.
    return;
  }

  if (boxed_array && input.offset < ARRAY_DATA_OFFSET) {
    // we are accessing a field in an array so we can treat this like any other structure.
    // this will add the basic offset, so we can pass in the input unchanged.
    try_reverse_lookup_other(input, ts, parent, output, max_count);
    return;
  }

  auto array_data_type =
      boxed_array ? ts.make_pointer_typespec(input.base_type.get_single_arg()) : input.base_type;
  auto di = ts.get_deref_info(array_data_type);
  bool is_integer = ts.tc(TypeSpec("integer"), input.base_type.get_single_arg());
  bool is_basic = ts.tc(TypeSpec("basic"), input.base_type.get_single_arg());
  ASSERT(di.mem_deref);  // it's accessing a pointer.
  auto elt_type = di.result_type;

  if (input.stride) {
    // variable access to the array.
    // this is an array of values, nothing inline, so we must get an _exact_ match for this to work.
    if (input.stride != di.stride) {
      // mismatched array strides, fail!
      return;
    }

    if (input.offset != (boxed_array ? ARRAY_DATA_OFFSET : 0)) {
      // can't access within an element of a pointer array.
      // todo - this could be some sort of constant folding for the next operation.
      // for example, something like (&+ (&-> array-of-uint32s 3) 1)
      return;
    }

    // add this to our path.
    ReverseLookupNode variable_node;
    variable_node.prev = parent;
    variable_node.token.kind = FieldReverseLookupOutput::Token::Kind::VAR_IDX;

    if (input.deref.has_value()) {
      // input did a load or store, we need to check the size/signed/kind.
      if (deref_matches(di, input.deref.value(), is_integer, is_basic)) {
        // access element of array
        // success!
        output->results.emplace_back(false, elt_type, variable_node.to_vector());
        // no other way to access an array with the given type.
        return;
      } else {
        // this isn't the right type of dereference, no way to make this work.
        return;
      }
    } else {
      // get the address of a variable indexed element of a pointer-style array
      output->results.emplace_back(true, ts.make_pointer_typespec(elt_type),
                                   variable_node.to_vector());
      return;
    }
  } else {
    // either access array or just plain deref a pointer.
    int offset = input.offset - (boxed_array ? ARRAY_DATA_OFFSET : 0);
    int elt_idx = offset / di.stride;
    int offset_into_elt = offset - (elt_idx * di.stride);
    if (offset_into_elt) {
      // shouldn't have a weird offset.
      return;
    }

    ReverseLookupNode constant_node;
    constant_node.prev = parent;
    constant_node.token.kind = FieldReverseLookupOutput::Token::Kind::CONSTANT_IDX;
    constant_node.token.idx = elt_idx;
    if (input.deref.has_value()) {
      if (!deref_matches(di, input.deref.value(), is_integer, is_basic)) {
        // this isn't the right type of dereference
        return;
      }

      // always push back an index so we're never ambiguous.
      output->results.emplace_back(false, elt_type, constant_node.to_vector());
      return;
    } else {
      // we want (&-> arr 0)
      output->results.emplace_back(true, ts.make_pointer_typespec(elt_type),
                                   constant_node.to_vector());

      // also just return the array
      if (elt_idx == 0) {
        if (boxed_array) {
          auto vec = parent_to_vector(parent);
          FieldReverseLookupOutput::Token tok;
          tok.kind = FieldReverseLookupOutput::Token::Kind::FIELD;
          tok.field_score = 0.0;  // don't bother
          tok.name = "data";
          vec.push_back(tok);
          output->results.emplace_back(false, array_data_type, vec);
        } else {
          auto parent_vector = parent_to_vector(parent);
          if (!parent_vector.empty()) {
            output->results.emplace_back(false, input.base_type, parent_vector);
          }
        }
      }

      return;
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
 *
 * Note: for an inline array of basics, the offset should include the basic offset.
 */
void try_reverse_lookup_inline_array(const FieldReverseLookupInput& input,
                                     const TypeSystem& ts,
                                     const ReverseLookupNode* parent,
                                     FieldReverseMultiLookupOutput* output,
                                     int max_count) {
  if ((int)output->results.size() >= max_count) {
    return;
  }
  auto di = ts.get_deref_info(input.base_type);
  ASSERT(di.can_deref);
  ASSERT(!di.mem_deref);

  if (input.stride && input.stride == di.stride && input.offset < di.stride) {
    // variable lookup.
    ReverseLookupNode var_idx_node;
    var_idx_node.prev = parent;

    var_idx_node.token.kind = FieldReverseLookupOutput::Token::Kind::VAR_IDX;

    if (input.offset == 0 && !input.deref.has_value()) {
      // put the element first, to match the old behavior.
      output->results.emplace_back(false, di.result_type, var_idx_node.to_vector());
      // keep trying!
    }

    if ((int)output->results.size() >= max_count) {
      return;
    }

    FieldReverseLookupInput next_input;
    next_input.deref = input.deref;
    next_input.stride = 0;
    next_input.offset = input.offset;  // includes the offset.
    next_input.base_type = di.result_type;
    try_reverse_lookup(next_input, ts, &var_idx_node, output, max_count);
    return;
  }

  // constant lookup, or accessing within the first one
  // which element we are in
  int elt_idx = (ts.lookup_type(di.result_type)->get_offset() + input.offset) / di.stride;
  // how many bytes into the element we look (including offset)
  int offset_into_elt = input.offset - (elt_idx * di.stride);
  // the expected number of bytes into the element we would look to grab a ref to the elt.

  ReverseLookupNode const_idx_node;
  const_idx_node.prev = parent;
  const_idx_node.token.kind = FieldReverseLookupOutput::Token::Kind::CONSTANT_IDX;
  const_idx_node.token.idx = elt_idx;

  if (offset_into_elt == 0 && !input.deref.has_value() && !input.stride) {
    // just get an element (possibly zero, and we want to include the 0 if so)
    // for the degenerate inline-array case, it seems more likely that we get the zeroth object
    // rather than the array, so this goes before that case.
    output->results.emplace_back(false, di.result_type, const_idx_node.to_vector());
    if ((int)output->results.size() >= max_count) {
      return;
    }
    // keep trying more stuff.
  }

  // can we just return the array?
  if (offset_into_elt == 0 && !input.deref.has_value() && elt_idx == 0 && !input.stride) {
    auto parent_vec = parent_to_vector(parent);
    if (!parent_vec.empty()) {
      output->results.emplace_back(false, input.base_type, parent_to_vector(parent));
    }

    if ((int)output->results.size() >= max_count) {
      return;
    }
  }

  // otherwise access within the element

  FieldReverseLookupInput next_input;
  next_input.deref = input.deref;
  next_input.stride = input.stride;
  next_input.offset = offset_into_elt;
  next_input.base_type = di.result_type;
  try_reverse_lookup(next_input, ts, &const_idx_node, output, max_count);
}

/*!
 * Handle a deref for fields of a structure.
 * - Access a field which requires mem deref.
 * - Get address of a field which requires mem deref.
 */
void try_reverse_lookup_other(const FieldReverseLookupInput& input,
                              const TypeSystem& ts,
                              const ReverseLookupNode* parent,
                              FieldReverseMultiLookupOutput* output,
                              int max_count) {
  auto type_info = ts.lookup_type(input.base_type);
  auto structure_type = dynamic_cast<StructureType*>(type_info);
  if (!structure_type) {
    return;
  }

  auto corrected_offset = input.offset + type_info->get_offset();
  // loop over fields. We may need to try multiple fields.
  for (auto& field : structure_type->fields()) {
    // todo, remove this and replace with score.
    if (field.skip_in_decomp()) {
      continue;
    }

    if ((int)output->results.size() >= max_count) {
      return;
    }

    auto field_deref = ts.lookup_field_info(type_info->get_name(), field.name());

    // how many bytes do we look at? In the case where we're just getting an address, we assume
    // one byte, so we'll always pass the size check.
    auto effective_load_size = 1;
    if (input.deref.has_value()) {
      effective_load_size = input.deref->size;
    }

    if (corrected_offset >= field.offset() &&
        (corrected_offset + effective_load_size <= field.offset() + ts.get_size_in_type(field) ||
         field.is_dynamic())) {
      // the field size looks okay.
      int offset_into_field = corrected_offset - field.offset();

      FieldReverseLookupOutput::Token token;
      token.kind = FieldReverseLookupOutput::Token::Kind::FIELD;
      token.name = field.name();
      token.field_score = field.field_score();

      if (field_deref.needs_deref) {
        if (offset_into_field == 0) {
          if (input.deref.has_value()) {
            // needs deref, offset is 0, did a deref.
            // Check the deref is right...
            // (pointer <field-type>)
            TypeSpec loc_type = ts.make_pointer_typespec(field_deref.type);
            auto di = ts.get_deref_info(loc_type);
            bool is_integer = ts.tc(TypeSpec("integer"), field_deref.type);
            bool is_basic = ts.tc(TypeSpec("basic"), field_deref.type);
            if (!deref_matches(di, input.deref.value(), is_integer, is_basic)) {
              continue;  // try another field!
            }
            // it's a match, just access the field like normal!
            if (input.stride) {
              continue;
            }
            ReverseLookupNode node;
            node.prev = parent;
            node.token = token;
            output->results.emplace_back(false, field_deref.type, node.to_vector());
            continue;  // try more!
          } else {
            // needs a deref, offset is 0, didn't do a deref.
            // we're taking the address
            if (input.stride) {
              continue;
            }
            ReverseLookupNode node;
            node.prev = parent;
            node.token = token;
            output->results.emplace_back(true, ts.make_pointer_typespec(field_deref.type),
                                         node.to_vector());
            continue;  // try more!
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
          expected_offset_into_field = ts.lookup_type(field.type())->get_offset();
        }
        if (offset_into_field == expected_offset_into_field && !input.deref.has_value() &&
            !input.stride) {
          ReverseLookupNode node;
          node.prev = parent;
          node.token = token;
          output->results.emplace_back(false, field_deref.type, node.to_vector());
          continue;  // try more!
        } else {
          FieldReverseLookupInput next_input;
          next_input.deref = input.deref;
          next_input.offset = offset_into_field - expected_offset_into_field;
          next_input.stride = input.stride;
          next_input.base_type = field_deref.type;
          ReverseLookupNode node;
          node.prev = parent;
          node.token = token;
          try_reverse_lookup(next_input, ts, &node, output, max_count);
        }
      }
    }
  }
}

/*!
 * Reverse lookup helper. Returns true if successful. It's okay to call this an have it fail.
 * Will set path/addr_of/result_type if successful.
 */
void try_reverse_lookup(const FieldReverseLookupInput& input,
                        const TypeSystem& ts,
                        const ReverseLookupNode* parent,
                        FieldReverseMultiLookupOutput* output,
                        int max_count) {
  if (debug_reverse_lookup) {
    lg::debug(" try_reverse_lookup on {} offset {} deref {} stride {}", input.base_type.print(),
              input.offset, input.deref.has_value(), input.stride);
  }

  auto base_input_type = input.base_type.base_type();
  if (base_input_type == "pointer") {
    try_reverse_lookup_array_like(input, ts, parent, output, max_count, false);
  } else if (base_input_type == "inline-array") {
    return try_reverse_lookup_inline_array(input, ts, parent, output, max_count);
  } else if (base_input_type == "array" && input.base_type.has_single_arg()) {
    try_reverse_lookup_array_like(input, ts, parent, output, max_count, true);
  } else {
    return try_reverse_lookup_other(input, ts, parent, output, max_count);
  }
}
}  // namespace

/*!
 * Old single reverse lookup. Check the success field of the result to see if it was successful.
 * Will return the type of result as well as the path taken to get there.
 * The path can be arbitrarily long because we could be looking through nested inline structures.
 * The result is _always_ an actual dereference and there is no way for this to return "no deref".
 * The "offset" should always be the actual memory offset in the load instruction.
 * This is the "offset into memory" - "boxed offset"
 */
FieldReverseLookupOutput TypeSystem::reverse_field_lookup(
    const FieldReverseLookupInput& input) const {
  // just use the multi-lookup set to 1 and grab the first result.
  auto multi_result = reverse_field_multi_lookup(input, 100);

  /*
  if (multi_result.results.size() > 1) {
    lg::print("Multiple:\n");
    for (auto& result : multi_result.results) {
      lg::print("  [{}] [{}] ", result.total_score, result.result_type.print());
      for (auto& tok : result.tokens) {
        lg::print("{} ", tok.print());
      }
      lg::print("\n");
    }
    lg::print("\n\n\n");
  }
   */

  FieldReverseLookupOutput result;
  if (multi_result.success) {
    result = multi_result.results.at(0);
    result.success = true;
  } else {
    result.success = false;
  }
  return result;
}

FieldReverseMultiLookupOutput TypeSystem::reverse_field_multi_lookup(
    const FieldReverseLookupInput& input,
    int max_count) const {
  if (debug_reverse_lookup) {
    lg::debug("reverse_field_lookup on {} offset {} deref {} stride {}", input.base_type.print(),
              input.offset, input.deref.has_value(), input.stride);
  }

  FieldReverseMultiLookupOutput result;
  try_reverse_lookup(input, *this, nullptr, &result, max_count);
  if (!result.results.empty()) {
    result.success = true;
    for (auto& r : result.results) {
      // compute the score.
      r.total_score = 0;
      for (auto& tok : r.tokens) {
        r.total_score += tok.score();
      }
    }

    // use stable sort to make sure we break ties by being first in the order.
    std::stable_sort(result.results.begin(), result.results.end(),
                     [](const FieldReverseLookupOutput& a, const FieldReverseLookupOutput& b) {
                       return a.total_score > b.total_score;
                     });
  }
  return result;
}
