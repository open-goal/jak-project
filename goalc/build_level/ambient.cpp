#include "ambient.h"

#include "goalc/data_compiler/DataObjectGenerator.h"
/*
(deftype drawable-group (drawable)
  ((length  int16       :offset 6)
   (data    drawable 1  :offset-assert 32)
   )
  (:methods
    (new (symbol type int) _type_)
    )
  :flag-assert #x1200000024
  )
 (deftype drawable-tree (drawable-group)
  ()
  :flag-assert #x1200000024
  )
(deftype drawable-inline-array (drawable)
  ((length  int16          :offset 6) ;; this is kinda weird.
   )
  :method-count-assert 18
  :size-assert         #x20
  :flag-assert         #x1200000020
  )
*/

size_t DrawableTreeAmbient::add_to_object_file(DataObjectGenerator& gen, size_t ambient_array) {
  gen.align_to_basic();
  gen.add_type_tag("drawable-tree-ambient");
  size_t result = gen.current_offset_bytes();
  gen.add_word(1 << 16);  // 4, 6
  gen.add_word(0);        // 8
  gen.add_word(0);        // 12
  gen.add_word(0);        // 16
  gen.add_word(0);        // 20
  gen.add_word(0);        // 24
  gen.add_word(0);        // 28
  gen.link_word_to_byte(gen.add_word(0), ambient_array);
  return result;
}