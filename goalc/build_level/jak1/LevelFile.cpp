#include "LevelFile.h"

namespace jak1 {
static size_t ambient_arr_slot;

size_t DrawableTreeArray::add_to_object_file(DataObjectGenerator& gen) const {
  /*
   (deftype drawable-tree-array (drawable-group)
    ((trees drawable-tree 1 :offset 32 :score 100))
    :flag-assert #x1200000024
    )
    (deftype drawable-group (drawable)
      ((length  int16       :offset 6)
       (data    drawable 1  :offset-assert 32)
       )
      (:methods
        (new (symbol type int) _type_)
        )
      :flag-assert #x1200000024
      )
   */
  gen.align_to_basic();
  gen.add_type_tag("drawable-tree-array");
  size_t result = gen.current_offset_bytes();
  int num_trees = 0;
  num_trees += tfrags.size();
  num_trees += collides.size();
  num_trees += ambients.size();
  gen.add_word(num_trees << 16);
  gen.add_word(0);
  gen.add_word(0);

  gen.add_word(0);
  gen.add_word(0);
  gen.add_word(0);
  gen.add_word(0);

  // todo add trees...

  if (num_trees == 0) {
    gen.add_word(0);  // the one at the end.
  } else {
    int tree_word = (int)gen.current_offset_bytes() / 4;
    for (int i = 0; i < num_trees; i++) {
      gen.add_word(0);
    }

    for (auto& tfrag : tfrags) {
      // gen.set_word(tree_word++, tfrag.add_to_object_file(gen));
      gen.link_word_to_byte(tree_word++, tfrag.add_to_object_file(gen));
    }

    for (auto& collide : collides) {
      gen.link_word_to_byte(tree_word++, collide.add_to_object_file(gen));
    }

    for (auto& ambient : ambients) {
      gen.link_word_to_byte(tree_word++, ambient.add_to_object_file(gen, ambient_arr_slot));
    }
  }

  return result;
}

size_t generate_u32_array(const std::vector<u32>& array, DataObjectGenerator& gen) {
  gen.align(4);
  size_t result = gen.current_offset_bytes();
  for (auto& entry : array) {
    gen.add_word(entry);
  }
  return result;
}

std::vector<u8> LevelFile::save_object_file() const {
  DataObjectGenerator gen;
  gen.add_type_tag("bsp-header");

  // add blank space for the bsp-header
  while (gen.words() < 100) {
    gen.add_word(0);
  }

  //(info                   file-info                        :offset          4)
  auto file_info_slot = info.add_to_object_file(gen);
  gen.link_word_to_byte(1, file_info_slot);

  ambient_arr_slot = jak1::generate_inline_array_ambients(gen, ambients);

  //(bsphere                vector :inline                   :offset-assert  16)
  //(all-visible-list       (pointer uint16)                 :offset-assert  32)
  //(visible-list-length    int32                            :offset-assert  36)
  //(drawable-trees         drawable-tree-array              :offset-assert  40)
  gen.link_word_to_byte(40 / 4, drawable_trees.add_to_object_file(gen));
  //(pat                    pointer                          :offset-assert  44)
  //(pat-length             int32                            :offset-assert  48)
  //(texture-remap-table    (pointer uint64)                 :offset-assert  52)
  //(texture-remap-table-len int32                           :offset-assert  56)
  //(texture-ids            (pointer texture-id)             :offset-assert  60)
  //(texture-page-count     int32                            :offset-assert  64)
  //(unk-zero-0             basic                            :offset-assert  68)
  //(name                   symbol                           :offset-assert  72)
  gen.link_word_to_symbol(name, 72 / 4);
  //(nickname               symbol                           :offset-assert  76)
  gen.link_word_to_symbol(nickname, 76 / 4);
  //(vis-info               level-vis-info                8  :offset-assert  80)
  //(actors                 drawable-inline-array-actor      :offset-assert 112)
  gen.link_word_to_byte(112 / 4, generate_inline_array_actors(gen, actors));
  //(cameras                (array entity-camera)            :offset-assert 116)
  //(nodes                  (inline-array bsp-node)          :offset-assert 120)
  //(level                  level                            :offset-assert 124)
  //(current-leaf-idx       uint16                           :offset-assert 128)
  //(unk-data-2             uint16                        9  :offset-assert 130)
  //(boxes                  box8s-array                      :offset-assert 148)
  //(current-bsp-back-flags uint32                           :offset-assert 152)
  //(ambients               drawable-inline-array-ambient    :offset-assert 156)
  gen.link_word_to_byte(156 / 4, ambient_arr_slot);
  //(unk-data-4             float                            :offset-assert 160)
  //(unk-data-5             float                            :offset-assert 164)
  //(adgifs                 adgif-shader-array               :offset-assert 168)
  //(actor-birth-order      (pointer uint32)                 :offset-assert 172)
  gen.link_word_to_byte(172 / 4, generate_u32_array(actor_birth_order, gen));
  //(split-box-indices      (pointer uint16)                 :offset-assert 176)
  //(unk-data-8             uint32                        55 :offset-assert 180)

  return gen.generate_v2();
}
}  // namespace jak1