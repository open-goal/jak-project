#include <common/link_types.h>
#include "common/util/FileUtil.h"
#include "gtest/gtest.h"
#include "common/log/log.h"
#include "decompiler/Disasm/OpcodeInfo.h"
#include "decompiler/config.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "goalc/compiler/Compiler.h"
#include "common/util/Timer.h"

namespace {
// the object files to test
const std::unordered_set<std::string> g_object_files_to_decompile = {
    "gcommon", "gstring-h", "gkernel-h", "gkernel",
    /*"pskernel",*/ "gstring", "dgo-h", "gstate", "types-h", "vu1-macros", "math", "vector-h",
    "bounding-box-h", "matrix-h", "quaternion-h", "euler-h", "transform-h", "geometry-h",
    "trigonometry-h", /* transformq-h */ "matrix", "transform", "quaternion",
    "euler", /* geometry, trigonometry, */
    "gsound-h", "timer-h", "timer", "vif-h", "dma-h", "video-h", "vu1-user-h", "dma", "dma-buffer",
    "dma-bucket", "dma-disasm",
    /* gap */
    "bounding-box",
    /* gap */
    "sync-info-h", "sync-info"};

// the object files to check against a reference in test/decompiler/reference
const std::vector<std::string> g_object_files_to_check_against_reference = {
    "gcommon",  // NOTE: this file needs work, but adding it for now just to test the framework.
    "gstring-h", "gkernel-h", "gkernel", "gstring", "dgo-h", "gstate", "types-h", "vu1-macros",
    "math", "vector-h", "bounding-box-h", "matrix-h", "quaternion-h", "euler-h", "transform-h",
    "geometry-h", "trigonometry-h",
    /* transformq-h, */
    "matrix", "transform", "quaternion", "euler", /* geometry, trigonometry */
    "gsound-h", "timer-h", /* timer, */ "vif-h", "dma-h", "video-h", "vu1-user-h", "dma",
    "dma-buffer", "dma-bucket", "dma-disasm",
    /* gap */ "bounding-box",
    /* gap */
    "sync-info-h", "sync-info"};

// the functions we expect the decompiler to skip
const std::unordered_set<std::string> expected_skip_in_decompiler = {
    // gcommon
    "quad-copy!",  // asm mempcy
    // gkernel
    "set-to-run-bootstrap",     // kernel context switch
    "throw",                    // manually sets fp/t9.
    "throw-dispatch",           // restore context
    "(method 0 catch-frame)",   // save context
    "(method 11 cpu-thread)",   // kernel -> user context switch
    "(method 10 cpu-thread)",   // user -> kernel context switch
    "reset-and-call",           // kernel -> user
    "return-from-thread-dead",  // kernel -> user
    "return-from-thread",       // kernel -> user
    "return-from-exception",    // ps2 exception -> ps2 user
    "run-function-in-process",  // temp while stack vars aren't supported.
    // pskernel
    "kernel-check-hardwired-addresses",  // ps2 ee kernel debug hook
    "kernel-read-function",              // ps2 ee kernel debug hook
    "kernel-write-function",             // ps2 ee kernel debug hook
    "kernel-copy-function",              // ps2 ee kernel debug hook
    // math
    "rand-uint31-gen",  // weird and terrible random generator
    // bounding-box
    "(method 9 bounding-box)",   // handwritten asm loop
    "(method 14 bounding-box)",  // handwritten asm loop
    // matrix
    "(method 9 matrix)",  // handwritten asm loop
    "matrix-axis-sin-cos!", "matrix-axis-sin-cos-vu!",
    // dma-h
    "dma-count-until-done",  // dma asm loop
    "dma-sync-with-count", "dma-send-no-scratch", "dma-sync-fast",
    // dma
    "symlink2", "symlink3", "dma-sync-hang",  // handwritten asm
    // sync-info
    "(method 15 sync-info)",         // needs *res-static-buf*
    "(method 15 sync-info-eased)",   // needs *res-static-buf*
    "(method 15 sync-info-paused)",  // needs *res-static-buf*
};

const std::unordered_set<std::string> skip_in_compiling = {
    /// GCOMMON
    // these functions are not implemented by the compiler in OpenGOAL, but are in GOAL.
    "abs", "ash", "min", "max", "lognor",
    // weird PS2 specific debug registers:
    "breakpoint-range-set!",
    // int128 fancy stuff.
    "(method 3 vec4s)", "(method 2 vec4s)",
    // does weird stuff with the type system.
    "print", "printl", "inspect",
    // inline assembly
    "valid?",

    /// GKERNEL-H
    // bitfields, possibly inline assembly
    "(method 2 handle)",

    /// GKERNEL
    // asm
    "(method 10 process)",

    /// GSTATE
    "enter-state",          // stack pointer asm
    "send-event-function",  // pp asm (eventually we should make this work)

    /// MATH
    "rand-vu-init", "rand-vu", "rand-vu-nostep",  // random hardware
    "log2",                                       // weird tricky int-as-float stuff

    /// VECTOR-H
    "(method 3 vector)",  // this function appears twice, which confuses the compiler.
    "vector-dot",         // fpu acc
    "vector4-dot",        // fpu acc

    // QUATERNION
    "matrix-with-scale->quaternion",  // fpu acc
    "quaternion-delta-y",             // fpu acc

    "(method 3 profile-frame)",  // double definition.

    // dma-disasm
    "disasm-dma-list",

    // sync-info
    "(method 15 sync-info)",         // needs display stuff first
    "(method 15 sync-info-eased)",   // needs display stuff first
    "(method 15 sync-info-paused)",  // needs display stuff first
};

// default location for the data. It can be changed with a command line argument.
std::string g_iso_data_path = "";

}  // namespace
int main(int argc, char** argv) {
  lg::initialize();

  // look for an argument that's not a gtest option
  bool got_arg = false;
  for (int i = 1; i < argc; i++) {
    auto arg = std::string(argv[i]);
    if (arg.length() > 2 && arg[0] == '-' && arg[1] == '-') {
      continue;
    }
    if (got_arg) {
      printf("You can only specify a single path for ISO data\n");
      return 1;
    }
    g_iso_data_path = arg;
    lg::warn("Using path {} for iso_data", g_iso_data_path);
    got_arg = true;
  }

  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

class OfflineDecompilation : public ::testing::Test {
 protected:
  static std::unique_ptr<decompiler::ObjectFileDB> db;

  static void SetUpTestCase() {
    // global setup
    file_util::init_crc();
    decompiler::init_opcode_info();
    decompiler::set_config(
        file_util::get_file_path({"decompiler", "config", "jak1_ntsc_black_label.jsonc"}));

    decompiler::get_config().allowed_objects = g_object_files_to_decompile;

    std::vector<std::string> dgos = {"CGO/KERNEL.CGO", "CGO/ENGINE.CGO"};
    std::vector<std::string> dgo_paths;
    if (g_iso_data_path.empty()) {
      for (auto& x : dgos) {
        dgo_paths.push_back(file_util::get_file_path({"iso_data", x}));
      }
    } else {
      for (auto& x : dgos) {
        dgo_paths.push_back(file_util::combine_path(g_iso_data_path, x));
      }
    }

    db = std::make_unique<decompiler::ObjectFileDB>(
        dgo_paths, decompiler::get_config().obj_file_name_map_file, std::vector<std::string>{},
        std::vector<std::string>{});

    // basic processing to find functions/data/disassembly
    db->process_link_data();
    db->find_code();
    db->process_labels();

    // fancy decompilation.
    db->analyze_functions_ir2({});
  }

  static void TearDownTestCase() { db.reset(); }
};

std::unique_ptr<decompiler::ObjectFileDB> OfflineDecompilation::db;

/*!
 * Check that the most basic disassembly into files/functions/instructions has succeeded.
 */
TEST_F(OfflineDecompilation, CheckBasicDecode) {
  int obj_count = 0;
  db->for_each_obj([&](decompiler::ObjectFileData& obj) {
    obj_count++;
    auto& stats = obj.linked_data.stats;
    // make sure we decoded all instructions
    EXPECT_EQ(stats.code_bytes / 4, stats.decoded_ops);
    // make sure all FP uses are properly recognized
    EXPECT_EQ(stats.n_fp_reg_use, stats.n_fp_reg_use_resolved);
  });

  EXPECT_EQ(obj_count, decompiler::get_config().allowed_objects.size());
}

/*!
 * Not a super great test, but check that we find functions, methods, and logins.
 * This is a test of ir2_top_level_pass, which isn't tested as part of the normal decompiler tests.
 */
TEST_F(OfflineDecompilation, FunctionDetect) {
  int function_count = 0;  // global functions
  int method_count = 0;    // methods
  int login_count = 0;     // top-level logins
  int unknown_count = 0;   // unknown functions, like anonymous lambdas

  db->for_each_function(
      [&](decompiler::Function& func, int segment_id, decompiler::ObjectFileData&) {
        if (segment_id == TOP_LEVEL_SEGMENT) {
          EXPECT_EQ(func.guessed_name.kind, decompiler::FunctionName::FunctionKind::TOP_LEVEL_INIT);
        } else {
          EXPECT_NE(func.guessed_name.kind, decompiler::FunctionName::FunctionKind::TOP_LEVEL_INIT);
        }
        switch (func.guessed_name.kind) {
          case decompiler::FunctionName::FunctionKind::GLOBAL:
            function_count++;
            break;
          case decompiler::FunctionName::FunctionKind::METHOD:
            method_count++;
            break;
          case decompiler::FunctionName::FunctionKind::TOP_LEVEL_INIT:
            login_count++;
            break;
          case decompiler::FunctionName::FunctionKind::UNIDENTIFIED:
            unknown_count++;
            break;
          default:
            assert(false);
        }
      });

  // one login per object file
  EXPECT_EQ(decompiler::get_config().allowed_objects.size(), login_count);

  // not many lambdas.
  EXPECT_TRUE(unknown_count < 10);
}

TEST_F(OfflineDecompilation, AsmFunction) {
  int failed_count = 0;
  db->for_each_function([&](decompiler::Function& func, int, decompiler::ObjectFileData&) {
    if (func.suspected_asm) {
      if (expected_skip_in_decompiler.find(func.guessed_name.to_string()) ==
          expected_skip_in_decompiler.end()) {
        lg::error("Function {} was marked as asm, but wasn't expected.",
                  func.guessed_name.to_string());
        failed_count++;
      }
    }
  });
  EXPECT_EQ(failed_count, 0);
}

/*!
 * Test that all functions pass CFG build stage.
 */
TEST_F(OfflineDecompilation, CfgBuild) {
  int failed_count = 0;
  db->for_each_function([&](decompiler::Function& func, int, decompiler::ObjectFileData&) {
    if (!func.suspected_asm) {
      if (!func.cfg || !func.cfg->is_fully_resolved()) {
        lg::error("Function {} failed cfg", func.guessed_name.to_string());
        failed_count++;
      }
    }
  });

  EXPECT_EQ(failed_count, 0);
}

/*!
 * Test that all functions pass the atomic op construction stage
 */
TEST_F(OfflineDecompilation, AtomicOp) {
  int failed_count = 0;
  db->for_each_function([&](decompiler::Function& func, int, decompiler::ObjectFileData&) {
    if (!func.suspected_asm) {
      if (!func.ir2.atomic_ops || !func.ir2.atomic_ops_succeeded) {
        lg::error("Function {} failed atomic ops", func.guessed_name.to_string());
        failed_count++;
      }
    }
  });

  EXPECT_EQ(failed_count, 0);
}

/*!
 * Test that all functions pass the type analysis stage
 */
TEST_F(OfflineDecompilation, TypeAnalysis) {
  int failed_count = 0;
  db->for_each_function([&](decompiler::Function& func, int, decompiler::ObjectFileData&) {
    if (!func.suspected_asm) {
      if (!func.ir2.env.has_type_analysis() || !func.ir2.env.types_succeeded) {
        lg::error("Function {} failed types", func.guessed_name.to_string());
        failed_count++;
      }
    }
  });

  EXPECT_EQ(failed_count, 0);
}

TEST_F(OfflineDecompilation, RegisterUse) {
  int failed_count = 0;
  db->for_each_function([&](decompiler::Function& func, int, decompiler::ObjectFileData&) {
    if (!func.suspected_asm) {
      if (!func.ir2.env.has_reg_use()) {
        lg::error("Function {} failed reg use", func.guessed_name.to_string());
        failed_count++;
      }
    }
  });

  EXPECT_EQ(failed_count, 0);
}

TEST_F(OfflineDecompilation, VariableSSA) {
  int failed_count = 0;
  db->for_each_function([&](decompiler::Function& func, int, decompiler::ObjectFileData&) {
    if (!func.suspected_asm) {
      if (!func.ir2.env.has_local_vars()) {
        lg::error("Function {} failed ssa", func.guessed_name.to_string());
        failed_count++;
      }
    }
  });

  EXPECT_EQ(failed_count, 0);
}

TEST_F(OfflineDecompilation, Structuring) {
  int failed_count = 0;
  db->for_each_function([&](decompiler::Function& func, int, decompiler::ObjectFileData&) {
    if (!func.suspected_asm) {
      if (!func.ir2.top_form) {
        lg::error("Function {} failed structuring", func.guessed_name.to_string());
        failed_count++;
      }
    }
  });

  EXPECT_EQ(failed_count, 0);
}

TEST_F(OfflineDecompilation, Expressions) {
  int failed_count = 0;
  db->for_each_function([&](decompiler::Function& func, int, decompiler::ObjectFileData&) {
    if (!func.suspected_asm) {
      if (!func.ir2.expressions_succeeded) {
        lg::error("Function {} failed expressions", func.guessed_name.to_string());
        failed_count++;
      }
    }
  });

  EXPECT_EQ(failed_count, 0);
}

namespace {
void strip_trailing_newlines(std::string& in) {
  while (!in.empty() && in.back() == '\n') {
    in.pop_back();
  }
}
}  // namespace

TEST_F(OfflineDecompilation, Reference) {
  for (auto& file : g_object_files_to_check_against_reference) {
    auto& obj_l = db->obj_files_by_name.at(file);
    ASSERT_EQ(obj_l.size(), 1);

    std::string src = db->ir2_final_out(obj_l.at(0));

    /*     if (file == "gstring") {
           fmt::print("{}\n", src);
         }*/

    auto reference = file_util::read_text_file(file_util::get_file_path(
        {"test", "decompiler", "reference", fmt::format("{}_REF.gc", file)}));

    strip_trailing_newlines(reference);
    strip_trailing_newlines(src);

    EXPECT_EQ(reference, src);
  }
}

namespace {
int line_count(const std::string& str) {
  int result = 0;
  for (auto& c : str) {
    if (c == '\n') {
      result++;
    }
  }
  return result;
}
}  // namespace

TEST_F(OfflineDecompilation, Compile) {
  Compiler compiler;

  compiler.run_front_end_on_string(file_util::read_text_file(file_util::get_file_path(
      {"test", "decompiler", "reference", "all_forward_declarations.gc"})));

  Timer timer;
  int total_lines = 0;
  for (auto& file : g_object_files_to_check_against_reference) {
    auto& obj_l = db->obj_files_by_name.at(file);
    ASSERT_EQ(obj_l.size(), 1);

    std::string src = db->ir2_final_out(obj_l.at(0), skip_in_compiling);
    total_lines += line_count(src);

    compiler.run_full_compiler_on_string_no_save(src);
  }
  auto time = timer.getSeconds();
  lg::info("Total Lines Compiled: {}. Lines/second: {:.1f}\n", total_lines,
           (float)total_lines / time);
}
