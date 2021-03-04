#include <common/link_types.h>
#include "common/util/FileUtil.h"
#include "gtest/gtest.h"
#include "common/log/log.h"
#include "decompiler/Disasm/OpcodeInfo.h"
#include "decompiler/config.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "goalc/compiler/Compiler.h"

namespace {
// the object files to test
const std::unordered_set<std::string> g_object_files_to_decompile = {"gcommon"};

// the object files to check against a reference in test/decompiler/reference
const std::unordered_set<std::string> g_object_files_to_check_against_reference = {
    "gcommon"  // NOTE: this file needs work, but adding it for now just to test the framework.
};

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
    // pskernel
    "kernel-check-hardwired-addresses",  // ps2 ee kernel debug hook
    "kernel-read-function",              // ps2 ee kernel debug hook
    "kernel-write-function",             // ps2 ee kernel debug hook
    "kernel-copy-function"               // ps2 ee kernel debug hook
};

const std::unordered_set<std::string> skip_in_compiling = {
    // these functions are not implemented by the compiler in OpenGOAL, but are in GOAL.
    "abs", "ash", "min", "max", "lognor", "(method 3 vec4s)", "(method 2 vec4s)"};

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
      if (!func.ir2.env.has_type_analysis()) {
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

    auto reference = file_util::read_text_file(file_util::get_file_path(
        {"test", "decompiler", "reference", fmt::format("{}_REF.gc", file)}));

    strip_trailing_newlines(reference);
    strip_trailing_newlines(src);

    EXPECT_EQ(reference, src);
  }
}

TEST_F(OfflineDecompilation, Compile) {
  Compiler compiler;

  for (auto& file : g_object_files_to_check_against_reference) {
    auto& obj_l = db->obj_files_by_name.at(file);
    ASSERT_EQ(obj_l.size(), 1);

    std::string src = db->ir2_final_out(obj_l.at(0), skip_in_compiling);

    compiler.run_full_compiler_on_string_no_save(src);
  }
}