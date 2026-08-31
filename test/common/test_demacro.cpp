#include "common/demacro/demacro.h"

#include "gtest/gtest.h"

TEST(Demacro, CapturesAndSequences) {
  const auto rules = demacro::parse_rules(R"RULES(
    {
      "rules": [
        {
          "name": "two-sets",
          "match": [
            "(set! (-> $dst x) (-> $src x))",
            "(set! (-> $dst y) (-> $src y))"
          ],
          "rewrite": "(copy-xy! $dst $src)"
        }
      ]
    }
  )RULES");
  const std::string source = R"((defun test ((a foo) (b foo))
  (set! (-> a x) (-> b x))
  (set! (-> a y) (-> b y))
  a)
)";
  const auto result = demacro::rewrite(source, rules);
  EXPECT_EQ(result.rewrite_count(), 1);
  EXPECT_EQ(result.source, R"((defun test ((a foo) (b foo))
  (copy-xy! a b)
  a)
)");
}

TEST(Demacro, RestCapturesHandleMergedLets) {
  const auto rules = demacro::parse_rules(R"RULES(
    {
      "rules": [
        {
          "name": "merged-let",
          "match": "(let ($*before ($temp $value) $*after) $*body-before (use $temp) $*body-after)",
          "rewrite": "(let ($*before $*after) $*body-before (use-macro $value) $*body-after)"
        }
      ]
    }
  )RULES");
  const auto result = demacro::rewrite(
      "(let ((keep value) (temp (compute)) (later other)) (first keep) (use temp) (last later))",
      rules);
  EXPECT_EQ(result.rewrite_count(), 1);
  EXPECT_EQ(result.source,
            "(let ((keep value) (later other)) (first keep) (use-macro (compute)) (last later))");
}

TEST(Demacro, PreservesCommentsInsideReplacementRange) {
  const auto rules = demacro::parse_rules(R"RULES(
    {
      "rules": [
        {
          "name": "two-sets",
          "match": ["(set! $dst 1)", "(set! $dst 2)"],
          "rewrite": "(set-twice! $dst)"
        }
      ]
    }
  )RULES");
  const std::string source = R"((begin
  (set! value 1)
  ;; Keep this explanation.
  (set! value 2))
)";
  const auto result = demacro::rewrite(source, rules);
  EXPECT_EQ(result.rewrite_count(), 1);
  EXPECT_EQ(result.source, R"((begin
  ;; Keep this explanation.
  (set-twice! value))
)");
}

TEST(Demacro, RepeatedCapturesMustAgree) {
  const auto rules = demacro::parse_rules(R"RULES(
    {
      "rules": [
        {
          "name": "two-sets",
          "match": ["(set! $dst 1)", "(set! $dst 2)"],
          "rewrite": "(set-twice! $dst)"
        }
      ]
    }
  )RULES");
  const auto result = demacro::rewrite("(begin (set! a 1) (set! b 2))", rules);
  EXPECT_EQ(result.rewrite_count(), 0);
}

TEST(Demacro, ExpandsPatternTables) {
  const auto rules = demacro::parse_rules(R"RULES(
    {
      "tables": {
        "kind": [
          {"value": "0", "symbol": "first"},
          {"value": "1", "symbol": "second"}
        ]
      },
      "rules": [
        {
          "name": "kind-{{symbol}}",
          "for_each": "kind",
          "match": "(expanded-kind {{value}})",
          "rewrite": "(kind {{symbol}})"
        }
      ]
    }
  )RULES");
  const auto result = demacro::rewrite("(begin (expanded-kind 0) (expanded-kind 1))", rules);
  EXPECT_EQ(result.rewrite_count(), 2);
  EXPECT_EQ(result.source, "(begin (kind first) (kind second))");
}

TEST(Demacro, Jak1PreservesMemUsageNameSemantics) {
  const auto rules =
      demacro::load_rules(file_util::get_file_path({"decompiler/config/jak1/demacro.jsonc"}));
  const std::string source = R"((begin
  (set! (-> usage length) (max 1 (-> usage length)))
  (set! (-> usage data 0 name) "drawable-group")
  (+! (-> usage data 0 count) 2)
  (let ((literal-bytes 32))
    (+! (-> usage data 0 used) literal-bytes)
    (+! (-> usage data 0 total) (logand -16 (+ literal-bytes 15))))
  (set! (-> other length) (max 1 (-> other length)))
  (set! (-> other data 0 name) (symbol->string 'drawable-group))
  (+! (-> other data 0 count) 3)
  (let ((symbol-bytes 48))
    (+! (-> other data 0 used) symbol-bytes)
    (+! (-> other data 0 total) (logand -16 (+ symbol-bytes 15)))))
)";
  const auto result = demacro::rewrite(source, rules);
  EXPECT_EQ(result.rewrite_count(), 2);
  EXPECT_EQ(result.source, R"((begin
  (mem-usage-add! usage drawable-group 2 32)
  (mem-usage-add-symbol! other drawable-group 3 48))
)");
}

TEST(Demacro, Jak1RecognizesPerfStatAndDmaMemUsageMacros) {
  const auto rules =
      demacro::load_rules(file_util::get_file_path({"decompiler/config/jak1/demacro.jsonc"}));
  const std::string source = R"((begin
  (let* ((stat (-> *perf-stats* data 1))
         (ctrl (-> stat ctrl)))
    (+! (-> stat count) 1)
    (b! (zero? ctrl) reset-done :delay (nop!))
    (.mtc0 Perf 0)
    (.sync.l)
    (.sync.p)
    (.mtpc pcr0 0)
    (.mtpc pcr1 0)
    (.sync.l)
    (.sync.p)
    (.mtc0 Perf ctrl))
  (.sync.l)
  (.sync.p)
  (label reset-done)
  0
  (let ((stat (-> *perf-stats* data 1)))
    (b! (zero? (-> stat ctrl)) read-done :delay (nop!))
    (.mtc0 Perf 0)
    (.sync.l)
    (.sync.p)
    (.mfpc counter0 pcr0)
    (+! (-> stat accum0) counter0)
    (.mfpc counter1 pcr1)
    (+! (-> stat accum1) counter1))
  (label read-done)
  0
  (let ((usage *dma-mem-usage*))
    (when (nonzero? usage)
      (set! (-> usage length) (max 87 (-> usage length)))
      (set! (-> usage data 86 name) "pris-generic")
      (+! (-> usage data 86 count) 1)
      (+! (-> usage data 86 used) bytes)
      (set! (-> usage data 86 total) (-> usage data 86 used))))
  (let ((line (-> dma-buf base)))
    (.sync.l)
    (.cache dxwbin line 0)
    (.sync.l)
    (.cache dxwbin line 1))
  (.sync.l)
  0)
)";
  const auto result = demacro::rewrite(source, rules);
  EXPECT_EQ(result.rewrite_count(), 4);
  EXPECT_EQ(result.source, R"((begin
  (reset! (-> *perf-stats* data 1))
  (read! (-> *perf-stats* data 1))
  (dma-mem-usage-add! pris-generic 1 bytes)
  (invalidate-cache-line (-> dma-buf base)))
)");
}

TEST(Demacro, Jak1RecognizesCachedEngineIteration) {
  const auto rules =
      demacro::load_rules(file_util::get_file_path({"decompiler/config/jak1/demacro.jsonc"}));
  const std::string source = R"((let ((node (-> *collide-player-list* alive-list next0)))
  *collide-player-list*
  (let ((next-node (-> node next0)))
    (while (!= node (-> *collide-player-list* alive-list-end))
      ;; Body comments must survive the collapsed traversal.
      (visit (-> (the-as connection node) param1))
      (set! node next-node)
      *collide-player-list*
      (set! next-node (-> next-node next0)))))
)";
  const auto result = demacro::rewrite(source, rules);
  EXPECT_EQ(result.rewrite_count(), 1);
  EXPECT_EQ(result.source, R"(;; Body comments must survive the collapsed traversal.
(iterate-engine-connections (node *collide-player-list*) (visit (-> (the-as connection node) param1)))
)");
}

TEST(Demacro, Jak1RecognizesMergedCachedEngineIterations) {
  const auto rules =
      demacro::load_rules(file_util::get_file_path({"decompiler/config/jak1/demacro.jsonc"}));
  const std::string source = R"((begin
  (let ((node (-> first-engine alive-list next0)))
    first-engine
    (let ((next-node (-> node next0)))
      (while (!= node (-> first-engine alive-list-end))
        (visit-first node)
        (set! node next-node)
        first-engine
        (set! next-node (-> next-node next0)))
      (set! node (-> second-engine alive-list next0))
      second-engine
      (set! next-node (-> node next0))
      (while (!= node (-> second-engine alive-list-end))
        (visit-second node)
        (set! node next-node)
        second-engine
        (set! next-node (-> next-node next0)))))
  (set! node (-> assigned-engine alive-list next0))
  assigned-engine
  (set! next-node (-> node next0))
  (while (!= node (-> assigned-engine alive-list-end))
    (visit-assigned node)
    (set! node next-node)
    assigned-engine
    (set! next-node (-> next-node next0)))
  (let ((keep value)
        (node (-> bound-engine alive-list next0)))
    bound-engine
    (let ((next-node (-> node next0)))
      (while (!= node (-> bound-engine alive-list-end))
        (visit-bound node)
        (set! node next-node)
        bound-engine
        (set! next-node (-> next-node next0))))))
)";
  const auto result = demacro::rewrite(source, rules);
  EXPECT_EQ(result.rewrite_count(), 4);
  EXPECT_EQ(result.source, R"((begin
  (iterate-engine-connections (node first-engine) (visit-first node))
  (iterate-engine-connections (node second-engine) (visit-second node))
  (iterate-engine-connections (node assigned-engine) (visit-assigned node))
  (let ((keep value)) (iterate-engine-connections (node bound-engine) (visit-bound node))))
)");
}

TEST(Demacro, Jak1RecognizesDmaBucketConstruction) {
  const auto rules =
      demacro::load_rules(file_util::get_file_path({"decompiler/config/jak1/demacro.jsonc"}));
  const std::string source = R"((let* ((buf (-> (current-frame) debug-buf))
       (start (-> buf base)))
  ;; Keep the packet-building body.
  (emit-packet buf)
  (let ((edge (-> buf base)))
    (let ((packet (the-as dma-packet (-> buf base))))
      (set! (-> packet dma) (new 'static 'dma-tag :id (dma-tag-id next)))
      (set! (-> packet vif0) (new 'static 'vif-tag))
      (set! (-> packet vif1) (new 'static 'vif-tag))
      (set! (-> buf base) (&+ (the-as pointer packet) 16)))
    (dma-bucket-insert-tag buckets bucket start (the-as (pointer dma-tag) edge))))
)";
  const auto result = demacro::rewrite(source, rules);
  EXPECT_EQ(result.rewrite_count(), 1);
  EXPECT_EQ(result.source, R"(;; Keep the packet-building body.
(with-dma-buffer-add-bucket ((buf (-> (current-frame) debug-buf)) bucket) :bucket-group buckets (emit-packet buf))
)");
}

TEST(Demacro, Jak1RecognizesDmaCntPacketsWithExplicitQwc) {
  const auto rules =
      demacro::load_rules(file_util::get_file_path({"decompiler/config/jak1/demacro.jsonc"}));
  const std::string source = R"((begin
  (let* ((dma-state dma-buf)
         (mask-packet (the-as object (-> dma-state base))))
    (set! (-> (the-as dma-packet mask-packet) dma)
          (new 'static 'dma-tag :qwc #x3 :id (dma-tag-id cnt)))
    (set! (-> (the-as dma-packet mask-packet) vif0)
          (new 'static 'vif-tag :cmd (vif-cmd stmask)))
    (set! (-> (the-as dma-packet mask-packet) vif1) (new 'static 'vif-tag))
    (set! (-> dma-state base) (&+ (the-as pointer mask-packet) 16)))
  (let* ((dma-state dma-buf)
         (packet (the-as dma-packet (-> dma-state base))))
    (set! (-> packet dma) (new 'static 'dma-tag :qwc qwc :id (dma-tag-id cnt)))
    (set! (-> packet vif0) vif0)
    (set! (-> packet vif1) vif1)
    (set! (-> dma-state base) (&+ (the-as pointer packet) 16))))
)";
  const auto result = demacro::rewrite(source, rules);
  EXPECT_EQ(result.rewrite_count(), 2);
  EXPECT_EQ(result.source, R"((begin
  (dma-buffer-add-cnt-vif2 dma-buf #x3 (new 'static 'vif-tag :cmd (vif-cmd stmask)) (new 'static 'vif-tag))
  (dma-buffer-add-cnt-vif2 dma-buf qwc vif0 vif1))
)");
}

TEST(Demacro, Jak1RecognizesDmaNextPackets) {
  const auto rules =
      demacro::load_rules(file_util::get_file_path({"decompiler/config/jak1/demacro.jsonc"}));
  const std::string source = R"((begin
  (let ((packet (the-as object (-> dma-buf base))))
    (set! (-> (the-as dma-packet packet) dma)
          (new 'static 'dma-tag :id (dma-tag-id next) :addr next-address))
    (set! (-> (the-as dma-packet packet) vif0) vif0)
    (set! (-> (the-as dma-packet packet) vif1) vif1)
    (set! (-> dma-buf base) (&+ (the-as pointer packet) 16)))
  (let ((packet (the-as dma-packet (-> dma-buf base))))
    (set! (-> packet dma) (new 'static 'dma-tag :id (dma-tag-id next)))
    (set! (-> packet vif0) (new 'static 'vif-tag))
    (set! (-> packet vif1) (new 'static 'vif-tag))
    (set! (-> dma-buf base) (&+ (the-as pointer packet) 16))))
)";
  const auto result = demacro::rewrite(source, rules);
  EXPECT_EQ(result.rewrite_count(), 2);
  EXPECT_EQ(result.source, R"((begin
  (dma-buffer-add-next-vif2 dma-buf 0 next-address vif0 vif1)
  (dma-buffer-add-next-vif2 dma-buf 0 0 (new 'static 'vif-tag) (new 'static 'vif-tag)))
)");
}

TEST(Demacro, Jak1RecognizesInlinedFontEnumSetters) {
  const auto rules =
      demacro::load_rules(file_util::get_file_path({"decompiler/config/jak1/demacro.jsonc"}));
  const std::string source = R"((begin
  (set! (-> font flags) (font-flags shadow kerning large))
  ;; Keep the color choice with its reconstructed call.
  (let ((selected-font font))
    (set! (-> selected-font color) (font-color progress-selected)))
  (let ((option-font font))
    (set! (-> option-font color)
          (if selected? (font-color progress-selected) (font-color default))))
  (set! (-> water flags) (water-flag active)))
)";
  const auto result = demacro::rewrite(source, rules);
  EXPECT_EQ(result.rewrite_count(), 3);
  EXPECT_EQ(result.source, R"((begin
  (set-flags! font (font-flags shadow kerning large))
  ;; Keep the color choice with its reconstructed call.
  (set-color! font (font-color progress-selected))
  (set-color! font (if selected? (font-color progress-selected) (font-color default)))
  (set! (-> water flags) (water-flag active)))
)");
}
