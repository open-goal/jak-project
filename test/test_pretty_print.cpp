#include "common/goos/PrettyPrinter.h"
#include "common/goos/PrettyPrinter2.h"
#include "common/goos/Reader.h"
#include "common/util/FileUtil.h"

#include "gtest/gtest.h"

#include "third-party/fmt/core.h"

using namespace goos;

namespace {
Object read(const std::string& str) {
  auto body = pretty_print::get_pretty_printer_reader().read_from_string(str).as_pair()->cdr;
  EXPECT_TRUE(body.as_pair()->cdr.is_empty_list());
  return body.as_pair()->car;
}

std::string pprint(const Object& o, int len = 80) {
  return pretty_print::to_string_v1(o, len);
}

// read then pretty print a string.
std::string ppr(const std::string& in, int len = 80) {
  return pprint(read(in), len);
}
}  // namespace

TEST(PrettyPrinter, Basics) {
  EXPECT_EQ(ppr("test"), "test");
  EXPECT_EQ(ppr("(l 12 asdf)"), "(l 12 asdf)");

  // force it to break
  EXPECT_EQ(ppr("(thing 12 asd asfd sdfjk)", 10), "(thing\n  12\n  asd\n  asfd\n  sdfjk\n  )");
}

TEST(PrettyPrinter, ReadAgain) {
  // first read the gcommon file
  auto gcommon_code = pretty_print::get_pretty_printer_reader().read_from_file(
      {"goal_src", "jak1", "kernel", "gcommon.gc"});
  // pretty print it
  auto printed_gcommon = pretty_print::to_string(gcommon_code);
  auto gcommon_code2 = pretty_print::get_pretty_printer_reader()
                           .read_from_string(printed_gcommon)
                           .as_pair()
                           ->cdr.as_pair()
                           ->car;
  auto printed_gcommon2 = pretty_print::to_string_v1(gcommon_code);
  EXPECT_TRUE(gcommon_code == gcommon_code2);
}

TEST(PrettyPrinter, ReadAgainVeryShortLines) {
  // first read the gcommon file
  auto gcommon_code = pretty_print::get_pretty_printer_reader().read_from_file(
      {"goal_src", "jak1", "kernel", "gcommon.gc"});
  // pretty print it but with a very short line length. This looks terrible but will hopefully
  // hit many of the cases for line breaking.
  auto printed_gcommon = pretty_print::to_string(gcommon_code, 80);
  auto gcommon_code2 = pretty_print::get_pretty_printer_reader()
                           .read_from_string(printed_gcommon)
                           .as_pair()
                           ->cdr.as_pair()
                           ->car;
  auto printed_gcommon2 = pretty_print::to_string_v1(gcommon_code);
  EXPECT_TRUE(gcommon_code == gcommon_code2);
}

TEST(PrettyPrinter, DefunNoArgs) {
  // wrong old printing
  std::string code =
      "(defun looping-code () (while #t (suspend)\n"
      "                       )\n"
      "  (the-as symbol #f)\n"
      "  )";

  auto obj = pretty_print::get_pretty_printer_reader()
                 .read_from_string(code)
                 .as_pair()
                 ->cdr.as_pair()
                 ->car;
  auto printed = pretty_print::to_string_v1(obj, 80);

  EXPECT_EQ(printed,
            "(defun looping-code ()\n"
            "  (while #t\n"
            "   (suspend)\n"
            "   )\n"
            "  (the-as symbol #f)\n"
            "  )");
}

TEST(PrettyPrinter2, Debugging) {
  // first read the gcommon file
  auto gcommon_code = pretty_print::get_pretty_printer_reader().read_from_file(
      {"goal_src", "jak1", "kernel", "gcommon.gc"});
  // pretty print it
  auto printed_gcommon = pretty_print::to_string(gcommon_code);
  auto gcommon_code2 = pretty_print::get_pretty_printer_reader()
                           .read_from_string(printed_gcommon)
                           .as_pair()
                           ->cdr.as_pair()
                           ->car;
  EXPECT_TRUE(gcommon_code == gcommon_code2);
}

namespace {
std::string pretty_print_v2(const std::string& str, int line_length = 110) {
  auto obj =
      pretty_print::get_pretty_printer_reader().read_from_string(str).as_pair()->cdr.as_pair()->car;
  return pretty_print::to_string(obj, line_length);
}
}  // namespace

TEST(PrettyPrinter2, Defun) {
  // checks that edefun is split up properly
  std::string code = "(defun identity ((obj object)) obj)";
  EXPECT_EQ(pretty_print_v2(code),
            R"((defun identity ((obj object))
  obj
  ))");
}

TEST(PrettyPrinter2, MultiLine) {
  // check that multiple lines are split correctly
  std::string code =
      "(defmethod inspect vec4s ((obj vec4s)) (format #t \"[~8x] ~A~%\" obj 'vec4s) (format #t "
      "\"~Tx: ~f~%\" (-> obj x)) (format #t \"~Ty: ~f~%\" (-> obj y)) (format #t \"~Tz: ~f~%\" (-> "
      "obj z)) (format #t \"~Tw: ~f~%\" (-> obj w)) obj )";
  EXPECT_EQ(pretty_print_v2(code),
            R"((defmethod inspect vec4s ((obj vec4s))
  (format #t "[~8x] ~A~%" obj 'vec4s)
  (format #t "~Tx: ~f~%" (-> obj x))
  (format #t "~Ty: ~f~%" (-> obj y))
  (format #t "~Tz: ~f~%" (-> obj z))
  (format #t "~Tw: ~f~%" (-> obj w))
  obj
  ))");
}

TEST(PrettyPrinter2, LetUntilIf) {
  std::string code =
      "(defun basic-type? ((obj basic) (parent-type type))  (let ((obj-type (-> obj type))        "
      "(end-type object)        )    (until (= obj-type end-type)      (if (= obj-type "
      "parent-type)          (return #t)          )      (set! obj-type (-> obj-type parent))      "
      ")    ) #f  )";

  // this checks that let defs are properly aligned, until is properly aligned (body only indented
  // by two), if indented properly (aligned with condition)
  EXPECT_EQ(pretty_print_v2(code),
            R"((defun basic-type? ((obj basic) (parent-type type))
  (let ((obj-type (-> obj type))
        (end-type object)
        )
    (until (= obj-type end-type)
      (if (= obj-type parent-type)
          (return #t)
          )
      (set! obj-type (-> obj-type parent))
      )
    )
  #f
  ))");
}

TEST(PrettyPrinter2, Overhang) {
  std::string code =
      "(defun nassoc ((item-name string) (alist object)) (while (not (or (null? alist) (let ((key "
      "(car (car alist)))) (if (pair? key) (nmember item-name key) (name= (the-as basic key) "
      "item-name) ) ) ) ) (set! alist (cdr alist)) ) (if (not (null? alist)) (car alist) ) )";

  // this case is tricky: you have several lists starting at the (while with their last elements
  // split. this makes sure that the close parens of these list are right.
  EXPECT_EQ(pretty_print_v2(code),
            R"((defun nassoc ((item-name string) (alist object))
  (while (not (or (null? alist) (let ((key (car (car alist))))
                                  (if (pair? key)
                                      (nmember item-name key)
                                      (name= (the-as basic key) item-name)
                                      )
                                  )
                  )
              )
    (set! alist (cdr alist))
    )
  (if (not (null? alist))
      (car alist)
      )
  ))");
}

TEST(PrettyPrint2, Cond) {
  // check that cond and its else get split.
  // note that for now we allow a short condition and body to be on the same line.
  std::string code =
      "(defmethod length pair ((obj pair)) (local-vars (result int)) (cond ((null? obj) (set! "
      "result 0)) (else (let ((iter (cdr obj))) (set! result 1) (while (and (not (null? iter)) "
      "(pair? iter)) (+! result 1) (set! iter (cdr iter)) ) ) ) ) result )";
  EXPECT_EQ(pretty_print_v2(code),
            R"((defmethod length pair ((obj pair))
  (local-vars (result int))
  (cond
    ((null? obj)
     (set! result 0)
     )
    (else
      (let ((iter (cdr obj)))
        (set! result 1)
        (while (and (not (null? iter)) (pair? iter))
          (+! result 1)
          (set! iter (cdr iter))
          )
        )
      )
    )
  result
  ))");
}

TEST(PrettyPrint2, ParenWayOutToTheRight) {
  std::string code =
      "(defmethod new inline-array-class ((allocation symbol) (type-to-make type) (size int)) (let "
      "((obj (object-new allocation type-to-make (the-as int (+ (-> type-to-make size) (* (the-as "
      "uint size) (-> type-to-make heap-base)))) ) ) ) (when (nonzero? obj) (set! (-> obj length) "
      "size) (set! (-> obj allocated-length) size)) obj ) )";

  // checks that the c0 stuff works right.
  EXPECT_EQ(
      pretty_print_v2(code),
      R"((defmethod new inline-array-class ((allocation symbol) (type-to-make type) (size int))
  (let ((obj (object-new
               allocation
               type-to-make
               (the-as int (+ (-> type-to-make size) (* (the-as uint size) (-> type-to-make heap-base))))
               )
             )
        )
    (when (nonzero? obj)
      (set! (-> obj length) size)
      (set! (-> obj allocated-length) size)
      )
    obj
    )
  ))");
}

TEST(PrettyPrint2, ParenWayOutToTheRight2) {
  std::string code =
      "(defmethod new inline-array-class ((allocation symbol) (type-to-make type) (size int)) (let "
      "((obj (object-new allocation type-to-make (the-as int (+ (-> type-to-make size) (* (the-as "
      "uint size) (-> type-to-make heap-base)))) ) ) ) obj ) )";

  // checks that the c0 stuff works right.
  EXPECT_EQ(
      pretty_print_v2(code),
      R"((defmethod new inline-array-class ((allocation symbol) (type-to-make type) (size int))
  (let ((obj (object-new
               allocation
               type-to-make
               (the-as int (+ (-> type-to-make size) (* (the-as uint size) (-> type-to-make heap-base))))
               )
             )
        )
    obj
    )
  ))");
}

TEST(PrettyPrint2, ImproperList) {
  std::string code = "( ( a .  b)  ( c .  d ) ( e  f . g) . #f )";
  EXPECT_EQ(pretty_print_v2(code), "((a . b) (c . d) (e f . g) . #f)");
}

TEST(PrettyPrint2, ImproperListMultiLine) {
  std::string code =
      "( ( asdfasfdasdf .  b)  ( casdfsadfasdf .  dsadfasfdsf ) ( esdfasdf  fdasfsadf . gdfasfd) . "
      "#f )";
  EXPECT_EQ(pretty_print_v2(code, 40),
            "((asdfasfdasdf . b)\n"
            "  (casdfsadfasdf . dsadfasfdsf)\n"
            "  (esdfasdf fdasfsadf . gdfasfd)\n"
            "  .\n"
            "  #f\n"
            "  )");
}

TEST(PrettyPrint2, BreakIfBug) {
  std::string code =
      "    (if (and (= (-> arg0 current-prt-color x) 0.0)"
      "               (= (-> arg0 current-prt-color y) 0.0)\n"
      "               (= (-> arg0 current-prt-color z) 0.0)\n"
      "               )\n"
      "        (update-mood-prt-color arg0)\n"
      "        )";
  EXPECT_EQ(pretty_print_v2(code, 100),
            "(if (and (= (-> arg0 current-prt-color x) 0.0)\n"
            "         (= (-> arg0 current-prt-color y) 0.0)\n"
            "         (= (-> arg0 current-prt-color z) 0.0)\n"
            "         )\n"
            "    (update-mood-prt-color arg0)\n"
            "    )");
}

TEST(PrettyPrint2, AnotherBug) {
  std::string code =
      "          (let ((f0-8 (* (fmin (vector-xz-length arg1) (* (vector-xz-length (-> s5-0 "
      "trans)) arg4))\n"
      "                          (-> *display* frames-per-second)\n"
      "                          )\n"
      "                       )\n"
      "                (t9-2 vector-xz-normalize!)\n"
      "                ))";
  EXPECT_EQ(
      pretty_print_v2(code, 100),
      "(let ((f0-8 (* (fmin (vector-xz-length arg1) (* (vector-xz-length (-> s5-0 trans)) arg4))\n"
      "               (-> *display* frames-per-second)\n"
      "               )\n"
      "            )\n"
      "      (t9-2 vector-xz-normalize!)\n"
      "      )\n"
      "  )");
}
