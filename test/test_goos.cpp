/*!
 * @file test_goos.cpp
 * Tests for the GOOS macro language.
 */

#include "common/goos/Interpreter.h"

#include "gtest/gtest.h"

using namespace goos;

namespace {
// helper to evaluate a string as a goos expression.
std::string e(Interpreter& interp, const std::string& in) {
  return interp.eval(interp.reader.read_from_string(in), interp.global_environment.as_env_ptr())
      .print();
}
}  // namespace

TEST(GoosBuiltins, Begin) {
  Interpreter i;
  // begin returns the last thing in the list
  EXPECT_EQ(e(i, "(begin 1 2)"), "2");
  // begin with nothing returns the empty list
  EXPECT_EQ(e(i, "(begin)"), "()");

  e(i, "(begin (define x 123) 3)");
  EXPECT_EQ(e(i, "x"), "123");

  i.disable_printfs();
  for (auto x : {"(begin :test 1)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, Read) {
  Interpreter i;
  // test that we can read a string into the reader at runtime.
  EXPECT_EQ(e(i, "(read \"1\")"), "(top-level 1)");

  i.disable_printfs();
  // check bad expressions.
  for (auto x :
       {"(read 1)", "(read)", R"((read "a" "b"))", "(read :a b \"a\")", "(read \"(((((\")"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, ReadFile) {
  Interpreter i;
  // check that we can read a file.
  EXPECT_EQ(e(i, "(read-file \"test/test_data/test_reader_file0.gc\")"), "(top-level (1 2 3 4))");

  i.disable_printfs();
  for (auto x : {"(read-file 1)", "(read-file)", "(read-file \"goal/test/not_a_real_file.gc\")"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, LoadFile) {
  Interpreter i;
  // check that we can read and execute a file.
  e(i, "(load-file \"test/test_data/test_goos_file0.gs\")");
  EXPECT_EQ(e(i, "x"), "23");
  i.disable_printfs();
  for (auto x : {"(load-file 1)", "(load-file)", "(load-file \"goal/test/not_a_real_file.gc\")"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, PrintAndInspect) {
  Interpreter i;
  i.disable_printfs();
  // check that we can print/inspect (doesn't check result, just sees that it doesn't crash)
  EXPECT_EQ(e(i, "(print 10)"), "()");
  EXPECT_EQ(e(i, "(inspect 10)"), "()");
  for (auto x : {"(print 1 2)", "(print)", "(print 'a :a b)", "(print :a b)", "(inspect 1 2)",
                 "(inspect)", "(inspect 'a :a b)", "(inspect :a b)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

namespace {
// checks if two things are equal by running them through the GOOS eq? form.
bool eq(Interpreter& i, const std::string& a, const std::string& b) {
  return e(i, "(eq? " + a + " " + " " + b + ")") == "#t";
}
}  // namespace

TEST(GoosBuiltins, Equality) {
  Interpreter i;

  // STRING
  EXPECT_TRUE(eq(i, "\"asdf\"", "\"asdf\""));
  EXPECT_FALSE(eq(i, "\"asdgf\"", "\"asdf\""));

  // INTEGER
  EXPECT_TRUE(eq(i, "123", "0123"));
  EXPECT_FALSE(eq(i, "123", "10123"));

  // FLOAT
  EXPECT_TRUE(eq(i, "1.23", "01.23"));
  EXPECT_FALSE(eq(i, "1.23", "10.123"));

  // CHAR
  EXPECT_TRUE(eq(i, "#\\a", "#\\a"));
  EXPECT_FALSE(eq(i, "#\\b", "#\\a"));

  // SYMBOL
  EXPECT_TRUE(eq(i, "'a", "'a"));
  EXPECT_FALSE(eq(i, "'a", "'b"));

  // ENV
  EXPECT_TRUE(eq(i, "*global-env*", "*global-env*"));
  EXPECT_FALSE(eq(i, "*global-env*", "*goal-env*"));

  // LAMBDA
  e(i, "(desfun test-fun-1 () 2)");
  e(i, "(desfun test-fun-2 () 3)");
  e(i, "(define test-fun-3 test-fun-1)");

  EXPECT_TRUE(eq(i, "test-fun-1", "test-fun-3"));
  EXPECT_FALSE(eq(i, "test-fun-1", "test-fun-2"));

  // MACRO
  e(i, "(defsmacro test-mac-1 () 2)");
  e(i, "(defsmacro test-mac-2 () 3)");
  e(i, "(define test-mac-3 test-mac-1)");

  EXPECT_TRUE(eq(i, "test-mac-1", "test-mac-3"));
  EXPECT_FALSE(eq(i, "test-mac-1", "test-mac-2"));

  // EMPTY LIST
  EXPECT_TRUE(eq(i, "'()", "'()"));

  // PAIR
  EXPECT_TRUE(eq(i, "'(1 2 3)", "'(1 2 3)"));
  EXPECT_FALSE(eq(i, "'(1 2 3)", "'(1 2 4)"));

  // ARRAY
  EXPECT_TRUE(eq(i, "'#(1 2 3)", "'#(1 2 3)"));
  EXPECT_FALSE(eq(i, "'#(1 2 3)", "'#(1 2 4)"));
  EXPECT_FALSE(eq(i, "'#(1 2 3)", "'#(1 2 3 4)"));
  EXPECT_FALSE(eq(i, "'#(1 2 3 4 5)", "'#(1 2 3 4)"));
  i.disable_printfs();
  for (auto x : {"(eq?)", "(eq? 1)", "(eq? 1 2 3)", "(eq? 1 2 :keyword 2)", "(eq? 1 :keyword 2)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, Addition) {
  Interpreter i;
  // single element adding
  EXPECT_EQ(e(i, "(+ 1)"), "1");
  EXPECT_EQ(e(i, "(+ 1.)"), "1.0");

  // two element adding
  EXPECT_EQ(e(i, "(+ 1 2)"), "3");
  EXPECT_EQ(e(i, "(+ 1.1 2.2)"), "3.3");

  // mixed
  EXPECT_EQ(e(i, "(+ 1 1.1)"), "2");
  EXPECT_EQ(e(i, "(+ 1.1 1)"), "2.1");

  // many, and check rounding happens at the right time
  EXPECT_EQ(e(i, "(+ 1 1.4 1.4 1.4 1.4)"), "5");

  i.disable_printfs();
  for (auto x : {"(+)", "(+ 'a)", "(+ #\\a)", "(+ 1 :test 2)", "(+ 1 2 :test 3)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, Multiplication) {
  Interpreter i;
  // single element adding
  EXPECT_EQ(e(i, "(* 2)"), "2");
  EXPECT_EQ(e(i, "(* 2.)"), "2.0");

  // two element adding
  EXPECT_EQ(e(i, "(* 3 2)"), "6");
  EXPECT_EQ(e(i, "(* 1.1 2.2)"), "2.42");

  // mixed
  EXPECT_EQ(e(i, "(* 1 1.1)"), "1");
  EXPECT_EQ(e(i, "(* 1.1 1)"), "1.1");

  // many, and check rounding happens at the right time
  EXPECT_EQ(e(i, "(* 3 1.4 1.4 1.4 1.4)"), "3");

  i.disable_printfs();
  for (auto x : {"(*)", "(* 'a)", "(* #\\a)", "(* 1 :test 2)", "(* 1 2 :test 3)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, Subtraction) {
  Interpreter i;
  // single element adding
  EXPECT_EQ(e(i, "(- 2)"), "-2");
  EXPECT_EQ(e(i, "(- 2.)"), "-2.0");

  // two element adding
  EXPECT_EQ(e(i, "(- 3 2)"), "1");
  EXPECT_EQ(e(i, "(- 1.1 2.2)"), "-1.1");

  // mixed
  EXPECT_EQ(e(i, "(- 1 1.1)"), "0");
  EXPECT_EQ(e(i, "(- 1.1 1)"), "0.1");

  // many, and check rounding happens at the right time
  EXPECT_EQ(e(i, "(- 3 1.4 1.4 1.4 1.4)"), "-1");

  i.disable_printfs();
  for (auto x : {"(-)", "(- 'a)", "(- #\\a)", "(- 1 :test 2)", "(- 1 2 :test 3)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, Division) {
  Interpreter i;

  // two element adding
  EXPECT_EQ(e(i, "(/ 16 2)"), "8");
  EXPECT_EQ(e(i, "(/ 9. 2.)"), "4.5");

  // mixed
  EXPECT_EQ(e(i, "(/ 3 2.)"), "1");
  EXPECT_EQ(e(i, "(/ 3. 2)"), "1.5");

  i.disable_printfs();
  for (auto x : {"(/ 1)", "(/ 1.0)", "(/)", "(/ 'a)", "(/ #\\a)", "(/ 1 :test 2)",
                 "(/ 1 2 :test 3)", "(/ 1 2 3)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, NumberEquals) {
  Interpreter i;
  EXPECT_EQ(e(i, "(= 16 2)"), "#f");
  EXPECT_EQ(e(i, "(= 2 2)"), "#t");

  EXPECT_EQ(e(i, "(= 16.3 2.4)"), "#f");
  EXPECT_EQ(e(i, "(= 2.2 2.2)"), "#t");

  EXPECT_EQ(e(i, "(= 2.0 2)"), "#t");
  EXPECT_EQ(e(i, "(= 2 2.2)"), "#t");
  EXPECT_EQ(e(i, "(= 2.2 2)"), "#f");
  EXPECT_EQ(e(i, "(= 2 3.2)"), "#f");

  EXPECT_EQ(e(i, "(= 2 2 2 2 2)"), "#t");
  EXPECT_EQ(e(i, "(= 2 2 2 2.2 2.3)"), "#t");
  EXPECT_EQ(e(i, "(= 2 2 2 2.2 3.0)"), "#f");

  i.disable_printfs();
  for (auto x : {"(= 1)", "(=)", "(= a 1)", "(= 'a 1)", "(= 1 2 :test 3)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, NumberLessThan) {
  Interpreter i;
  EXPECT_EQ(e(i, "(< 16 2)"), "#f");
  EXPECT_EQ(e(i, "(< 2 2)"), "#f");
  EXPECT_EQ(e(i, "(< 1 2)"), "#t");

  EXPECT_EQ(e(i, "(< 16.8 16.2)"), "#f");
  EXPECT_EQ(e(i, "(< 2.2 2.2)"), "#f");
  EXPECT_EQ(e(i, "(< 1.9 2.1)"), "#t");

  EXPECT_EQ(e(i, "(< 1.1 2)"), "#t");
  EXPECT_EQ(e(i, "(< 1 2.1)"), "#t");
  EXPECT_EQ(e(i, "(< 2.2 2)"), "#f");
  EXPECT_EQ(e(i, "(< 2 2.2)"), "#f");

  i.disable_printfs();
  for (auto x : {"(< 1)", "(<)", "(< a 1)", "(< 'a 1)", "(< 1 2 :test 3)", "(< 1 2 3)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, NumberLessThanEqual) {
  Interpreter i;
  EXPECT_EQ(e(i, "(<= 16 2)"), "#f");
  EXPECT_EQ(e(i, "(<= 2 2)"), "#t");
  EXPECT_EQ(e(i, "(<= 1 2)"), "#t");

  EXPECT_EQ(e(i, "(<= 16.8 16.2)"), "#f");
  EXPECT_EQ(e(i, "(<= 2.2 2.2)"), "#t");
  EXPECT_EQ(e(i, "(<= 1.9 2.1)"), "#t");

  EXPECT_EQ(e(i, "(<= 1.1 2)"), "#t");
  EXPECT_EQ(e(i, "(<= 1 2.1)"), "#t");
  EXPECT_EQ(e(i, "(<= 2.2 2)"), "#f");
  EXPECT_EQ(e(i, "(<= 2 2.2)"), "#t");

  i.disable_printfs();
  for (auto x : {"(<= 1)", "(<=)", "(<= a 1)", "(<= 'a 1)", "(<= 1 2 :test 3)", "(<= 1 2 3)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, NumberGreaterThan) {
  Interpreter i;
  EXPECT_EQ(e(i, "(> 16 2)"), "#t");
  EXPECT_EQ(e(i, "(> 2 2)"), "#f");
  EXPECT_EQ(e(i, "(> 1 2)"), "#f");

  EXPECT_EQ(e(i, "(> 16.8 16.2)"), "#t");
  EXPECT_EQ(e(i, "(> 2.2 2.2)"), "#f");
  EXPECT_EQ(e(i, "(> 1.9 2.1)"), "#f");

  EXPECT_EQ(e(i, "(> 1.1 2)"), "#f");
  EXPECT_EQ(e(i, "(> 1 2.1)"), "#f");
  EXPECT_EQ(e(i, "(> 2.2 2)"), "#t");
  EXPECT_EQ(e(i, "(> 2 2.2)"), "#f");

  i.disable_printfs();
  for (auto x : {"(> 1)", "(>)", "(> a 1)", "(> 'a 1)", "(> 1 2 :test 3)", "(> 1 2 3)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, NumberGreaterThanEqual) {
  Interpreter i;
  EXPECT_EQ(e(i, "(>= 16 2)"), "#t");
  EXPECT_EQ(e(i, "(>= 2 2)"), "#t");
  EXPECT_EQ(e(i, "(>= 1 2)"), "#f");

  EXPECT_EQ(e(i, "(>= 16.8 16.2)"), "#t");
  EXPECT_EQ(e(i, "(>= 2.2 2.2)"), "#t");
  EXPECT_EQ(e(i, "(>= 1.9 2.1)"), "#f");

  EXPECT_EQ(e(i, "(>= 1.1 2)"), "#f");
  EXPECT_EQ(e(i, "(>= 1 2.1)"), "#f");
  EXPECT_EQ(e(i, "(>= 2.2 2)"), "#t");
  EXPECT_EQ(e(i, "(>= 2 2.2)"), "#t");

  i.disable_printfs();
  for (auto x : {"(>= 1)", "(>=)", "(>= a 1)", "(>= 'a 1)", "(>= 1 2 :test 3)", "(>= 1 2 3)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, Eval) {
  Interpreter i;
  EXPECT_EQ(e(i, "(eval '(+ 1 2))"), "3");

  i.disable_printfs();
  for (auto x : {"(eval bad-to-evaluate-me)", "(eval)", "(eval 1 2)", "(eval :test 3)",
                 "(eval 1 :test 3)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, CarCdr) {
  Interpreter i;
  EXPECT_EQ(e(i, "(car '(3 . 4))"), "3");
  EXPECT_EQ(e(i, "(cdr '(3 . 4))"), "4");
  i.disable_printfs();
  for (auto x : {"(car)", "(car '(1 . 2) '(3 . 4))", "(cdr)", "(cdr '(1 . 2) '(3 . 4))"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, SetCarCdr) {
  Interpreter i;
  e(i, "(define x (cons 1 2))");
  EXPECT_EQ(e(i, "(set-car! x (+ 1 3))"), "(4 . 2)");
  EXPECT_EQ(e(i, "x"), "(4 . 2)");
  EXPECT_EQ(e(i, "(set-cdr! x (+ 2 3))"), "(4 . 5)");
  EXPECT_EQ(e(i, "x"), "(4 . 5)");
  i.disable_printfs();
  for (auto x : {"(set-car!)", "(set-car! '(1 . 2))", "(set-cdr!)", "(set-cdr! '(1 . 2))"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, GenSym) {
  Interpreter i;
  EXPECT_EQ(e(i, "(eq? (gensym) (gensym))"), "#f");
  i.disable_printfs();
  for (auto x : {"(gensym 1)", "(gensym :a 2)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, Cons) {
  Interpreter i;
  EXPECT_EQ(e(i, "(cons 'a 2)"), "(a . 2)");
  EXPECT_EQ(e(i, "(cons 'a (cons 2 (cons 3 '())))"), "(a 2 3)");
  i.disable_printfs();
  for (auto x : {"(cons 1)", "(cons)", "(const 1 2 3)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, Null) {
  Interpreter i;
  EXPECT_EQ(e(i, "(null? 1)"), "#f");
  EXPECT_EQ(e(i, "(null? '())"), "#t");
  i.disable_printfs();
  for (auto x : {"(null? 1 2)", "(null?)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, Type) {
  Interpreter i;
  EXPECT_EQ(e(i, "(type? 'empty-list '())"), "#t");
  EXPECT_EQ(e(i, "(type? 'empty-list 1)"), "#f");

  EXPECT_EQ(e(i, "(type? 'integer 2)"), "#t");
  EXPECT_EQ(e(i, "(type? 'integer '())"), "#f");

  EXPECT_EQ(e(i, "(type? 'float 2.)"), "#t");
  EXPECT_EQ(e(i, "(type? 'float '())"), "#f");

  EXPECT_EQ(e(i, "(type? 'char #\\c)"), "#t");
  EXPECT_EQ(e(i, "(type? 'char '())"), "#f");

  EXPECT_EQ(e(i, "(type? 'symbol 'a)"), "#t");
  EXPECT_EQ(e(i, "(type? 'symbol '())"), "#f");

  EXPECT_EQ(e(i, "(type? 'string \"test\")"), "#t");
  EXPECT_EQ(e(i, "(type? 'string '())"), "#f");

  EXPECT_EQ(e(i, "(type? 'pair '(1 . 2))"), "#t");
  EXPECT_EQ(e(i, "(type? 'pair '())"), "#f");

  EXPECT_EQ(e(i, "(type? 'array '#(1 2))"), "#t");
  EXPECT_EQ(e(i, "(type? 'array '())"), "#f");

  EXPECT_EQ(e(i, "(type? 'lambda (lambda () 1))"), "#t");
  EXPECT_EQ(e(i, "(type? 'lambda '())"), "#f");

  EXPECT_EQ(e(i, "(type? 'macro (macro () 1))"), "#t");
  EXPECT_EQ(e(i, "(type? 'macro '())"), "#f");

  EXPECT_EQ(e(i, "(type? 'environment *global-env*)"), "#t");
  EXPECT_EQ(e(i, "(type? 'environment '())"), "#f");
}

/*!
 * Confirm that the GOOS Library is loaded automatically on interpreter start.
 */
TEST(GoosEval, GoosLibLoaded) {
  Interpreter i;
  Object loaded;
  EXPECT_TRUE(i.get_global_variable_by_name("__goos-lib-loaded__", &loaded));
  EXPECT_EQ(loaded.print(), "#t");
}

/*!
 * Check that integers are evaluated.
 */
TEST(GoosEval, EvalSelfEvaluating) {
  Interpreter i;
  EXPECT_EQ(e(i, "010"), "10");
  EXPECT_EQ(e(i, "-010"), "-10");
  EXPECT_EQ(e(i, "\"test\""), "\"test\"");
  EXPECT_EQ(e(i, "1.2"), "1.2");  // this depends on how we decide to print floats
  EXPECT_EQ(e(i, "#\\a"), "#\\a");
  EXPECT_EQ(e(i, "#\\\\n"), "#\\\\n");

  i.disable_printfs();
  EXPECT_ANY_THROW(e(i, "#\\\\a"));
}

TEST(GoosEval, FloatEvalAndPrinting) {
  Interpreter i;
  EXPECT_EQ(e(i, "0.9999979734420776"), "0.999998");
  EXPECT_EQ(e(i, "0.999998"), e(i, "0.9999979734420776"));
  EXPECT_EQ(e(i, "1."), "1.0");
  EXPECT_EQ(e(i, ".03"), "0.03");
  EXPECT_EQ(e(i, "0.02999999932944774627685546875"), e(i, "0.03"));
  EXPECT_EQ(e(i, "0.5883"), e(i, "0.5882999897"));
}

/*!
 * Check named "keyword" arguments
 */
TEST(GoosEval, KeywordArgs) {
  Interpreter i;
  e(i, "(desfun test (a b &key c &key d) c)");
  EXPECT_EQ(e(i, "(test 1 2 :c 3 :d 4)"), "3");
  EXPECT_EQ(e(i, "(test 1 2 :d 3 :c 4)"), "4");

  e(i, "(desfun test (a b &key c &key d) (+ b d))");
  EXPECT_EQ(e(i, "(test 1 2 :c 3 :d 4)"), "6");
  EXPECT_EQ(e(i, "(test 1 2 :d 3 :c 4)"), "5");
  EXPECT_EQ(e(i, "(test 1 :d 3 2 :c 4)"), "5");
  EXPECT_EQ(e(i, "(test :d 3 1 2 :c 4)"), "5");

  i.disable_printfs();
  for (const auto& x : {"(test 1 2 :c 3 :d)", "(test 1 2 :c 3)", "(test 1 2 :c 3 :d 4 :e 3)",
                        "(test 1 2 :c 3 :d 4 :c 4)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosEval, KeywordArgsWithDefault) {
  Interpreter i;
  e(i, "(desfun test (a b &key (c 3) &key d) c)");

  EXPECT_EQ(e(i, "(test 1 2 :c 7 :d 4)"), "7");
  EXPECT_EQ(e(i, "(test 1 2 :d 4)"), "3");

  e(i, "(desfun test (a b &key (c 3) &key d) c)");
  i.disable_printfs();
  for (const auto& x :
       {"(test 1 2 :c 3 :d)", "(test 1 2 :c :d 3)", "(test 1 2 :d 3 :c )", "(test 1 2 :c 3)",
        "(test 1 2 :c 3 :d 4 :e 3)", "(test 1 2 :c 3 :d 4 :c 4)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosIntegrated, Begin) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
(define *test-value* (begin
		       (define x 10)
#|
asdfasdfasdfa
|#
		       (define y 20)
		       (set! x y)
		       x
; y
		       )
  )
)"),
            "20");
}

TEST(GoosIntegrated, Lambda1) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
(define x 10)

(define f (lambda (x) ((lambda () x))))

(define *test-actual* (f 20))
)"),
            "20");
}

TEST(GoosIntegrated, Lambda2) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
(define make-returner (lambda (thing-to-return)
                              (lambda () thing-to-return)))

(define my-func (make-returner "beans"))
(define fake-func (make-returner "cheese"))
(fake-func)
(define *test-actual* (fake-func))
(set! *test-actual* (my-func))
)"),
            "\"beans\"");
}

TEST(GoosIntegrated, Cond) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
(define s1 (cond
	     (#f a)
	     ((+ 2 3) (+ 3 4))
	     (#t 3333)
	     (#t this-cannot-be-evaluated)
	     )
  )


(define s2 (cond (2)))

(define s3 (cond (#f a)
                 (#t (define s4 10)
                     (+ 1 1)
		     )
		 )
  )

(define *test-actual* (+ s1 s2 s3 s4))
)"),
            "21");
}

TEST(GoosIntegrated, CountingChange) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
(define count-change
    (lambda (amount)
      (cc amount 5)
      )
  )

(define cc
    (lambda (amount kinds-of-coins)
      (cond
	;; base case
	((= amount 0) 1)
	;; impossible
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	;; otherwise
	(#t (+ (cc amount (- kinds-of-coins 1))
	       (cc (- amount (first-denom kinds-of-coins)) kinds-of-coins)
	       )
	    )
	)
      )
  )


(define first-denom
    (lambda (kinds-of-coins)
      (cond
	((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)
	)
      )
  )

(define *test-actual* (count-change 100))
)"),
            "292");
}

TEST(GoosIntegrated, Eval) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
(define *test-actual*
    (eval '(+ 1 2)
	  )
  )
)"),
            "3");
}

TEST(GoosIntegrated, Read) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
(define *test-actual*
    (eval (read "(+ 1 2) ")
	  )
  )
)"),
            "3");
}

// todo - test read?

TEST(GoosIntegrated, LambdaWithRest) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
(define func (lambda (x &rest y) y))
(define x (func -123 2 3 1))

(define *test-actual* (+ (car x) (car (cdr x)) (car (cdr (cdr x)))))
)"),
            "6");
}

TEST(GoosIntegrated, IntegerEquality1) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
(define x 2)
(set! x 1)
(define *test-actual* (eq? x 2))
)"),
            "#f");
}

TEST(GoosIntegrated, IntegerEquality2) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
(define x 2)
(define *test-actual* (eq? x 2))
)"),
            "#t");
}

TEST(GoosIntegrated, Lambda3) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
(define my-func (lambda (a b c) b))
(define *test-actual* (my-func 1 2 3))
)"),
            "2");
}

TEST(GoosIntegrated, Lambda4) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
(define func1 (lambda () 1))
(define func2 (lambda () (func1)))
(set! func1 (lambda () 2))
(define *test-actual* (func2))
)"),
            "2");
}

TEST(GoosIntegrated, Lambda5) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
(define x 10)
(define my-func (lambda (x) x))
(define s1 (my-func 20))
(define s2 x)
(define s3 ((lambda (x) x) 30))
(define my-func-2 (lambda (x y) (set! y x) y))
(define s4 (my-func-2 11 12))
(define f3 (lambda (x) (set! x (+ 1 x)) x))
(define s5 (f3 14))
(define x ((lambda (x) (set! x (+ x 1)) x) 16))
(define s6 x)
(define f1 (lambda (x) (+ x 1)))
(define f2 (lambda (x) (+ 2 (f1 x))))
(define s7 (f2 (+ 1 2)))
(define *test-actual* (+ s1 s2 s3 s4 s5 s6 s7))
)"),
            "109");
}

TEST(GoosIntegrated, Let1) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
(define s1
    (let ((x (+ 1 2))
	  (y (+ 3 4)))
      (+ x y)
      )
  )

(define var-1 2)

(define s2
    (let ((x (lambda () var-1)))
      (set! var-1 1000)
      (x)
      )
  )

(define *test-actual* (+ s1 s2))
)"),
            "1010");
}

TEST(GoosIntegrated, Let2) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
(define s1
    (let* ((x (+ 1 2))
	  (y (+ x 5)))
      (+ x y)
      )
  )

(define var-1 2)

(define s2
    (let ((x (lambda () var-1)))
      (set! var-1 1000)
      (x)
      )
  )

(define x 0)
(define s3
    (let ((x (+ 1 2))
	  (y x))
      y
      )
  )

(define x 0)
(define s4
    (let* ((x (+ 1 2))
	  (y x))
      y
      )
  )

(define *test-actual* (+ s1 s2 s3 s4))
)"),
            "1014");
}

TEST(GoosIntegrated, WeirdSymbols) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
'+
'-
'...
'!..
'$.+
'%.-
'&.!
'*.:
'/:.
;':+. ; forget this one, it gets recognized as a keyword argument to (quote).
'<-.
'=.
'>.
'?.
'~.
'_.
'^.
1
)"),
            "1");
}

TEST(GoosIntegrated, WhileLoop) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
(define count 0)
(define sum 0)

(while (< count 100)
  (set! sum (+ sum count))
  (set! count (+ count 1))
  )

(define *test-actual* sum)
)"),
            "4950");
}

TEST(GoosLib, Desfun) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
(desfun test-function (a b c &rest d)
  (set! a (+ a b c))
  (cons a d)
)

(test-function 1 2 3 4 5 6)

)"),
            "(6 4 5 6)");
}

TEST(GoosLib, Factorial) {
  Interpreter i;
  // now large numbers are printed as hex.
  EXPECT_EQ(e(i, "(factorial 10)"), "#x375f00");
}

TEST(GoosLib, ApplySimple) {
  Interpreter i;
  EXPECT_EQ(e(i, "(apply (lambda (x) (* x 2)) '(1 2 3))"), "(2 4 6)");
}

TEST(GoosLib, ApplyComplex1) {
  Interpreter i;
  e(i, "(define y 2)");
  e(i, "(define test-func (lambda (x) (* x y)))");
  EXPECT_EQ(e(i, "(apply test-func '(1 2 3))"), "(2 4 6)");
}
TEST(GoosLib, ApplyComplex2) {
  Interpreter i;
  e(i, "(define y 2)");
  e(i, "(define test-func (lambda (x) (* x y)))");
  e(i, "(set! y 4)");
  EXPECT_EQ(e(i, "(apply test-func '(1 2 3))"), "(4 8 12)");
}

TEST(GoosLib, ApplyComplex3) {
  Interpreter i;
  e(i, "(desfun make-mult-by-x-fun (x) (lambda (y) (* y x)))");
  e(i, "(define mult3 (make-mult-by-x-fun 3))");
  e(i, "(define mult2 (make-mult-by-x-fun 2))");
  EXPECT_EQ(e(i, "(apply mult2 '(1 2 3))"), "(2 4 6)");
  EXPECT_EQ(e(i, "(apply mult3 '(1 2 3))"), "(3 6 9)");
  EXPECT_EQ(e(i, "(apply mult2 '(1 2 3))"), "(2 4 6)");
}

TEST(GoosLib, Let) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
  (let ((x 1)
        (y 2)
        (z 3))
    (+ x y z))

)"),
            "6");
}

TEST(GoosLib, LetRec) {
  Interpreter i;
  EXPECT_EQ(e(i, R"(
  (let* ((x 1)        ; 1
        (y (+ 2 x))  ; 3
        (z (+ 3 y))) ; 6
    (+ x y z))
)"),
            "10");
}

/*!
 * Test special case character printing.
 */
TEST(GoosObject, char_to_string) {
  // printable
  EXPECT_EQ("#\\d", goos::fixed_to_string('d'));

  // special ones
  EXPECT_EQ("#\\\\s", goos::fixed_to_string(' '));
  EXPECT_EQ("#\\\\n", goos::fixed_to_string('\n'));
  EXPECT_EQ("#\\\\t", goos::fixed_to_string('\t'));

  // decimal for ones we don't have special cased
  EXPECT_EQ("#\\{17}", goos::fixed_to_string(char(17)));
}

/*!
 * Test the EmptyListObject
 */
TEST(GoosObject, EmptyList) {
  // create two empty lists
  Object nil = Object::make_empty_list();
  Object nil2 = Object::make_empty_list();

  // check type is set
  EXPECT_TRUE(nil.is_empty_list());

  // check equality operator
  EXPECT_TRUE(nil == nil2);

  // check print and inspect
  EXPECT_EQ(nil.print(), "()");
  EXPECT_EQ(nil.inspect(), "[empty list] ()\n");
}

/*!
 * Test IntegerObject
 */
TEST(GoosObject, Integer) {
  // create integer objects
  Object io = Object::make_integer(-1234);
  Object different = Object::make_integer(3);
  Object same = Object::make_integer(-1234);

  // check type
  EXPECT_TRUE(io.is_int());

  // check equality
  EXPECT_TRUE(same == io);
  EXPECT_FALSE(different == io);

  // check changing the value through the as_int reference
  different.as_int() = -1234;
  EXPECT_TRUE(different.as_int() == -1234);
  EXPECT_TRUE(different == same);

  // check print and inspect
  EXPECT_EQ(different.print(), "-1234");
  EXPECT_EQ(different.inspect(), "[integer] -1234\n");
}

/*!
 * Test FloatObject
 */
TEST(GoosObject, Float) {
  // create integer objects
  Object fo = Object::make_float(-12.34);
  Object different = Object::make_float(3);
  Object same = Object::make_float(-12.34);

  // check type
  EXPECT_FALSE(fo.is_int());
  EXPECT_TRUE(fo.is_float());

  // check equality
  EXPECT_TRUE(same == fo);
  EXPECT_FALSE(different == fo);

  // check changing the value through the as_float reference
  different.as_float() = -12.34;
  EXPECT_TRUE(different.as_float() == -12.34);
  EXPECT_TRUE(different == same);

  // check print and inspect
  EXPECT_EQ(different.print(), "-12.34");
  EXPECT_EQ(different.inspect(), "[float] -12.34\n");
}

/*!
 * Test CharObject
 */
TEST(GoosObject, Char) {
  // create integer objects
  Object co = Object::make_char('w');
  Object different = Object::make_char('X');
  Object same = Object::make_char('w');

  // check type
  EXPECT_FALSE(co.is_int());
  EXPECT_TRUE(co.is_char());

  // check equality
  EXPECT_TRUE(same == co);
  EXPECT_FALSE(different == co);

  // check changing the value through the as_char reference
  different.as_char() = 'w';
  EXPECT_TRUE(different.as_char() == 'w');
  EXPECT_TRUE(different == same);

  // check print and inspect
  EXPECT_EQ(different.print(), "#\\w");
  EXPECT_EQ(different.inspect(), "[char] #\\w\n");
}

/*!
 * Test SymbolObject
 */
TEST(GoosObject, Symbol) {
  SymbolTable st, st2;
  Object obj = Object::make_symbol(&st, "test1");
  Object obj2 = Object::make_symbol(&st, "test2");
  Object obj3 = Object::make_symbol(&st, "test1");
  Object obj4 = Object::make_symbol(&st2, "test1");

  // check type
  EXPECT_TRUE(obj.is_symbol());

  // check equality
  EXPECT_TRUE(obj == obj3);
  EXPECT_FALSE(obj == obj2);
  EXPECT_FALSE(obj == obj4);  // different because different st's

  // check interning works
  auto obj_as = obj.as_symbol();
  auto obj2_as = obj2.as_symbol();
  auto obj3_as = obj3.as_symbol();
  auto obj4_as = obj4.as_symbol();

  EXPECT_TRUE(obj_as == obj3_as);
  EXPECT_FALSE(obj_as == obj2_as);
  EXPECT_FALSE(obj_as == obj4_as);  // different because different st's.

  // check print and inspect
  EXPECT_EQ(obj.print(), "test1");
  EXPECT_EQ(obj.inspect(), "[symbol] test1\n");
}

/*!
 * Test StringObject
 */
TEST(GoosObject, String) {
  Object obj1 = StringObject::make_new("test1");
  Object obj2 = StringObject::make_new("test2");
  Object obj3 = StringObject::make_new("test1");

  Object obj4 = StringObject::make_new("test-with\"quote");

  EXPECT_TRUE(obj1.is_string());

  EXPECT_TRUE(obj1 == obj3);
  EXPECT_FALSE(obj1 == obj2);

  obj2.as_string()->data = "test1";
  EXPECT_TRUE(obj1 == obj2);

  EXPECT_EQ(obj1.print(), "\"test1\"");
  EXPECT_EQ(obj1.inspect(), "[string] \"test1\"\n");
  EXPECT_EQ(obj4.print(), "\"test-with\\\"quote\"");
}

/*!
 * Test PairObject
 */
TEST(GoosObject, Pair) {
  Object obj = PairObject::make_new(Object::make_integer(1), Object::make_integer(2));
  Object obj2 = PairObject::make_new(Object::make_integer(2), Object::make_integer(2));
  Object obj3 = PairObject::make_new(Object::make_integer(1), Object::make_integer(2));

  EXPECT_TRUE(obj.is_pair());

  EXPECT_TRUE(obj == obj3);
  EXPECT_FALSE(obj == obj2);
  obj2.as_pair()->car = Object::make_integer(1);
  EXPECT_TRUE(obj == obj2);

  EXPECT_EQ(obj.print(), "(1 . 2)");
  EXPECT_EQ(obj.inspect(), "[pair] (1 . 2)\n");
}

/*!
 * Test proper list stuff
 */
TEST(GoosObject, PairList) {
  Object obj = build_list({});
  EXPECT_TRUE(obj.is_empty_list());

  obj = build_list({Object::make_integer(1)});
  EXPECT_EQ(obj.print(), "(1)");

  obj = build_list({Object::make_integer(1), Object::make_integer(2), Object::make_integer(3)});
  auto obj2 =
      build_list({Object::make_integer(1), Object::make_integer(2), Object::make_integer(2)});
  auto obj3 =
      build_list({Object::make_integer(1), Object::make_integer(2), Object::make_integer(3)});

  EXPECT_EQ(obj.print(), "(1 2 3)");
  EXPECT_TRUE(obj == obj3);
  EXPECT_FALSE(obj == obj2);
}

/*!
 * Test ArrayObject
 */
TEST(GoosObject, Array) {
  Object empty_array = ArrayObject::make_new({});
  EXPECT_EQ(empty_array.as_array()->size(), 0);
  EXPECT_TRUE(empty_array.is_array());
  EXPECT_EQ(empty_array.print(), "#()");

  auto array1 = ArrayObject::make_new(
      {Object::make_integer(1), Object::make_integer(2), Object::make_integer(3)});
  auto array2 = ArrayObject::make_new(
      {Object::make_integer(1), Object::make_integer(2), Object::make_integer(4)});
  auto array3 = ArrayObject::make_new(
      {Object::make_integer(1), Object::make_integer(2), Object::make_integer(3)});

  EXPECT_TRUE(array1 == array3);
  EXPECT_TRUE(array1 != array2);

  EXPECT_EQ(array1.print(), "#(1 2 3)");
  EXPECT_EQ(array1.inspect(), "[array] size: 3 data: #(1 2 3)\n");
}

TEST(GoosSpecialForms, Define) {
  Interpreter i;

  e(i, "(define x 010)");
  EXPECT_EQ(e(i, "x"), "10");

  e(i, "(define :env *goal-env* x 20)");
  EXPECT_EQ(e(i, "x"), "10");

  Object goal_env;
  EXPECT_TRUE(i.get_global_variable_by_name("*goal-env*", &goal_env));

  auto x_in_goal_env = goal_env.as_env()->vars.lookup(i.intern("x").as_symbol());
  EXPECT_TRUE(x_in_goal_env);
  EXPECT_EQ(x_in_goal_env->print(), "20");

  // test automatic environment of define
  e(i, "(begin (desfun test-define () (define x 500)) (test-define))");
  EXPECT_EQ(e(i, "x"), "10");

  // test manual setting of global env for define
  e(i, "(begin (desfun test-define () (define :env *global-env* x 500)) (test-define))");
  EXPECT_EQ(e(i, "x"), "500");

  e(i, "(begin (desfun test-define () (define x :env *global-env* 600)) (test-define))");
  EXPECT_EQ(e(i, "x"), "600");

  e(i, "(begin (desfun test-define () (define x 700 :env *global-env*)) (test-define))");
  EXPECT_EQ(e(i, "x"), "700");

  i.disable_printfs();
  EXPECT_ANY_THROW(e(i, "(define :env 3 x 10)"));
  EXPECT_ANY_THROW(e(i, "(define :beans 3 x 10)"));
  EXPECT_ANY_THROW(e(i, "(define x)"));
  EXPECT_ANY_THROW(e(i, "(define x 1 2)"));
  EXPECT_ANY_THROW(e(i, "(define 1)"));
  EXPECT_ANY_THROW(e(i, "(define 1 2)"));
  EXPECT_ANY_THROW(e(i, "(define)"));
}

TEST(GoosSpecialForms, Set) {
  Interpreter i;
  e(i, "(define x 10)");
  EXPECT_EQ(e(i, "(set! x (+ 10 10))"), "20");
  EXPECT_EQ(e(i, "x"), "20");

  // set parent env
  EXPECT_EQ(e(i, "(begin (desfun test-set () (set! x 30)) (test-set))"), "30");
  EXPECT_EQ(e(i, "x"), "30");

  // check non-global env (also a good check with two different variables named x)
  EXPECT_EQ(e(i, "(begin (desfun test-set (x) (set! x (+ x 30))) (test-set x))"), "60");
  EXPECT_EQ(e(i, "x"), "30");

  i.disable_printfs();
  for (auto x :
       {"(set!)", "(set! x)", "(set! 1)", "(set! 1 1)", "(set! x 1 2)", "(set! x 1 :test 2)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosSpecialForms, Quote) {
  Interpreter i;
  e(i, "(define x 'y)");
  EXPECT_EQ(e(i, "x"), "y");
  e(i, "(define x (quote z))");
  EXPECT_EQ(e(i, "x"), "z");
  e(i, "(define x '(1 2 3))");
  EXPECT_EQ(e(i, "x"), "(1 2 3)");
  e(i, "(define x '(1 2 3 ,w))");
  EXPECT_EQ(e(i, "x"), "(1 2 3 (unquote w))");

  i.disable_printfs();
  for (auto x : {"(quote)", "(quote 1 2)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosSpecialForms, DoubleQuote) {
  Interpreter i;
  e(i, "(define x ''y)");
  EXPECT_EQ(e(i, "x"), "(quote y)");
  e(i, "(define x `'`y)");
  EXPECT_EQ(e(i, "x"), "(quote (quasiquote y))");
}

TEST(GoosSpecialForms, QuasiQuote) {
  Interpreter i;
  e(i, "(define x 'y)");
  EXPECT_EQ(e(i, "(define z `(x ,x z))"), "(x y z)");
  i.disable_printfs();
  for (auto x : {"(quasiquote)", "(quasiquote 1 2)", "(unquote 1)", "(unquote-spicing 1)",
                 "`( (unquote-splicing) 2)", "`( (unquote-splicing 1 2) 2)",
                 "`( (unquote-splicing 1) 2)", "`( (unquote) 2)", "`( (unquote 1 2) 2)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosSpecialForms, QuasiQuoteSplicing) {
  Interpreter i;
  e(i, "(define x '(1 2 3))");
  EXPECT_EQ(e(i, "(define z `(x ,x z))"), "(x (1 2 3) z)");
  EXPECT_EQ(e(i, "(define z `(x ,@x z))"), "(x 1 2 3 z)");
}

TEST(GoosSpecialForms, Or) {
  Interpreter i;
  e(i, "(desfun rf () #f)");
  e(i, "(desfun r1 () 1)");
  EXPECT_EQ(e(i, "(or #f (rf) #f)"), "#f");
  EXPECT_EQ(e(i, "(or #f (rf) (r1) 2 #f)"), "1");
  EXPECT_EQ(e(i, "(or #f (rf) 1 2 cannot-be-evaluated)"), "1");

  i.disable_printfs();
  for (auto x : {"(or)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosSpecialForms, And) {
  Interpreter i;
  e(i, "(desfun rf () #f)");
  e(i, "(desfun r1 () 1)");
  EXPECT_EQ(e(i, "(and #f (rf) #f)"), "#f");
  EXPECT_EQ(e(i, "(and (r1) 2 3 4 3 #t 3 2)"), "2");
  EXPECT_EQ(e(i, "(and #f (rf) 1 2 cannot-be-evaluated 2 3 4)"), "#f");

  i.disable_printfs();
  for (auto x : {"(and)"}) {
    EXPECT_ANY_THROW(e(i, x));
  }
}

TEST(GoosBuiltins, Format) {
  Interpreter i;
  EXPECT_EQ(e(i, "(fmt #f \"{}, {}, {}\" 3 'bean \"str\")"), "\"3, bean, str\"");
}

TEST(GoosBuiltins, Error) {
  Interpreter i;
  EXPECT_ANY_THROW(e(i, "(error \"hi\")"));
}

TEST(GoosBuiltins, Ash) {
  Interpreter i;
  EXPECT_EQ(e(i, "(ash 3 2)"), "12");
  EXPECT_EQ(e(i, "(ash 3 -1)"), "1");
}

TEST(GoosBuiltins, StringUtils) {
  Interpreter i;
  EXPECT_EQ(e(i, "(string-ref \"test\" 2)"), "#\\s");
  EXPECT_EQ(e(i, "(string-length \"test\")"), "4");
  EXPECT_EQ(e(i, "(string-append \"hello\" \" \" \"world\")"), "\"hello world\"");
  EXPECT_EQ(e(i, "(symbol->string 'test)"), "\"test\"");
}

TEST(GoosBuiltins, HashTable) {
  Interpreter i;
  e(i, "(define ht (make-string-hash-table))");
  EXPECT_EQ(e(i, "(car (hash-table-try-ref ht \"foo\"))"), "#f");

  e(i, "(hash-table-set! ht \"foo\" 123)");
  EXPECT_EQ(e(i, "(car (hash-table-try-ref ht \"bar\"))"), "#f");
  EXPECT_EQ(e(i, "(car (hash-table-try-ref ht \"foo\"))"), "#t");
  EXPECT_EQ(e(i, "(cdr (hash-table-try-ref ht \"foo\"))"), "123");
  e(i, "(hash-table-set! ht \"foo\" 456)");
  EXPECT_EQ(e(i, "(cdr (hash-table-try-ref ht \"foo\"))"), "456");
}