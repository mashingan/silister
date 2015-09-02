import std.stdio;
import std.variant;

import simpleread;
import simpleext01;
import simpledatatypes;
import simpleevaluate;

void main() {
    setupEnvironment();
    assert (stringToken == '"');
    assert (quoteToken == '\'');
    assert (quasiToken == '`');
    assert (commaToken == ',');
    assert (exprToken == '(');
    assert (stringToken == "\"hello world\"");
    assert (quoteToken == "'hello-world");
    assert (quasiToken == "`hello-world");
    assert (commaToken == ",hello-world");
    assert (exprToken == "(hello world)");

    assert (read("hello") == read("hello"));
    assert (eval(read("''hello")) == read("'hello"));
    assert (eval(read("'''hello")) == read("''hello"));
    assert (eval(read("(+ 1 2 3 (+ 2 3) 4 5)")) == read("20"));
    assert (eval(read("'(1 2 3 4)")) == read("(1 2 3 4)"));

    eval(read("(setf hello 2 world 3)"));
    assert(eval(read("(cons hello world)")) == read("'(2 3)"));

    eval(read("(defun hello (x) (* x 2))"));
    eval(read("(defun world () 2)"));
    assert (eval(read("(hello (world))")) == read("4"));

    assert (eval(read("`(the sum of 17 and 83 is ,(+ 17 83))")) ==
            read("(the sum of 17 and 83 is 100)"));
    assert (eval(read("`(the sum of 17 and 83 is (+ 17 83))")) ==
            read("(the sum of 17 and 83 is (+ 17 83))"));
    assert (eval(read("`(the sum of 17 and 83 is '(,(+ 17 83)))")) ==
            read("(the sum of 17 and 83 is '(100))"));
    assert (eval(read("`(the sum of 17 and 83 is '((,(+ 17 83))))")) ==
            read("(the sum of 17 and 83 is '((100)))"));

    assert (eval(read("(let ((x 'hello) (y 'world)) (list x y))")) ==
            read("(hello world)"));

    eval(read("(setf hello 2 world 3)"));
    assert (eval(read("`(,hello ,world)")) == read("( 2 3)"));

    eval(read("(setf hello-world (list hello world))"));
    assert (eval(read("`,hello-world")) == read("(2 3)"));

    eval(read("(defun mklist (arg1 arg2 &rest ok)
        (list arg1 arg2 ok))"));
    assert (eval(read(`(mklist 1 'a 'hello "hehe haha" #\a)`)) ==
            read(`(1 a '(hello "hehe haha" #\a))`));
    assert (eval(read(`(mklist 1 'a 'hello "hehe haha" #\a)`)) ==
            read("(1 a '(hello \"hehe haha\" #\\a))"));

    eval(read("(defmacro double-using-macro (x) (* x 2))"));
    assert (eval(read("(double-using-macro 2)")) == read("4"));

    // BUG: evaluating a macro only return form, need to eval twice to
    // be used with another part of expression
    // this is just like a macroexpansion
    eval(read("(defmacro swap (a b) `(let ((temp ,a))
                (setf ,a ,b ,b temp)))"));
    assert (eval(read("(swap hello world)")) ==
            read("(let ((temp hello))
                    (setf hello world world temp))"));
    // BUG: the twice of eval according macro definition
    assert (eval(read("(let ((x 2) (y 3))
                        (eval (swap x y))
                        (list x y))")) == read("(3 2)"));

    assert (eval(read("(load \"src/stdlib.lisp\")")) == read("t"));;
    assert (eval(read("(load \"src/not-exists.lisp\")")) ==
            LispObject(Variant(), LVAL.ERR));

    assert (eval(read("`(,@(map (lambda (x) (* x 2)) '(1 2 3)))")) ==
            read("(2 4 6)"));
    // BUG: cons return quoted pair to stop the identification with 
    // list and doesn't have to be evaluated as function invocation
    // hence the apostrophe in expression ('(2 4 6))
    assert (eval(read("`(,(map (lambda (x) (* x 2)) '(1 2 3)))")) ==
            read("('(2 4 6))"));
}
