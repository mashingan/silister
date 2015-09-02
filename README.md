# siliste
Simple Lisp interpreter in on top of [D](http://dlang.org/). The this interpreter is only implemented as basic evaluator and not intended to fully usable (for now).

## Implemented
* Primitive functions such as `cons`, `car`, `cdr`, `setf`, `if`, `print`, `eprogn`, `lambda`, basic math operator.
* `let`, `defun`, and `defmacro` implemented in D instead of in term of above primitives.
* Loading file sources.

## Usage
To compile the source into executable, make sure we have a 
[D compiler](http://dlang.org/download.html). 
After we have installed D compiler, do below:

    $ git clone https://github.com/mashingan/silister simplelisp
    $ cd simplelisp/
    $ make
    $ bin/repl

To quit interpreter, invoke quit function `(quit)`


## Known Bugs
There are many glaringly known bugs such as:

* Evaluating defined symbol will result as error binding in top of interpreter, but when it's evaluated by `(eval the-symbol)` it will correctly give the result
  Example:

        >> (setf hello 3)
           3
        >> hello
           Error binding: No symbol hello defined
        >> (eval hello)
           3

* Macro which defined with defmacro need to be evaluated explicitly because it only returns the form, not the evaluated form.
  Example:

        >> (defmacro swap (a b) `(let ((temp ,a)) (setf ,a ,b ,b temp)))
        >> (setf x 2 y 3)
           3
        >> (swap x y)
           (let ((temp x)) (setf x y y temp))
        >> (eval x)
           2
        >> (eval  (swap x y))
           2
        >> (eval x)
           3

* `cons` for 2 atoms won't result pair, all in list.

To see other usage, please see the file test/simpletest.dr
