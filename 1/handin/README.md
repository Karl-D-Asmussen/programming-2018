Skeleton for subscript interpreter
==================================

This directory contains a skeleton for the subscript interpreter,
organized to be build with the Haskell build tool `stack`


Files
-----

The `src` directory contains three files:

 * `SubsAst.hs` and `SubsInterpreter.hs`, see assignment description.
 * `Main.hs` a simple command-line driver wrapping the interpreter.

The `tests` directory contains the beginning of a test suite, edit
`Test.hs` to add your own tests.

The files `stack.yaml` and `package.yaml` is used by `stack`. You
shouldn't need to change `stack.yaml`, but you may need to edit
`package.yaml`, for instance if you want to use the `tasty-quickcheck`
package. The format of `package.yaml` is mostly self-explaining
otherwise just ask on Absalon.

The directory `examples` contains a couple of subscript programs and
the corresponding ASTs.


How to use `stack`
------------------

To build the simple command-line subscript interpreter:

    stack build

To run the subscript interpreter on the a subscript file `foo.js`:

    stack exec subscript-interpreter foo.js

To run the test-suite:

    stack test
