YARTY
=====

Yare and Astounding Regression Tester: YARTY

For SBCL and CCL.

Status: The library has only been used lightly, but seems to preform
reasonably.

YARTY is similar to other CL regression test libraries. The main
design points particular to YARTY are as follows. First, the returned
value from `RUN-TESTS` is immediately compatible with
CL-TEST-GRID. Second, there is only one test predicate \-\- `EACH`
\-\- and it only tests whether values are truthy. For other
predicates, e.g. `EQUAL`, the YARTY way is to include the `EQUAL` call
within an `EACH`. Third, automatic triggering of the test suite on
filesystem changes is included via `AUTORUN`.

`AUTORUN` is still experimental, but it is a primary goal of YARTY to
make it practical. When active, it internally calls `ASDF:TEST-SYSTEM`
when any of that system's source files are touched. ASDF thus takes
care of triggering any necessary recompilation.

Glossary
========

For Writing Tests
-----------------

    DEFTEST:            Define a function that will be called during RUN-TESTS.
    EACH:               Test that each form returns truthy.
    DEFTEST/EACH:       Like DEFTEST but wraps its body in an EACH.
    SIGNALS-A:          Returns true if body signals the condition.

For Running Tests
-----------------

    RUN-TESTS:          Runs all the tests defined by DEFTEST in the given packages.
    AUTORUN:            Toggle whether asdf:test-system is automatically run when source is touched.
    FRESH-TEST:        Test on a fresh CCL or SBCL image. Assumes the userinit loads quicklisp.
    TEST-SYSTEM:        Test the system. Either return the last value of RUN-TESTS or quit the image.

For Managing Tests
------------------

    CLEAR-TESTS:        Clear the tests for the given package, default to *package*.
    *HANDLE-ERRORS*:    t: handle in tests; nil: decline to handle. Default is t.

Full docstrings and argument lists are available in the file `docstrings`.

Installation
============

At the shell prompt:

    cd ~/quicklisp/local-projects
    git clone https://github.com/m-n/yarty

Then, in lisp:

    (ql:quickload 'yarty)

In order to see the output of `AUTORUN` at the slime repl, you may
additionally need to configure slime to globally redirect output.

Any bug reports, success stories, failure stories, patches, or other
feedback are welcome at `https://github.com/m-n/yarty` and
`matt.niemeir@gmail.com`.

Use with ASDF
=============

Below is the recommended way to setup YARTY tests to run to run when
`ASDF:TEST-SYSTEM` is called.

    ;;;; example.asd

    (asdf:defsystem #:example
      :components ((:file "example"))
      ;; This line alerts AUTORUN to the name of the
      ;; test system, so that AUTORUN can watch its files.
      :in-order-to ((test-op (load-op :example-test))))

    (asdf:defsystem #:example-test
      :depends-on (#:example #:yarty)
      :components ((:file "tests")))

    (defmethod asdf:perform ((o asdf:test-op)
                             (c (eql (asdf:find-system :example))))
      (funcall (intern (symbol-name :run-tests)
                       (find-package :yarty))
               :example-test))

And the tests file might look something like this:

    ;;;; tests.lisp

    (defpackage #:example-test
      (:use :cl :yarty))

    (in-package #:example-test)

    (deftest failing-test
      (each (= 1 0)
            (/ 1 0)))

`AUTORUN` calls `ASDF:TEST-SYSTEM` to trigger testing, so if you plan
to use `AUTORUN` you should create a setup like above.

Grouping tests
==============

Many Common Lisp test libraries offer some way to group multiple tests
together, sometimes calling the grouping a "suite" or similar.

The YARTY way to group tests is to define each group of tests within
its own package. You can then run them together by
`(run-tests 'package1 'package2)`.

Heirarchical grouping of tests is not provided by YARTY.
