YARTY
=====

Yare and Astounding Regression Tester: YARTY

For SBCL and CCL.

YARTY is similar to other CL regression test libraries. The main
design points particular to YARTY are as follows. First, the returned
value from `RUN-TESTS` is immediately compatible with
CL-TEST-GRID. Second, there is only one test predicate \-\- `EACH`
\-\- and it only tests whether values are truthy. For other
predicates, e.g. `EQUAL`, the YARTY way is to include the `EQUAL` call
within an `EACH`. Third, automatic triggering of the test suite on
filesystem changes is included via `AUTORUN`.

It is a primary goal of YARTY to make `AUTORUN` practical. When
active, it internally calls `ASDF:TEST-SYSTEM` when any of the watched
system's source files are touched. ASDF thus takes care of triggering
any necessary recompilation.

Installation
============

At the shell prompt:

    cd ~/quicklisp/local-projects
    git clone https://github.com/m-n/yarty

Then, in lisp:

    (ql:quickload 'yarty)

In order to see the output of `AUTORUN` at the slime repl, you may
additionally need to configure slime to globally redirect output:

    ;;;; in file ~/.swank.lisp
    (cl:setq swank:*globally-redirect-io* cl:t)

Setup and Organization
======================

YARTY expects the tests to typically be run under ASDF. This is the
recommended way to set that up:

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
      (let ((x 1)
            (y 0))
        (each (= x y)
              (/ x y))))

Then calling `(asdf:test-system :example)` should create output
similar to:

      In FAILING-TEST
      Failing Form (= X Y)
                   (= 1 0)
      each in FAILING-TEST threw "DIVISION-BY-ZERO detected
    performing / on (1 0)"
        Erroring Form (/ X Y)
    (:FAILED-TESTS ("failing-test"))

YARTY relies on this setup:

* YARTY relies on `ASDF:TEST-SYSTEM`'s recompilation to keep tests up
  to date with changed sources.
* `AUTORUN` calls `ASDF:TEST-SYSTEM` to trigger testing.
* `YARTY:TEST-SYSTEM` is a wrapper around `ASDF:TEST-SYSTEM`.

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
    *OUTPUT*:           The stream testing info is print to.

Full docstrings and argument lists are available in the file `docstrings`.

Grouping tests
==============

Many Common Lisp test libraries offer some way to group multiple tests
together, sometimes calling the grouping a "suite" or similar.

The YARTY way to group tests is to define each group of tests within
its own package. You can then run them together by
`(run-tests 'package1 'package2)`.

Heirarchical grouping of tests is not provided by YARTY.

Footer
======

Any bug reports, success stories, failure stories, patches, or other
feedback are welcome at `https://github.com/m-n/yarty` and
`matt.niemeir@gmail.com`.
