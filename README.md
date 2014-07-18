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
make it practical. When active, it internally calls ASDF:TEST-SYSTEM
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

    AUTORUN:            Toggle whether asdf:test-system is automatically run when source is touched.
    RUN-TESTS:          Runs all the tests defined by DEFTEST in a given package.
    *HANDLE-SERIOUS-CONDITIONS*:  t: handle in tests; nil: decline to handle. Default is t.

For Managing Tests
------------------

    CLEAR-TESTS:        Clear the tests for the given package, default to *package*.

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
