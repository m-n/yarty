;;;; package.lisp

(defpackage #:com.bareyak.test
  (:use #:cl)
  (:export
   #:deftest/and
   #:deftest
   #:run-tests
   #:test-and
   #:signals-a
   #:test-on-change
   #:clear-tests
   #:*handle-errors*
   ))

