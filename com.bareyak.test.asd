;;;; com.bareyak.test.asd

(asdf:defsystem #:com.bareyak.test
  :serial t
  :description "A small testing library"
  :author "Matt Niemeir <matt.niemeir@gmail.com>"
  :license "BSD 2-clause"
  :depends-on (#:cl-fam
               #:alexandria
               #:bordeaux-threads
               #:lparallel)
  :components ((:file "package")
               (:file "utils")
               (:file "com.bareyak.test")
               (:file "autorun")))

