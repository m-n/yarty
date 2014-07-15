;;;; yarty.asd

(asdf:defsystem #:yarty
  :serial t
  :description "Yare and Astounding Regression Tester: YARTY"
  :author "Matt Niemeir <matt.niemeir@gmail.com>"
  :license "BSD 2-clause"
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:lparallel)
  :components ((:file "package")
               (:file "utils")
               (:file "yarty")
               (:file "autorun")))
