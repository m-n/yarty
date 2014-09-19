;;;; yarty.asd

(asdf:defsystem #:yarty
  :serial t
  :description "Yare and Astounding Regression Tester: YARTY"
  :author "Matt Niemeir <matt.niemeir@gmail.com>"
  :license "BSD 2-clause"
  :depends-on (#:alexandria

               ;; Concurrency used for running tests on filesystem change
               #:lparallel

               ;; RPC used for running tests in fresh images
               #:lfarm-client
               #:lfarm-admin
               )
  :components ((:file "package")
               (:file "utils")
               (:file "yarty")
               (:file "autorun")
               (:file "fresh")
               ))
