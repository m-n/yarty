;;;; yarty.asd

(asdf:defsystem #:yarty
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
               (:file "yarty")
               (:file "autorun")))

