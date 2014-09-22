;; graph.lisp

(in-package #:yarty)

;; make a bar chart of the duration of the last test runs durations

(defun timing-alist (package)
  (loop for s being the external-symbols of package
        when (get s :duration)
        collect (cons s (get s :duration))))

(defun graph-durations (&rest packages)
  "Return a string which is a bar graph of the run time of each test in packages.

Experimental feature."
  (let* ((results (alexandria:mappend #'timing-alist packages))
         (symbols (mapcar #'car results))
         (times (mapcar #'cdr results)))
    (spark:vspark times :labels symbols :title "Latest test durations")))
