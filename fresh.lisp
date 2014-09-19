;;;; resh.lisp

(in-package #:yarty)

;;; Provision to allow running tests in a fresh lisp image; be it of
;;; a different implementation to test for compatibility or the same
;;; implementation to test on a clean load.

(defun launch-sbcl-server (&optional (host "127.0.0.1") (port 11111))
  "Launch sbcl. Pass host as a string and port as an integer."
  (uiop:run-program
   ;; I think the use of & to backgroundis posix specific; port to
   ;; windows later.
   (format () "sbcl --noinform --eval '(ql:quickload :lfarm-server)' --eval '(lfarm-server:start-server \"~A\" ~A)' &" host port)))

(defun launch-ccl-server (&optional (host "127.0.0.1") (port 11111))
  (uiop:run-program
   (format () "ccl -e '(ql:quickload :lfarm-server)' -e '(lfarm-server:start-server \"~A\" ~A)' &" host port)))

(defun fresh-test (system &key (host "127.0.0.1") (port 11112) (lisp :ccl))
  "Test on a fresh CCL or SBCL image. Assumes the userinit loads quicklisp."
  (unwind-protect
       (progn
         (cond ((string-equal lisp :ccl)
                (launch-ccl-server host port))
               ((string-equal lisp :sbcl)
                (launch-sbcl-server host port))
               (t (return-from fresh-test
                    (format t "lisp must name either ccl or sbcl not ~A." lisp))))
         (let* ((lfarm:*kernel* (lfarm:make-kernel `((,host ,port))))
                (channel (lfarm:make-channel)))
           (lfarm:submit-task
            channel
            `(lambda ()
               (funcall (find-symbol "QUICKLOAD" "QL") 'yarty)
               (yarty:test-system ,system)))
           (lfarm-client.kernel:receive-result channel)))
    (lfarm-admin:end-server host port)))
