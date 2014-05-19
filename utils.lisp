;;;; utils.lisp

(in-package #:yarty)

;;;; General

(defmacro assocf (key val place &rest keyargs &environment env)
  "Sets first instance of key in alist to val, adding entry if necessary."
  (multiple-value-bind (vars vals bind set access)
      (get-setf-expansion place env)
    (alexandria:with-gensyms (gkey gval cons gaccess)
      `(let ((,gkey ,key)
             (,gval ,val))
         (let* (,@(mapcar #'list vars vals)
                (,gaccess ,access))
           (multiple-value-bind ,bind
               (let ((,cons (assoc ,gkey ,gaccess ,@keyargs)))
                 (if ,cons
                     (prog1 ,gaccess
                       (setf (cdr ,cons) ,gval))
                     (acons ,gkey ,gval ,gaccess)))
             ,set))))))

(defmacro ensure-dynamic-bindings ((&rest symbols) &body body)
  "Ensure symbols are dynamically bound without shadowing extant bindings."
  (alexandria:with-gensyms (unbound)
    `(let ((,unbound (remove-if #'boundp ',symbols)))
       (progv ,unbound ',(make-list (length symbols))
         (locally (declare (special ,@symbols))
           ,@body)))))

(defun function-name-p (form)
  (and form
       (symbolp form)
       (symbol-function form)
       (not (special-operator-p form))
       (not (macro-function form))))
