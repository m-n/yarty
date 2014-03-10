;;;; utils.lisp

(in-package #:com.bareyak.test)

;;;; General

(defmacro assocf (key val place &rest keyargs &environment env)
  "Sets first instance of key in alist to val, adding entry if necessary."
  (multiple-value-bind (vars vals nv set access)
      (get-setf-expansion place env)
    (alexandria:with-gensyms (gkey gval cons gaccess)
      `(let ((,gkey ,key)
             (,gval ,val))
         (let* (,@(mapcar #'list vars vals)
                (,gaccess ,access)
                (,(car nv)
                 (let ((,cons (assoc ,gkey ,gaccess ,@keyargs)))
                   (if ,cons
                       (prog1 ,gaccess
                         (setf (cdr ,cons) ,gval))
                       (acons ,gkey ,gval ,gaccess)))))
           ,set)))))
