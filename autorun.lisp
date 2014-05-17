;;;; autorun.lisp

(in-package #:yarty)

(defun monitor (filename fn &key (monitor-fn #'cl-fam:fam-monitor-file))
  (bt:make-thread
   (lambda ()
     (funcall monitor-fn filename)
     (let ((channel (lparallel:make-channel)))
       (loop (let ((fam-event (cl-fam:fam-next-event)))
               (lparallel:submit-task channel fn fam-event)))))
   :name (namestring filename)
   :initial-bindings  `((cl-fam:*fam* . (cl-fam:fam-open
                                         (namestring ,filename) ()))
                        (*package* . ,*package*)
                        (lparallel:*kernel* . ,(lparallel:make-kernel
                                                4
                                                :name (namestring filename)))
                        (*restart-queue* . ,(lparallel.queue:make-queue
                                             :fixed-capacity 1))
                        (*in-progress-queue* . ,(lparallel.queue:make-queue
                                                 :fixed-capacity 1)))))

(defmacro with-watched-filesystem
    ((var path &key (location-type (or :directory :file)))
     &body body)
  `(monitor ,path (lambda (,var) ,@body)
            :monitor-fn (ecase ,location-type
                          ((:directory) #'cl-fam:fam-monitor-directory)
                          ((:file)      #'cl-fam:fam-monitor-file))))

(defun test (&optional (system (make-symbol (package-name *package*))))
  (let ((*test-system* system))
    (unwind-protect
         (progn
           (lparallel.queue:with-locked-queue *in-progress-queue*
             (unless (lparallel.queue:peek-queue/no-lock *in-progress-queue*)
               (lparallel.queue:push-queue/no-lock t *in-progress-queue*)))
           (asdf:test-system system))
      (lparallel.queue:try-pop-queue *in-progress-queue*))))

(defun test-on-change
    (&key (system (make-symbol (package-name *package*)))
          (location (directory-namestring
                     (asdf/system:system-source-file
                      system)))
          (location-type (or :directory :file)))
  "Start a background thread which runs tests when location is modified.

System should be a string or symbol designating a package
name. Location defaults to the directory containing the
asdf:system-source-file. location-type is either :directory or :file,
defaulting to :directory."
  (when (stringp system)
    (setq system (make-symbol (string-upcase system))))
  (with-watched-filesystem (event location
                                  :location-type location-type)
    (when (eq (cl-fam:fam-code event) :fam-changed)
      ;; gutted from quicklisp's call-with-quiet-compilation
      (let ((*compile-verbose* nil)
            (*compile-print* nil)
            (*load-verbose* nil)
            (*load-print* nil))
        (handler-bind (;(warning #'muffle-warning)
                       )
          (handler-case
              (if (lparallel.queue:peek-queue *in-progress-queue*)
                  (lparallel.queue:with-locked-queue *restart-queue*
                    (unless (lparallel.queue:peek-queue/no-lock *restart-queue*)
                      (lparallel.queue:push-queue/no-lock t *restart-queue*)))
                  (test system))
            (error (c)
              (pprint c *error-output*)
              (print '(:failed)))))))))
