;;;; autorun.lisp

(in-package #:yarty)

(defvar *control-queue* (lparallel.queue:make-queue))

(defvar *system-channels* (make-hash-table))

(defun ensure-system-channel (system)
  (if (typep (gethash system *system-channels*)
             'lparallel.kernel:channel)
      (gethash system *system-channels*)
      (setf (gethash system *system-channels*)
            (let* ((restart-queue (lparallel.queue:make-queue
                                   :fixed-capacity 1))
                   (in-progress-queue (lparallel.queue:make-queue
                                       :fixed-capacity 1))
                   (control-queue (lparallel.queue:make-queue))
                   (lparallel:*kernel*
                    (lparallel.kernel:make-kernel
                     4
                     :name (concatenate 'string
                                        "yarty-autorun:"
                                        (string system))
                     :bindings
                     `((*compile-verbose* . nil)
                       (*compile-print* . nil)
                       (*load-verbose* . nil)
                       (*load-print* . nil)
                       (*package* . ,*package*)
                       (*restart-queue* . ,restart-queue)
                       (*in-progress-queue* . ,in-progress-queue)
                       (*control-queue* . ,control-queue)
                       (*handle-errors* . t)
                       (*test-system* . ,system)))))
              (lparallel:make-channel)))))

(defun autorun-test-system (&optional (system (intern (package-name *package*) :keyword)))
  (unwind-protect (handler-case (asdf:test-system system)
                    (error (c)
                      (format t "Compilation aborted due to error `~A`" c)
                      (return-from autorun-test-system :failed-tests)))
    (if (lparallel.queue:try-pop-queue *restart-queue*)
        (autorun-test-system system)
        (lparallel.queue:try-pop-queue *in-progress-queue*))))

(defun systems-in-order-to-test (system)
  ;; lists the systems that will be loaded for testing due to
  ;; the system definition's :in-order-to slot.
  (unless (typep system 'asdf:system)
    (setq system (asdf:find-system
                  (intern (string-upcase system) :keyword))))
  (let (out)
    (dolist (in-order-to (asdf/component:component-in-order-to system))
      (when (string= (car in-order-to) :test-op)
        (dolist (to-do (cdr in-order-to))
          (when (string= (car to-do) :load-op)
            (dolist (systems (cdr to-do))
              (push systems out))))))
    (reverse out)))

(defun files-to-watch (system)
  (handler-bind ((error (lambda (c)
                          (declare (ignore c))
                          (when *handle-errors*
                            (return-from files-to-watch
                              ())))))
    (flet ((to-watch-one-system (system)
             (cons (asdf:system-source-file system)
                   (mapcar #'asdf:component-pathname
                           (remove-if-not (alexandria:of-type 'asdf:file-component)
                                          (asdf:required-components system))))))
      (remove-duplicates
       (reduce #'append
               (mapcar #'to-watch-one-system
                       (cons system (systems-in-order-to-test system))))
       :test #'equalp
       :from-end t))))

(defun files-date-sum (files)
  (reduce #'+ (mapcar #'file-write-date files)))

(defun make-system-watcher (system)
  (let* ((files (files-to-watch system))
         (date-sum (files-date-sum files)))
    (lambda ()
      (loop
        (let ((new-date-sum (files-date-sum files)))
          (when (not (= new-date-sum date-sum))
            (setq date-sum new-date-sum
                  ;; We recompute the files to watch here because
                  ;; computing them every time through the loop caused
                  ;; too much cpu usage while idle.
                  files (files-to-watch system))
            (lparallel.queue:push-queue :file-changed *control-queue*)))
        (sleep 0.5)
        (multiple-value-bind (elt foundp)
            (lparallel.queue:peek-queue *control-queue*)
          (when foundp
            (ecase elt
              (:shutdown (return))
              (:file-changed ()))))))))

(defun queue-priority-event (queue priority)
  "Empty the queue and return the highest priority item.

Blocks if the queue is empty."
  (do  ((item (lparallel.queue:pop-queue queue)
              (lparallel.queue:try-pop-queue queue))
        task)
       ((null item) task)
    (when (> (funcall priority item) (funcall priority task))
      (setq task item))))

(defun control-priority (item)
  (let ((priorities '(:shutdown 1 :file-changed 0)))
    (getf priorities item -1)))

(defun flush-channel (channel)
  (loop (multiple-value-bind (res foundp)
            (lparallel:try-receive-result channel)
          (declare (ignore res))
          (unless foundp (return ())))))

(defun make-test-runner (system channel)
  (lambda ()
    (loop
      (flush-channel channel)
      (ecase (queue-priority-event *control-queue* #'control-priority)
        (:shutdown
         (lparallel.queue:push-queue :shutdown *control-queue*)
         (return))
        (:file-changed
         (lparallel.queue:with-locked-queue *in-progress-queue*
           (cond ((lparallel.queue:peek-queue/no-lock *in-progress-queue*)
                  (lparallel.queue:with-locked-queue *restart-queue*
                    (unless (lparallel.queue:peek-queue/no-lock *restart-queue*)
                      (lparallel.queue:push-queue/no-lock t *restart-queue*))))
                 (t
                  (lparallel.queue:push-queue/no-lock t *in-progress-queue*)
                  (lparallel:submit-task channel #'autorun-test-system system)))))))))

(defun start-autorun (system)
  (let ((chan (ensure-system-channel system)))
    (lparallel.kernel:submit-task
     chan
     (make-system-watcher system))
    (lparallel.kernel:submit-task
     chan
     (make-test-runner system chan))))

(defun shutdown-autorun (system)
  (let ((chan (ensure-system-channel system)))
    (lparallel.kernel:submit-task
     chan
     (lambda ()
       (lparallel.queue:push-queue :shutdown *control-queue*)
       (lparallel:end-kernel)
       (remhash system *system-channels*)))))

(defun autorun
    (&key (system (intern (package-name *package*) :keyword)))
  "Toggle whether asdf:test-system is automatically run when source is touched.

System should be a string or symbol designating a system name. Autorun
also binds *handle-errors* to t.

Touching files of a test-system will also trigger the tests if the
asdf setup described in YARTY's readme is used."
  (unless (keywordp system)
    (setq system (intern (string-upcase system) :keyword)))
  (cond ((gethash system *system-channels*)
         (format t "Terminating autorun for system ~(~A~)." system)
         (shutdown-autorun system))
        ((asdf:find-system system ())
         (format t "Starting autorun for system ~(~A~)." system)
         (start-autorun system))
        (t
         (format t "Failure to start autorun: system \"~(~A~)\" not found."
                 system))))
