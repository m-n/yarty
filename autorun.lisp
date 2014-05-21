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
                       (cl-fam:*fam* . (cl-fam:fam-open "yarty" ()))
                       (*package* . ,*package*)
                       (*restart-queue* . ,restart-queue)
                       (*in-progress-queue* . ,in-progress-queue)
                       (*control-queue* . ,control-queue)))))
              (lparallel:make-channel)))))

(defun test-system (&optional (system (intern (package-name *package*) :keyword)))
  (let ((*test-system* system))
    (lparallel.queue:with-locked-queue *in-progress-queue*
      (unless (lparallel.queue:peek-queue/no-lock *in-progress-queue*)
        (lparallel.queue:push-queue/no-lock t *in-progress-queue*)))
    (unwind-protect (asdf:test-system system)
      (lparallel.queue:try-pop-queue *in-progress-queue*))))

(defun make-fam-handler (location)
  (lambda ()
    (cl-fam:fam-monitor-directory location)
    (loop
      (let ((event (cl-fam:fam-next-event t))
            (elt (lparallel.queue:peek-queue *control-queue*)))
        (case elt
          (:shutdown
           (lparallel.queue:push-queue :shutdown *control-queue*)
           (cl-fam:fam-close)
           (return))
          (t
           (lparallel.queue:push-queue event *control-queue*)))))))

(defun queue-priority-event (queue priority)
  "Empty the queue and return the highest priority item.

Blocks if the queue is empty."
  (do  ((item (lparallel.queue:pop-queue queue)
              (lparallel.queue:try-pop-queue queue))
        task)
       ((null item) task)
    (when (> (funcall priority item) (funcall priority task))
      (setq task item))))

(defun test-priority (item)
  (let ((priorities '(:shutdown 1 :fam-changed 0)))
    (getf priorities item -1)))

(defun make-test-runner (system channel)
  (lambda ()
    (loop
      (case (queue-priority-event *control-queue* #'test-priority)
        (:shutdown
         (lparallel.queue:push-queue :shutdown *control-queue*)
         (return))
        (:fam-changed
         (if (lparallel.queue:peek-queue *in-progress-queue*)
             (lparallel.queue:with-locked-queue *restart-queue*
               (unless (lparallel.queue:peek-queue/no-lock *restart-queue*)
                 (lparallel.queue:push-queue/no-lock t *restart-queue*)))
             (lparallel:submit-task channel #'test-system system)))))))

(defun start-test-on-change (system location)
  (let ((chan (ensure-system-channel system)))
    (lparallel.kernel:submit-task
     chan
     (make-fam-handler location))
    (lparallel.kernel:submit-task
     chan
     (make-test-runner system chan))))

(defun shutdown-test-on-change (system)
  (let ((chan (ensure-system-channel system)))
    (lparallel.kernel:submit-task
     chan
     (lambda ()
       (lparallel.queue:push-queue :shutdown *control-queue*)
       (lparallel:end-kernel)
       (remhash system *system-channels*)))))

(defun test-on-change
    (&key (system (intern (package-name *package*) :keyword))
          (location (directory-namestring
                     (asdf/system:system-source-file
                      system))))
  "Toggle whether asdf:test-system is automatically run when source is touched.

System should be a string or symbol designating a system
name. Location defaults to the directory containing the
asdf:system-source-file."
  (unless (keywordp system)
    (setq system (intern (string-upcase system) :keyword)))
  (if (gethash system *system-channels*)
      (shutdown-test-on-change system)
      (start-test-on-change system location)))
