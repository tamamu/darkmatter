(in-package :cl)
(defpackage darkmatter.async
  (:use :cl :websocket-driver :bordeaux-threads)
  (:import-from :alexandria
                :if-let)
  (:export :attach-runtask
           :check-task
           :attach-task-thread
           :get-task-output
           :set-task-kill
           :get-task-thread
           :send-recv
           :exists-task))
(in-package :darkmatter.async)

(defvar *taskbase*
  (make-hash-table :test #'equal))

(defstruct task-entity
  (body nil)
  (output nil)
  (should-kill-p nil)
  (exit-p nil)
  (thread nil))

(defun regist-task (id task)
  (setf (gethash id *taskbase*) task))

(defun attach-task-thread (id)
  (setf (task-entity-thread (gethash id *taskbase*))
        (make-thread (task-entity-body (gethash id *taskbase*)))))

(defun get-task-thread (id)
  (task-entity-thread (gethash id *taskbase*)))

(defun exists-task (id)
  (and (gethash id *taskbase* nil)
       (get-task-thread id)))

(defun set-task-kill (id)
  (setf (task-entity-should-kill-p (gethash id *taskbase*)) t))

(defun set-task-exit (id)
  (setf (task-entity-exit-p (gethash id *taskbase*)) t))

(defun get-task-exit (id)
  (task-entity-exit-p (gethash id *taskbase*)))

(defun should-task-kill-p (id)
  (task-entity-should-kill-p (gethash id *taskbase*)))

(defun set-task-output (id value)
  (setf (task-entity-output (gethash id *taskbase*)) value))

(defun delete-task (id)
  (remhash id *taskbase*))

(defun get-task-output (id)
  (task-entity-output (gethash id *taskbase*)))

(defun macroexpand-all (form)
  (let ((form (macroexpand form)))
    (cons (car form) (mapcar #'macroexpand (cdr form)))))

(defmacro checkpoint (value &optional (kill nil))
  "Alert client that has the id to progress"
  `(if (should-task-kill-p $<id>)
       (progn
         (unless (null ,kill)
           (set-task-output $<id> ,kill))
         (return))
       (set-task-output $<id> ,value)))

(defmacro runtask (initial &body body)
  "Run task which takes an id asynchronous"
  (make-task-entity
    :output initial
    :body
    `(let (($<id> id-form)
           ($<result> nil))
       (lambda ()
         (let ((*standard-output* (make-string-output-stream))
               (*error-output* (make-string-output-stream)))
           (handler-case
             (unwind-protect
               (setf $<result> (progn ,@body))
               (unless (null $<result>)
                 (set-task-output $<id> $<result>)
                 (set-task-exit $<id>)))
             (error (c) (progn
                          (set-task-output $<id>
                                           (format nil "<pre>~A~A~A</pre>"
                                                   (get-output-stream-string *standard-output*)
                                                   (get-output-stream-string *error-output*)
                                                   c))
                          (set-task-exit $<id>)))))))))

(defun symbol= (a b)
  (and (symbolp a)
       (symbolp b)
       (string= (symbol-name a)
                (symbol-name b))))

(defun attach-runtask (form)
  (subst 'runtask 'runtask form :test #'symbol=))

(defun attach-checkpoint (form)
  (subst 'checkpoint 'checkpoint form :test #'symbol=))

(defun attach-id (id form)
  (subst id 'id-form form :test #'symbol=))

(defun send-recv (ws id)
  "Alert to client that has the id"
  (if-let (task (exists-task id))
          (progn
            (send ws (jsown:to-json
                       `(:obj ("message" . "update")
                              ("output" . ,(prin1-to-string (get-task-output id))))))
            (when (or (get-task-exit id)
                      (should-task-kill-p id))
              (progn
                (delete-task id)
                (send ws (jsown:to-json
                           `(:obj ("message" . "exit")))))))
          (send ws (jsown:to-json
                     `(:obj ("message" . "exit"))))))

(defmacro ->$ (value &body forms)
  (reduce (lambda (acc form)
            (append form (list acc)))
          forms :initial-value value))

(defun attach-task (id task)
  (->$ (task-entity-body task)
       (attach-checkpoint)
       (macroexpand-all)
       (attach-id id)
       (eval)
       (setf (task-entity-body task))))

(defun check-task (obj id symbols)
  (when (task-entity-p obj)
    (let ((task obj))
      (unless (null (exists-task id))
        (progn
          (set-task-kill id)
          (join-thread (get-task-thread id))))
      (attach-task id task)
      (regist-task id task)
      (let ((task `(:obj ("message" . "alert_start")
                         ("id" . ,id))))
        (setf (jsown:val task "symbols") symbols)))))
