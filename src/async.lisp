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
  (cell "")
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
         (unwind-protect
           (setf $<result> (progn ,@body))
           (unless (null $<result>)
             (set-task-output $<id> $<result>)
             (set-task-exit $<id>)))))))

(defun replace-all (src dest form)
  (if (listp form)
    (mapcar
      (lambda (obj)
        (if (listp obj)
            (replace-all src dest obj)
            (if (and (symbolp obj)
                     (equalp (symbol-name obj)
                             (symbol-name src)))
                dest
                obj)))
          form)
    form))

(defun attach-runtask (form)
  (replace-all 'runtask 'runtask form))

(defun attach-checkpoint (form)
  (replace-all 'checkpoint 'checkpoint form))

(defun attach-id (id form)
  (replace-all 'id-form id form))

(defun send-recv (ws id)
  "Alert to client that has the id"
  (if-let (task (exists-task id))
          (progn
            (send ws (jsown:to-json
                       `(:obj ("message" . "update")
                              ("output" . ,(get-task-output id)))))
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

(defun startswith (a b)
  (let ((alen (length a))
        (blen (length b)))
    (if (>= alen blen)
      (string= (subseq a 0 blen) b)
      nil)))

(defun attach-task (id task)
  (->$ (task-entity-body task)
       (attach-checkpoint)
       (macroexpand-all)
       (attach-id id)
       (eval)
       (setf (task-entity-body task))))

(defun check-task (cell id obj)
  (if (task-entity-p obj)
      (let ((task obj))
        (unless (null (exists-task id))
          (progn
            (set-task-kill id)
            (join-thread (get-task-thread id))))
        (attach-task id task)
        (regist-task id task)
        (setf (task-entity-cell task) cell)
        `(:obj ("message" . "alert_start")
               ("id" . ,id)))
      nil))

