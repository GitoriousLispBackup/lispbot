(in-package :lispbot)

(defclass test-bot (bot)
  ((luser :initform (make-instance 'user :nick "luser") :accessor test-bot-luser)
   (topic :initform "There is no topic" :accessor test-bot-topic)))

(defparameter *testbot-commands*
  '(("/nick" . test-bot-/nick)
    ("/topic" . test-bot-/topic)))

(defmethod start ((bot test-bot) _ &optional __)
  (declare (ignore _ __))
  (format t "Lispbot testing repl: Use /exit to leave~%")
  (let ((channel "repl"))
   (loop
     (princ "testbot-repl> ")
     (let ((l (read-line)))
       (if (equal l "/exit")
           (return)
           (test-bot-handle-msg bot l channel))))))

(defun test-bot-handle-msg (bot l chan)
  (let ((args (split-string l)))
    (if-let (cmd (assoc (first args) *testbot-commands* :test #'string-equal))
      (apply (cdr cmd) bot (rest args))
      (handle-priv-message (make-instance 'channel-message
                                          :text l
                                          :from (test-bot-luser bot)
                                          :bot bot
                                          :channel chan)))))

(defmethod stop ((bot test-bot))
  (declare (ignore bot))
  (error "stop not defined for test-bot"))

(defmethod send (lines to (bot test-bot) &key actionp)
  (declare (ignore to))
  (dolist (l (if actionp (actionize-lines lines) (ensure-list lines)))
    (princ l) (terpri)))

(defmethod leave ((self test-bot) _ &key message)
  (declare (ignore self _ message))
  (values))

(defmethod join ((self test-bot) channel)
  (declare (ignore self channel))
  (values))

(defmethod get-topic ((self test-bot) channel)
  (test-bot-topic self))

(defmethod set-topic ((self test-bot) channel topic)
  (setf (test-bot-topic self) topic))

(defun start-test-bot (plugins)
  (start (make-instance 'test-bot :plugins plugins) nil))

;;; internal commands

(defun test-bot-/nick (bot nick)
  (format t "changed nick to ~a~%" nick)
  (setf (nick (test-bot-luser bot)) nick))

(defun test-bot-/topic (bot &optional topic)
  (if topic
      (progn
        (set-topic bot "" topic)
        (format t "Topic changed to ~a~%" topic))
      (format t "~a~%" (get-topic bot ""))))
