;; A Plugin that prints a short help about the bot and other plugins

(defpackage :lispbot.help
  (:use :cl :lispbot)
  (:export :help-plugin))

(in-package :lispbot.help)

(defclass help-plugin (plugin)
  ((name :initform "hilfe")))

(defun helpfull-plugin-p (plugin)
  (let ((bot::*last-message* nil))
    (not (eq (help plugin) :unimplemented-help))))

(defcommand help ((p help-plugin) &optional what)
  (if what
      (loop for plugin in (plugins (bot p))
	 when (string= (name plugin) what)
	 do (help plugin))
      (reply (list (concatenate 'string "the following plugins are available: "
				(format nil "~{~a~^, ~}" (loop for x in (plugins (bot p))
							    when (helpfull-plugin-p x) collect (name x))))
		   "get more help with \"help <plugin>\""))))
