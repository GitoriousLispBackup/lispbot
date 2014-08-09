 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; __                               ____            __             ;;
;;/\ \       __                    /\  _`\         /\ \__          ;;
;;\ \ \     /\_\    ____  _____    \ \ \L\ \    ___\ \ ,_\         ;;
;; \ \ \  __\/\ \  /',__\/\ '__`\   \ \  _ <'  / __`\ \ \/         ;;
;;  \ \ \L\ \\ \ \/\__, `\ \ \L\ \   \ \ \L\ \/\ \L\ \ \ \_        ;;
;;   \ \____/ \ \_\/\____/\ \ ,__/    \ \____/\ \____/\ \__\       ;;
;;    \/___/   \/_/\/___/  \ \ \/      \/___/  \/___/  \/__/       ;;
;;                          \ \_\                                  ;;
;;                           \/_/                                  ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :lispbot)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ;;
;; Configuration variables ;;
;;                         ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *default-data-directory*
  (merge-pathnames ".lispbot/" (user-homedir-pathname))
  "the default place where the bot and plugins will create their files")

(defvar *debug* nil
  "print debug output?")

 ;;;;;;;;;;;;;;
;;            ;;
;; Bot Class  ;;
;;            ;;
 ;;;;;;;;;;;;;;

(defclass bot ()
  ((connection
    :initform nil
    :reader connection
    :type (or null irc:connection)
    :documentation "internal connection representation")
   (channels        ;; list of strings. TODO: add channel class, that for example keeps track of users
    :initform nil
    :reader channels
    :documentation "list of channels the bot has joined")
   (plugins
    :initform nil
    :accessor plugins
    :documentation "set of plugins for this bot")
   (nick
    :initform "lispbot"
    :initarg :nick
    :reader nick
    :type string
    :documentation "the nickname of the bot")
   (quit-message
    :initform "The Lispbot: gitorious.org/lispbot"
    :initarg :quit-message
    :accessor quit-message
    :type string
    :documentation "A short message to say goodbye")
   (data-dir
    :initform *default-data-directory*
    :initarg :data-dir
    :accessor data-dir
    :documentation "the directory where the bot and plugins will store there files")
   (command-prefix
    :initform "!"
    :initarg :command-prefix
    :accessor command-prefix
    :documentation "Control what the bot recognizes as commands")
   (plugin-blacklist
    :initform nil
    :initarg :plugin-blacklist
    :accessor plugin-blacklist
    :documentation "Alist of (channel . plugins) pairs. Plugins can also be :all")
   (plugin-whitelist
    :initform nil
    :initarg :plugin-whitelist
    :accessor plugin-whitelist
    :documentation "Alist of (channel . plugins) pairs.
This is applied after the blacklist")
   (bot-lock
    :initform (bt:make-recursive-lock "global bot lock")
    :accessor bot-lock
    :documentation "The big bot write lock")
   (message-channel
    :initform (make-instance 'channel)
    :accessor message-channel
    :documentation "Messages received from the network but not processed yet")
   (control-thread
    :accessor control-thread)
   (read-thread
    :accessor read-thread))
  (:documentation "a irc bot"))

(defgeneric start (bot server &optional port)
  (:documentation "connect to server and enter read loop"))

(defgeneric stop (bot)
  (:documentation "disconnect from server"))

(defgeneric send (lines to bot &key actionp)
  (:documentation "send a privmsg to `to' (which can be a chan or a user).
If `actionp' is true, use the ctcp action command"))

(defgeneric join (bot channel)
  (:documentation "Join a channel"))

(defgeneric leave (bot channel &key message)
  (:documentation "Leave a channel"))

(defgeneric get-topic (bot channel)
  (:documentation "Return the channel topic"))

(defgeneric set-topic (bot channel topic)
  (:documentation "Set the channel topic"))

(defgeneric add-plugin (plugin bot)
  (:documentation "add `plugin' to the bot if there isn't already a plugin with the same name.
Returns the bot as first value. The second value is either `t' if the plugin was really
inserted, or `nil' otherwise."))

(defgeneric remove-plugin (plugin-name bot)
  (:documentation "remove the plugin with the name `plugin-name' from the bot.
Returns the bot as first value. The second value is either `t' if the plugin was really
removed, or `nil' otherwise."))

(defgeneric find-plugin (plugin-name bot)
  (:documentation "return the plugin with `plugin-name' if it exists and nil otherwise"))


(defgeneric add-plugins (bot &rest plugins)
  (:documentation "add `plugins' to the bot. Plugins can be instances of classes derived
from PLUGIN, names of classes derived from PLUGIN or lists of those including lists of
lists of ..."))

;; The next two functions rely on the context of *last-message*. They should only
;; be called from an implementation of handle-event or a command.

(defgeneric reply (texts &optional to-user-p)
  (:documentation "can be used by plugins to let the bot say something. `texts' can be a list of strings or a string.
If `to-user-p' is t, address the user of the last received message directly"))

(defgeneric action (texts)
  (:documentation "can be used by plugins to write a /me message"))

(defmacro with-bot-lock (bot &body body)
  `(bt:with-recursive-lock-held ((bot-lock ,bot))
     ,@body))

 ;;;;;;;;;;;;;;;;;
;;               ;;
;; Plugin Class  ;;
;;               ;;
 ;;;;;;;;;;;;;;;;;

(defclass plugin ()
  ((name
    :initform "noname"
    :accessor name
    :initarg :name)
   (bot
    :initform nil
    :reader bot
    :initarg :bot))
  (:documentation "all plugins must derive from this class"))

(defgeneric handle-event (plugin event)
  (:documentation "plugins can implement this for the various events"))

(defgeneric help (plugin)
  (:documentation "called when the user requests help for a plugin"))

(defparameter *last-message* nil
  "this is bound to the last message to the bot during the execution of commands
or the `handle-event' methods of plugins")

(defparameter *command-argstring* nil
  "bound to the argument of a command before splitting")

(defun help-for-commands (plugin)
  "Print the lambda-lists and docstrings of all commands from `plugin'
To be used in the `help' method."
  (reply (mappend (curry #'help-for-command (bot plugin))
                  (commands plugin)) nil))

(defclass command ()
  ((name
    :initarg :name
    :accessor command-name
    :documentation "The thing, a query will be matched against. Can be a string or
a symbol")
   (function
    :initarg :function
    :accessor command-function
    :documentation "The actual function to call when the command is run")
   (lambda-list
    :initarg :lambda-list
    :accessor command-lambda-list
    :documentation "The lambda list of the function")
   (doc-string
    :initform nil
    :initarg :doc
    :accessor command-doc-string
    :documentation "The documentation for this command. Can for example be used in
a help plugin")))

(defun make-command (name lambda-list function &key doc)
  "create a new command `name' can be a string or a symbol"
  (make-instance 'command
                 :name (string-downcase (string name))
                 :function function
                 :lambda-list lambda-list
                 :doc doc))

(defgeneric commands (plugin)
  (:documentation "return a list of all commands of the plugin"))

(defgeneric add-command (plugin command)
  (:documentation "add or replace a command to the plugin"))

(defgeneric remove-command (plugin command)
  (:documentation "remove a command from the plugin"))

(defgeneric run-command (command &rest args)
  (:documentation "run this command"))

(defgeneric find-command (plugin command)
  (:documentation "return a command named by `command' from `plugin', or nil
if there is no such command"))

(defgeneric command-matches-p (command-designator command)
  (:documentation "return true if the command-name of `command'
matches the `command-designator' which can be a string, a symbol
of an instance of the command class."))

(defmacro defcommand (name ((plvar plclass) &rest args) &body body)
  "add a new command to the plugin `plclass'. The name can be a string or a symbol.
If the first element of body is a string, it will be used as docstring for the
new command."
  (let* ((doc (if (stringp (first body)) (first body) nil))
         (body (if (stringp (first body)) (rest body) body))
         (fun `(lambda (,plvar ,@args) ,@body)))
    `(add-command ',plclass
                  (make-command ',name ',args ,fun :doc ,doc))))

 ;;;;;;;;;;;;;;;
;;             ;;
;; User Class  ;;
;;             ;;
 ;;;;;;;;;;;;;;;

;; 'user interface' is to be implemented

(defclass user ()
  ((nick
    :initarg :nick
    :accessor nick)                     ; TODO: setf nick should be /nick-aware
   (username
    :initarg :username
    :reader name)
   (host
    :initarg :host
    :reader host)))

(defgeneric user-equal (user1 user2)
  (:documentation "compare users"))

(defgeneric hostmask (user)
  (:documentation "return a host mask of the form nick!username@host"))

(defun userp (object)
  (eq (type-of object) 'user))

(defun selfp (user bot)
  (string= (nick bot) (nick user)))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ;;
;; Internal implementation ;;
;;                         ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *hooks*
  '(irc:irc-rpl_luserme-message
    irc:irc-privmsg-message
    irc:irc-join-message
    irc:irc-part-message))

(defgeneric handle-irc-message (type bot message))

(defmethod handle-irc-message ((type (eql 'irc:irc-rpl_luserme-message)) bot msg)
  (declare (ignore msg))
  (dolist (chan (channels bot))
    (irc:join (connection bot) chan)))

(defmethod handle-irc-message ((type (eql 'irc:irc-privmsg-message)) bot msg)
  (let ((message (make-event msg bot)))
    (handle-priv-message message)))

(defmethod handle-irc-message ((type (eql 'irc:irc-join-message)) bot msg)
  (let ((*last-message* (make-event msg bot)))
    (if (selfp (user *last-message*) bot)
        (with-bot-lock bot
          (pushnew (channel *last-message*) (slot-value bot 'channels) :test #'string=))
        (call-event-handlers *last-message*))))

(defmethod handle-irc-message ((type (eql 'irc:irc-part-message)) bot msg)
  (let ((*last-message* (make-event msg bot)))
    (if (selfp (user *last-message*) bot)
        (with-bot-lock bot
          (removef (slot-value bot 'channels) (channel *last-message*) :test #'string=))
        (call-event-handlers *last-message*))))

(defun handle-errors-in-command (bot command err)
  (reply (cons (format nil "error in ~a: ~a | Usage:"
                       (command-name command) err)
               (help-for-command bot command))))

(defun string-splitter ()
  (let ((separators '(#\' #\"))
        (last-sep nil)
	(escaped nil))
    (lambda (x)
      (cond
	(escaped (setf escaped nil))
        ((and last-sep
              (char= x last-sep)) (progn (setf last-sep nil) t))
        ((member x separators) (progn
                                 (if (not last-sep)
                                     (progn (setf last-sep x) t))))
	((char= x #\\) (setf escaped t) nil)
	((and (char= x #\Space)
	      (not last-sep))
	 t)
	(t nil)))))

(defun split-string (string)
  (let ((list (split-sequence:split-sequence-if (string-splitter)
					   string
					   :remove-empty-subseqs nil)))
    list))

(defun plugin-allowed-p (blacklist whitelist plugin)
  (labels ((is-plugin (arg)
             (typecase arg
               (string (string= (name plugin) arg))
               (t (eq arg (type-of plugin))))))
    (let ((blacklisted (or (member :all blacklist)
                           (find-if #'is-plugin blacklist)))
          (whitelisted (find-if #'is-plugin whitelist)))
      (or (not blacklisted) whitelisted))))

(defun merge-policy-list (channel list)
  (let ((all  (cdr (assoc :all list)))
        (chan (cdr (assoc channel list :test #'string=))))
    (concatenate 'list (ensure-list all) (ensure-list chan))))

(defun bot-channel-policy (bot channel plugin)
  (let ((blacklist (merge-policy-list channel (plugin-blacklist bot)))
        (whitelist (merge-policy-list channel (plugin-whitelist bot))))
    (plugin-allowed-p blacklist whitelist plugin)))


(defun run-command-by-name (bot command &rest args)
  (when-let (plugin (find-if (rcurry #'find-command command)
                             (plugins bot)))
    (unless (and (typep *last-message* 'channel-message)
                 (not
                  (bot-channel-policy bot (channel *last-message*) plugin)))
      (handler-case
          (apply #'run-command (find-command plugin command) (cons plugin args))
        (condition (err)
          (handle-errors-in-command bot (find-command plugin command) err))))))

(defun call-commands (message command)
  (let* ((*last-message* message)
         (args-white (split-string command))
         (args (remove-if #'alexandria:emptyp args-white))
         (*command-argstring* (format nil "~{~a~^ ~}" (subseq args-white (1+ (position-if-not #'alexandria:emptyp (rest args-white)))))))
    (apply #'run-command-by-name (bot message) (first args) (rest args))))

(defun call-event-handlers (event)
  (dolist (p (plugins (bot event)))
    (unless (and (typep event 'channel-message)
                 (not
                  (bot-channel-policy (bot event) (channel event) p)))
      (handle-event p event))))

(defun is-message-a-command-p (nick prefix text)
  (ppcre:scan-to-strings
   (concatenate 'string "^(" nick "\\W+"
                "|" prefix ")(.*)")
   text))

(defun handle-priv-message (message)
  (with-slots (bot text sender) message
    (when (not (selfp sender bot)) ;; never respond to myself!!
      (let ((*last-message* message))
        (call-event-handlers message))
      (if (typep message 'channel-message)
	  (multiple-value-bind (match msg)
	      (is-message-a-command-p (nick bot)
                                      (command-prefix bot)
                                      text)
	    (when match (call-commands message (elt msg 1))))
	  (call-commands message text)))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ;;
;; Implementations of generics  ;;
;;                              ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *default-irc-port* 6667)

(defun read-message-loop (bot)
  (loop while
        (when-let (con (connection bot))
          (handler-case
              (bt:with-timeout (3)
                (irc:read-message con))
            (condition () t)))))

(defun run-read-thread (bot)
  (unwind-protect
       (read-message-loop bot)
    (let ((connection (connection bot)))
      (unless (null (connection bot)) ;; stop was called on this bot.
        (setf (slot-value bot 'connection) nil)
        (irc:quit connection (quit-message bot))))))

(defun run-control-thread (bot)
  (loop for x = (channel-recv (message-channel bot))
        while (case x
                (quit nil)
                (otherwise
                 (handle-irc-message (car x) bot (cdr x))
                 t))))

(defun make-hook (bot hook)
  (lambda (msg)
    (channel-send (message-channel bot)
                (cons hook msg))))

(defun add-hook (bot hook)
  (irc:add-hook (connection bot) hook (make-hook bot hook)))

(defmethod start ((bot bot) server &optional (port *default-irc-port*))
  (if-let (conn (irc:connect
		 :server server
		 :port port
		 :nickname (nick bot)
		 :logging-stream *debug*))
    (progn
      (setf (slot-value bot 'connection) conn)
      (mapc (curry #'add-hook bot) *hooks*)
      (setf (read-thread bot)
            (bt:make-thread (lambda () (run-read-thread bot)) :name "bot read thread"))
      (setf (control-thread bot)
       (bt:make-thread (lambda () (run-control-thread bot)) :name "bot control thread")))
    (error "could not connect to server")))

(defmethod stop ((bot bot))
  (if-let (con (connection bot))
    (progn
      (setf (slot-value bot 'connection) nil)
      (channel-send-out-of-band (message-channel bot) 'quit)
      (loop while (bt:thread-alive-p (control-thread bot)) do (sleep 1))
      (loop while (bt:thread-alive-p (read-thread bot)) do (sleep 1))
      (channel-clear (message-channel bot))
      (irc:quit con (quit-message bot)))
    (error "The bot was not started.")))

(defmethod send (lines to (bot bot) &key actionp)
  (with-bot-lock bot
    (let ((connection (connection bot))
          (to (if (userp to) (nick to) to))
          (lines (if actionp
                     (actionize-lines lines)
                     (ensure-list lines))))
      (when connection
        (loop for msg in lines
              for i from 1
              do (progn
                   (irc:privmsg connection to msg)
                   (when (= (mod i 3) 0)
                     (sleep 1))))))))

(defmethod join ((self bot) channel)
  (with-bot-lock self
    (unless (find channel (channels self) :test #'string=)
      (irc:join (connection self) channel)
      t)))

(defmethod leave ((self bot) channel &key message)
  (with-bot-lock self
    (when (find channel (channels self) :test #'string=)
      (irc:part (connection self) channel (or message (quit-message self)))
      t)))

(defmethod get-topic ((self bot) channel)
  (with-bot-lock self
    (when (find channel (channels self) :test #'string=)
      (irc:topic (irc:find-channel (connection self) channel)))))

(defmethod set-topic ((self bot) channel topic)
  (with-bot-lock self
    (when (find channel (channels self) :test #'string=)
      (irc:topic- (connection self) channel topic))))

(defmethod handle-event ((plugin plugin) (event event))
  (declare (ignore plugin event))
  nil)

(defgeneric reply-to-event (message lines &optional to-user-p))

(defun address-user (lines nick)
  (mapcar (lambda (x)
	    (format nil "~a: ~a" nick x))
	  (ensure-list lines)))

(defmethod reply-to-event ((message channel-message) lines &optional to-user-p)
  (send (if to-user-p
	    (address-user lines (nick (sender message)))
	    lines)
	(channel message) (bot message)))

(defmethod reply-to-event ((message query-message) lines &optional to-user-p)
  (declare (ignore to-user-p))
  (send lines (sender message) (bot message)))

(defmethod reply-to-event ((event join-event) lines &optional to-user-p)
  (send (if to-user-p
	    (address-user lines (nick (user event)))
	    lines)
	(channel event) (bot event)))

(defmethod reply-to-event ((event (eql nil)) lines &optional to-user-p)
  (declare (ignore event lines to-user-p)))

(defmethod reply (lines &optional to-user-p)
  (reply-to-event *last-message* lines to-user-p))

(defparameter *ctcp-delimiter* (code-char 1))

(defun actionize-lines (lines)
  (mapcar (lambda (x)
	    (format nil "~aACTION ~a~@*~a~*" *ctcp-delimiter* x))
	  (ensure-list lines)))

(defmethod action (lines)
  (reply-to-event *last-message* (actionize-lines lines)))

(defmethod help ((plugin plugin))
  (declare (ignore plugin))
  :unimplemented-help)

(defmethod user-equal ((user1 user) (user2 user))
  (and (string-equal (nick user1) (nick user2))
       (string-equal (host user1) (host user2))
       (string-equal (name user1) (name user2))))

;; TODO: also specialize the hostmask function on the bot class
(defmethod hostmask ((user user))
  (format nil
	  "~a!~a@~a"
	  (nick user)
	  (name user)
	  (host user)))

(defmethod add-plugin ((plugin plugin) (bot bot))
  (values bot (unless (find-plugin (name plugin) bot)
                (push plugin (plugins bot))
                t)))

(defmethod remove-plugin (name (bot bot))
  (values bot
          (when (find-plugin name bot)
            (setf (plugins bot)
                  (delete-if (plugin-name= name) (plugins bot)))
            t)))

(defmethod find-plugin (name (bot bot))
  (find-if (plugin-name= name) (plugins bot)))

(defun plugin-name= (name)
  (lambda (plugin) (string= (name plugin) name)))

(defmethod add-plugins ((self bot) &rest plugins)
  (with-bot-lock self
    (labels ((make-plugins (plugins)
               (loop for p in plugins appending
                     (cond
                       ((listp p) (make-plugins p))
                       ((symbolp p) (ensure-list (make-instance p :bot self)))
                       ((subtypep (type-of p) 'plugin) (progn
                                                         (setf (slot-value p 'bot) self)
                                                         (ensure-list p)))
                       (t (error "strange plugin: ~a" p))))))
      (reduce #'add-plugin (make-plugins plugins) :initial-value self
              :from-end t))))

(defmethod initialize-instance :after ((bot bot) &key plugins channels)
  (apply #'add-plugins bot plugins)
  (setf (slot-value bot 'channels) (ensure-list channels)))

;; Plugins and commands
(defun ensure-plugin-symbol (thing)
  (if (symbolp thing) thing (type-of thing)))

(defmethod commands (self)
  (get (ensure-plugin-symbol self) :commands))

(defmethod add-command (plugin (cmd command))
  (setf (get (ensure-plugin-symbol plugin) :commands)
        (cons cmd (delete cmd (get (ensure-plugin-symbol plugin) :commands)
                          :test #'command-matches-p))))

(defmethod remove-command (plugin cmd)
  (setf (get (ensure-plugin-symbol plugin) :commands)
        (delete cmd (get (ensure-plugin-symbol plugin) :commands)
                :test #'command-matches-p)))

(defmethod run-command ((self command) &rest args)
  (apply (command-function self) args))

(defmethod find-command (plugin command)
  (find command (commands plugin) :test #'command-matches-p))

(defmethod command-matches-p ((s string) (self command))
  (string= (command-name self) s))

(defmethod command-matches-p ((s symbol) (self command))
  (string-equal (command-name self) (string-downcase (symbol-name s))))

(defmethod command-matches-p ((other command) (self command))
  (string-equal (command-name self)
                (command-name other)))

(defun help-for-command (bot command)
  (let ((doclines (split-sequence:split-sequence
                   #\Newline (command-doc-string command)
                   :remove-empty-subseqs t)))
    (cons (format nil "~a~a~{~^ ~(~a~)~}: ~a" (command-prefix bot)
                  (command-name command)
                  (command-lambda-list command)
                  (or (first doclines) "no help available"))
          (rest doclines))))

(defmethod print-object ((object bot) s)
  (print-unreadable-object (object s :type t)
    (princ (nick object) s)))

(defmethod print-object ((object plugin) s)
  (print-unreadable-object (object s :type t)
    (princ (name object) s)))
