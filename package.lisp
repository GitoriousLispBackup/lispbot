(defpackage :lispbot
  (:use :cl)
  (:nicknames :bot)
  (:export :bot
	   :make-bot
	   :add-plugins
	   :plugins
	   :channels
	   :nick

	   :connect
	   :disconnect
	   :read-loop
	   :reply
	   :action
	   :send

	   :plugin
	   :name
	   :defcommand
	   :help
	   :connected
	   :handle-event
	   :*last-message*

	   :random-entry

	   :event
	   :message
	   :time
	   :text
	   :sender
	   :channel-message
	   :channel
	   :query-message
	   :user-event
	   :join-event
	   :part-event

	   :user
	   :user-equal
	   :hostmask))
