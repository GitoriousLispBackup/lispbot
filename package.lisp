(defpackage :lispbot
  (:use :cl :alexandria)
  (:nicknames :bot)
  (:export :*default-data-directory*
           :*debug*

           :bot
           :make-bot
           :plugins
           :channels
           :nick
           :data-dir
           :command-prefix

           :add-plugin
           :remove-plugin
           :find-plugin
           :add-plugins

           :reply
           :action
           :send
           :start
           :stop
           :join
           :leave

           :plugin
           :name
           :defcommand
           :help
           :help-for-commands
           :handle-event
           :*last-message*
           :*command-argstring*

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
           :host
           :user-equal
           :hostmask
           :test-bot
           :plugin-blacklist
           :plugin-whitelist))
