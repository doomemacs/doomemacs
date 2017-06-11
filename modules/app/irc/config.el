;;; app/irc/config.el -*- lexical-binding: t; -*-

;;
;; Config
;;

(defvar +irc-left-padding 13
  "TODO")

(defvar +irc-disconnect-hook nil
  "TODO")

(defvar +irc-bot-list '("fsbot" "rudybot")
  "TODO")

(defvar +irc-time-stamp-format "%H:%M"
  "TODO")

(defvar +irc-notifications-watch-strings nil
  "TODO")

(def-setting! :irc (server letvars)
  "Registers an irc server for circe."
  `(progn
     (push ',(cons server letvars) +irc--accounts)
     (push ',(cons server letvars) circe-network-options)))

(defvar +irc--accounts nil)


;;
;; Plugins
;;

(def-package! circe
  ;; need a low impact function just to force an eval
  :commands (circe circe--version)
  :init
  (setq circe-network-defaults nil)
  :config
  (set! :evil-state 'circe-channel-mode 'emacs)

  (defun +irc|circe-format-padding (left right)
    (format (format "%%%ds | %%s" +irc-left-padding) left right))

  (setq circe-use-cycle-completion t
        circe-reduce-lurker-spam t

        circe-format-say (format "{nick:+%ss} │ {body}" +irc-left-padding)
        circe-format-self-say circe-format-say
        circe-format-action (format "{nick:+%ss} * {body}" +irc-left-padding)
        circe-format-self-action circe-format-action

        circe-format-server-topic
        (+irc|circe-format-padding "*** Topic" "{userhost}: {topic-diff}")

        circe-format-server-join-in-channel
        (+irc|circe-format-padding "*** Join" "{nick} ({userinfo}) joined {channel}")

        circe-format-server-join
        (+irc|circe-format-padding "*** Join" "{nick} ({userinfo})")

        circe-format-server-part
        (+irc|circe-format-padding "*** Part" "{nick} ({userhost}) left {channel}: {reason}")

        circe-format-server-quit
        (+irc|circe-format-padding "*** Quit" "{nick} ({userhost}) left IRC: {reason}]")

        circe-format-server-quit-channel
        (+irc|circe-format-padding "*** Quit" "{nick} ({userhost}) left {channel}: {reason}]")

        circe-format-server-rejoin
        (+irc|circe-format-padding "*** Re-join" "{nick} ({userhost}), left {departuredelta} ago")

        circe-format-server-nick-change
        (+irc|circe-format-padding "*** Nick" "{old-nick} ({userhost}) is now known as {new-nick}")

        circe-format-server-nick-change-self
        (+irc|circe-format-padding "*** Nick" "You are now known as {new-nick} ({old-nick})")

        circe-format-server-nick-change-self
        (+irc|circe-format-padding "*** Nick" "{old-nick} ({userhost}) is now known as {new-nick}")

        circe-format-server-mode-change
        (+irc|circe-format-padding "*** Mode" "{change} on {target} by {setter} ({userhost})")

        circe-format-server-lurker-activity
        (+irc|circe-format-padding "*** Lurk" "{nick} joined {joindelta} ago"))

  (defun +irc*circe-truncate-nicks (orig-func keywords)
    "If nick is too long, truncate it. Uses `+irc-left-padding'
to determine length."
    (when (plist-member keywords :nick)
      (let* ((long-nick (plist-get keywords :nick))
             (short-nick (s-left (- +irc-left-padding 1) long-nick)))
        ;; only change the nick if it's needed
        (unless (or (= +irc-left-padding
                       (length long-nick))
                    (equal long-nick short-nick))
          (plist-put keywords :nick (s-concat short-nick "▶")))))
    (funcall orig-func keywords))
  (advice-add 'circe--display-add-nick-property :around #'+irc*circe-truncate-nicks)

  (defun +irc*circe-disconnect-hook (&rest _)
    (run-hooks '+irc-disconnect-hook))
  (advice-add 'circe--irc-conn-disconnected :after #'+irc*circe-disconnect-hook)

  (add-hook! '+irc-disconnect-hook
    (run-at-time "5 minute" nil #'circe-reconnect-all))

  (defun +irc|circe-message-option-bot (nick &rest ignored)
    (when (member nick +irc-bot-list)
      '((text-properties . (face circe-fool-face
                                 lui-do-not-track t)))))
  (add-hook 'circe-message-option-functions #'+irc|circe-message-option-bot)

  (enable-circe-new-day-notifier)
  (add-hook! 'circe-server-connected-hook
    (run-at-time "5 minutes" nil #'enable-circe-notifications))

  (add-hook! 'circe-channel-mode-hook
    #'(enable-circe-color-nicks enable-lui-autopaste turn-on-visual-line-mode)))


(def-package! circe-color-nicks
  :commands enable-circe-color-nicks
  :config
  (setq circe-color-nicks-min-constrast-ratio 4.5
        circe-color-nicks-everywhere t))


(def-package! circe-new-day-notifier
  :commands enable-circe-new-day-notifier
  :config (setq circe-new-day-notifier-format-message
                (+irc|circe-format-padding
                "*** Day"
                "Date changed [{day}]")))


(def-package! circe-notifications
  :commands enable-circe-notifications
  :config (setq circe-notifications-watch-strings
                +irc-notifications-watch-strings))


(def-package! lui
  :commands lui-mode
  :config
  (map! :map lua-mode-map "C-u" #'lui-kill-to-beginning-of-line)

  (enable-lui-logging)
  (defun +irc|lui-setup-margin ()
    (setq lui-time-stamp-position 'right-margin
          lui-time-stamp-format +irc-time-stamp-format
          right-margin-width (length (format-time-string lui-time-stamp-format))))
  (defun +irc|lui-setup-wrap ()
    (setq fringes-outside-margins t
          word-wrap t
          wrap-prefix (s-repeat (+ +irc-left-padding 3) " ")))
  (add-hook! 'lui-mode-hook '(+irc|lui-setup-margin +irc|lui-setup-wrap))
  (when (featurep! :feature spellcheck)
    (setq lui-flyspell-p t
          lui-fill-type nil)))


(def-package! lui-autopaste
  :commands enable-lui-autopaste)


(def-package! lui-logging
  :commands enable-lui-logging)
