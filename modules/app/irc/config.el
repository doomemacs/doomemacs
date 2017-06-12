;;; app/irc/config.el -*- lexical-binding: t; -*-

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

(defvar +irc-connections nil
  "A list of connections set with :irc. W")

(defvar +irc-defer-notifications nil
  "How long to defer enabling notifications, in seconds (e.g. 5min = 300).
Useful for ZNC users who want to avoid the deluge of notifications during buffer
playback.")

(def-setting! :irc (server letvars)
  "Registers an irc server for circe."
  `(cl-pushnew (cons ,server ,letvars) +irc-connections
               :test #'equal :key #'car))

(defvar +irc--defer-timer nil)


;;
;; Plugins
;;

(def-package! circe
  :commands (circe circe-server-buffers)
  :init (setq circe-network-defaults nil)
  :config
  ;; change hands
  (setq circe-network-options +irc-connections)
  (defvaralias '+irc-connections 'circe-network-options)

  (defsubst +irc--pad (left right)
    (format (format "%%%ds | %%s" +irc-left-padding)
            (concat "*** " left) right))

  (setq circe-default-quit-message nil
        circe-default-part-message nil
        circe-use-cycle-completion t
        circe-reduce-lurker-spam t

        circe-format-say (format "{nick:+%ss} │ {body}" +irc-left-padding)
        circe-format-self-say circe-format-say
        circe-format-action (format "{nick:+%ss} * {body}" +irc-left-padding)
        circe-format-self-action circe-format-action
        circe-format-server-topic
        (+irc--pad "Topic" "{userhost}: {topic-diff}")
        circe-format-server-join-in-channel
        (+irc--pad "Join" "{nick} ({userinfo}) joined {channel}")
        circe-format-server-join
        (+irc--pad "Join" "{nick} ({userinfo})")
        circe-format-server-part
        (+irc--pad "Part" "{nick} ({userhost}) left {channel}: {reason}")
        circe-format-server-quit
        (+irc--pad "Quit" "{nick} ({userhost}) left IRC: {reason}]")
        circe-format-server-quit-channel
        (+irc--pad "Quit" "{nick} ({userhost}) left {channel}: {reason}]")
        circe-format-server-rejoin
        (+irc--pad "Re-join" "{nick} ({userhost}), left {departuredelta} ago")
        circe-format-server-nick-change
        (+irc--pad "Nick" "{old-nick} ({userhost}) is now known as {new-nick}")
        circe-format-server-nick-change-self
        (+irc--pad "Nick" "You are now known as {new-nick} ({old-nick})")
        circe-format-server-nick-change-self
        (+irc--pad "Nick" "{old-nick} ({userhost}) is now known as {new-nick}")
        circe-format-server-mode-change
        (+irc--pad "Mode" "{change} on {target} by {setter} ({userhost})")
        circe-format-server-lurker-activity
        (+irc--pad "Lurk" "{nick} joined {joindelta} ago"))

  (enable-circe-new-day-notifier)

  (defun +irc*circe-disconnect-hook (&rest _)
    (run-hooks '+irc-disconnect-hook))
  (advice-add 'circe--irc-conn-disconnected :after #'+irc*circe-disconnect-hook)

  (defun +irc*circe-truncate-nicks ()
    "Truncate long nicknames in chat output (non-destructive)."
    (when-let (beg (text-property-any (point-min) (point-max) 'lui-format-argument 'nick))
      (goto-char beg)
      (let ((end (next-single-property-change beg 'lui-format-argument))
            (nick (plist-get (plist-get (text-properties-at beg) 'lui-keywords)
                             :nick)))
        (when (> (length nick) +irc-left-padding)
          (compose-region (+ beg +irc-left-padding -1) end
                          ?…)))))
  (add-hook 'lui-pre-output-hook #'+irc*circe-truncate-nicks)

  (add-hook! '+irc-disconnect-hook
    (run-at-time "5 minute" nil #'circe-reconnect-all))

  (defun +irc|circe-message-option-bot (nick &rest ignored)
    (when (member nick +irc-bot-list)
      '((text-properties . (face circe-fool-face lui-do-not-track t)))))
  (add-hook 'circe-message-option-functions #'+irc|circe-message-option-bot)

  (add-hook! 'circe-channel-mode-hook
    #'(enable-circe-color-nicks enable-lui-autopaste turn-on-visual-line-mode))

  (after! evil
    ;; Let `+irc/quit' and `circe' handle buffer cleanup
    (map! :map circe-mode-map [remap doom/kill-this-buffer] #'bury-buffer)

    ;; Ensure entering insert mode will put us at the prompt.
    (add-hook! 'lui-mode-hook
      (add-hook 'evil-insert-state-entry-hook #'end-of-buffer nil t)))

  (after! solaire-mode
    ;; distinguish chat/channel buffers from server buffers.
    (add-hook 'circe-chat-mode-hook #'solaire-mode)))



(def-package! circe-color-nicks
  :commands enable-circe-color-nicks
  :config
  (setq circe-color-nicks-min-constrast-ratio 4.5
        circe-color-nicks-everywhere t))


(def-package! circe-new-day-notifier
  :commands enable-circe-new-day-notifier
  :config
  (setq circe-new-day-notifier-format-message
        (+irc--pad "Day" "Date changed [{day}]")))


(def-package! circe-notifications
  :commands enable-circe-notifications
  :init
  (if +irc-defer-notifications
      (add-hook! 'circe-server-connected-hook
        (setq +irc--defer-timer
              (run-at-time +irc-defer-notifications nil
                           #'enable-circe-notifications)))
    (add-hook 'circe-server-connected-hook #'enable-circe-notifications))
  :config
  (setq circe-notifications-watch-strings +irc-notifications-watch-strings
        circe-notifications-emacs-focused nil
        circe-notifications-alert-style
        (cond (IS-MAC 'osx-notifier)
              (IS-LINUX 'libnotify))))


(def-package! lui
  :commands lui-mode
  :config
  (map! :map lui-mode-map "C-u" #'lui-kill-to-beginning-of-line)
  (when (featurep! :feature spellcheck)
    (setq lui-flyspell-p t
          lui-fill-type nil))

  (enable-lui-logging)
  (defun +irc|lui-setup-margin ()
    (setq lui-time-stamp-position 'right-margin
          lui-time-stamp-format +irc-time-stamp-format
          right-margin-width (length (format-time-string lui-time-stamp-format))))
  (defun +irc|lui-setup-wrap ()
    (setq fringes-outside-margins t
          word-wrap t
          wrap-prefix (s-repeat (+ +irc-left-padding 3) " ")))
  (add-hook! 'lui-mode-hook #'(+irc|lui-setup-margin +irc|lui-setup-wrap)))


(def-package! lui-autopaste
  :commands enable-lui-autopaste)


(def-package! lui-logging
  :commands enable-lui-logging)
