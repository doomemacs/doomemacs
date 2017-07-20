;;; app/irc/config.el -*- lexical-binding: t; -*-

(defvar +irc-left-padding 13
  "TODO")

(defvar +irc-scroll-to-bottom-on-commands
  '(self-insert-command yank hilit-yank)
  "If these commands are called pre prompt the buffer will scroll to `point-max'.")

(defvar +irc-disconnect-hook nil
  "TODO")

(defvar +irc-bot-list '("fsbot" "rudybot")
  "TODO")

(defvar +irc-time-stamp-format "%H:%M"
  "TODO")

(defvar +irc-notifications-watch-strings nil
  "TODO")

(defvar +irc-defer-notifications nil
  "How long to defer enabling notifications, in seconds (e.g. 5min = 300).
Useful for ZNC users who want to avoid the deluge of notifications during buffer
playback.")

(def-setting! :irc (server letvars)
  "Registers an irc server for circe."
  `(after! circe
     (cl-pushnew (cons ,server ,letvars) circe-network-options
                 :test #'equal :key #'car)))

(defvar +irc--defer-timer nil)

(defsubst +irc--pad (left right)
  (format (format "%%%ds | %%s" +irc-left-padding)
          (concat "*** " left) right))


;;
;; Plugins
;;

(def-package! circe
  :commands (circe circe-server-buffers)
  :init (setq circe-network-defaults nil)
  :config
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

  (add-hook 'circe-channel-mode-hook #'turn-on-visual-line-mode)

  ;; Let `+irc/quit' and `circe' handle buffer cleanup
  (map! :map circe-mode-map [remap doom/kill-this-buffer] #'bury-buffer)

  (defun +irc*circe-truncate-nicks ()
    "Truncate long nicknames in chat output non-destructively."
    (when-let (beg (text-property-any (point-min) (point-max) 'lui-format-argument 'nick))
      (goto-char beg)
      (let ((end (next-single-property-change beg 'lui-format-argument))
            (nick (plist-get (plist-get (text-properties-at beg) 'lui-keywords)
                             :nick)))
        (when (> (length nick) +irc-left-padding)
          (compose-region (+ beg +irc-left-padding -1) end
                          ?…)))))
  (add-hook 'lui-pre-output-hook #'+irc*circe-truncate-nicks)

  (defun +irc|circe-message-option-bot (nick &rest ignored)
    "Fontify known bots and mark them to not be tracked."
    (when (member nick +irc-bot-list)
      '((text-properties . (face circe-fool-face lui-do-not-track t)))))
  (add-hook 'circe-message-option-functions #'+irc|circe-message-option-bot)

  (after! solaire-mode
    ;; distinguish chat/channel buffers from server buffers.
    (add-hook 'circe-chat-mode-hook #'solaire-mode)))


(def-package! circe-color-nicks
  :commands enable-circe-color-nicks
  :init (add-hook 'circe-channel-mode-hook 'enable-circe-color-nicks)
  :config
  (setq circe-color-nicks-min-constrast-ratio 4.5
        circe-color-nicks-everywhere t))


(def-package! circe-new-day-notifier
  :after circe
  :config
  (enable-circe-new-day-notifier)
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

  (after! evil
    (defun +irc|evil-insert ()
      "Ensure entering insert mode will put us at the prompt, unless editing
after prompt marker."
      (when (> (marker-position lui-input-marker) (point))
        (goto-char (point-max))))

    (add-hook! 'lui-mode-hook
      (add-hook 'evil-insert-state-entry-hook #'+irc|evil-insert nil t))

    (mapc (lambda (cmd) (cl-pushnew cmd +irc-scroll-to-bottom-on-commands))
          '(evil-paste-after evil-paste-before evil-open-above evil-open-below)))


  (defun +irc|preinput-scroll-to-bottom ()
    "Go to the end of the buffer in all windows showing it.
Courtesy of esh-mode.el"
    (when (memq this-command +irc-scroll-to-bottom-on-commands)
      (let* ((selected (selected-window))
             (current (current-buffer)))
        (when (> (marker-position lui-input-marker) (point))
          (walk-windows
           (function
            (lambda (window)
              (when (eq (window-buffer window) current)
                (select-window window)
                (goto-char (point-max))
                (select-window selected))))
           nil t)))))

  (add-hook! 'lui-mode-hook
    (add-hook 'pre-command-hook #'+irc|preinput-scroll-to-bottom nil t))

  (defun +irc|init-lui-margins ()
    (setq lui-time-stamp-position 'right-margin
          lui-time-stamp-format +irc-time-stamp-format
          right-margin-width (length (format-time-string lui-time-stamp-format))))

  (defun +irc|init-lui-wrapping ()
    (setq fringes-outside-margins t
          word-wrap t
          wrap-prefix (s-repeat (+ +irc-left-padding 3) " ")))

  (add-hook! 'lui-mode-hook #'(+irc|init-lui-margins +irc|init-lui-wrapping)))


(def-package! lui-logging
  :after lui
  :config (enable-lui-logging))


(def-package! lui-autopaste
  :commands enable-lui-autopaste
  :init (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste))
