;;; app/irc/config.el -*- lexical-binding: t; -*-

(defvar +irc-left-padding 13
  "By how much spaces the left hand side of the line should be padded.
Below a value of 12 this may result in uneven alignment between the various
types of messages.")

(defvar +irc-truncate-nick-char ?…
  "Character to displayed when nick > `+irc-left-padding' in length.")

(defvar +irc-scroll-to-bottom-on-commands
  '(self-insert-command yank hilit-yank)
  "If these commands are called pre prompt the buffer will scroll to `point-max'.")

(defvar +irc-disconnect-hook nil
  "Runs each hook when circe noticies the connection has been disconnected.
Useful for scenarios where an instant reconnect will not be successful.")

(defvar +irc-bot-list '("fsbot" "rudybot")
  "Nicks listed have `circe-fool-face' applied and will not be tracked.")

(defvar +irc-defer-notifications nil
  "How long to defer enabling notifications, in seconds (e.g. 5min = 300).

Useful for ZNC users who want to avoid the deluge of notifications during buffer
playback.")

(defvar +irc--defer-timer nil)

(defsubst +irc--pad (left right)
  (format (format "%%%ds | %%s" +irc-left-padding)
          (concat "*** " left) right))

(define-obsolete-variable-alias '+irc-notifications-watch-strings 'circe-notifications-watch-strings "3.0.0")
(define-obsolete-variable-alias '+irc-time-stamp-format 'lui-time-stamp-format "3.0.0")


;;
;;; Packages

(use-package! circe
  :commands circe-server-buffers
  :config
  (setq circe-default-quit-message nil
        circe-default-part-message nil
        circe-use-cycle-completion t
        circe-reduce-lurker-spam t

        circe-format-say (format "{nick:+%ss} │ {body}" +irc-left-padding)
        circe-format-self-say circe-format-say
        circe-format-action (format "{nick:+%ss} * {body}" +irc-left-padding)
        circe-format-self-action circe-format-action
        circe-format-server-notice
        (let ((left "-Server-")) (concat (make-string (- +irc-left-padding (length left)) ? )
                                         (concat left " _ {body}")))
        circe-format-notice (format "{nick:%ss} _ {body}" +irc-left-padding)
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
        circe-format-server-netmerge
        (+irc--pad "Netmerge" "{split}, split {ago} ago (Use /WL to see who's still missing)")
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

  (add-hook 'doom-real-buffer-functions #'+circe-buffer-p)
  (add-hook 'circe-channel-mode-hook #'turn-on-visual-line-mode)
  (add-hook 'circe-mode-hook #'+irc--add-circe-buffer-to-persp-h)
  (add-hook 'circe-mode-hook #'turn-off-smartparens-mode)

  (defadvice! +irc--circe-run-disconnect-hook-a (&rest _)
    "Runs `+irc-disconnect-hook' after circe disconnects."
    :after #'circe--irc-conn-disconnected
    (run-hooks '+irc-disconnect-hook))

  (add-hook! 'circe-message-option-functions
    (defun +irc-circe-message-option-bot-h (nick &rest ignored)
      "Fontify known bots and mark them to not be tracked."
      (when (member nick +irc-bot-list)
        '((text-properties . (face circe-fool-face lui-do-not-track t))))))

  ;; Let `+irc/quit' and `circe' handle buffer cleanup
  (define-key circe-mode-map [remap kill-buffer] #'bury-buffer)
  ;; Fail gracefully if not in a circe buffer
  (global-set-key [remap tracking-next-buffer] #'+irc/tracking-next-buffer)

  (when (modulep! :completion vertico)
    (after! consult
      (add-to-list 'consult-buffer-sources '+irc--consult-circe-source 'append)))

  (map! :localleader
        (:map circe-mode-map
          "a" #'tracking-next-buffer
          "j" #'circe-command-JOIN
          "m" #'+irc/send-message
          "p" #'circe-command-PART
          "Q" #'+irc/quit
          "R" #'circe-reconnect
          "c" #'+irc/jump-to-channel)
        (:map circe-channel-mode-map
          "n" #'circe-command-NAMES)))


(use-package! circe-color-nicks
  :defer t
  ;; NOTE: I avoid `:after' on purpose, because it convolutes load order and
  ;;   makes it harder for users to know what to target with `after!' or
  ;;   `with-eval-after-load' when trying to configure packages.
  :init (after! circe (require 'circe-color-nicks))
  :config
  (setq circe-color-nicks-min-constrast-ratio 4.5
        circe-color-nicks-everywhere t)
  (enable-circe-color-nicks))


(use-package! circe-new-day-notifier
  :defer t
  ;; NOTE: See NOTE in `circe-color-nicks' above.
  :init (after! circe (require 'circe-new-day-notifier))
  :config
  (enable-circe-new-day-notifier)
  (setq circe-new-day-notifier-format-message
        (+irc--pad "Day" "Date changed [{day}]")))


(use-package! circe-notifications
  :defer t
  :init
  (add-hook! 'circe-server-connected-hook
    (defun +irc-init-circe-notifications-h ()
      (if (numberp +irc-defer-notifications)
          (setq +irc--defer-timer
                (run-at-time +irc-defer-notifications nil
                             #'enable-circe-notifications))
        (enable-circe-notifications))))
  :config
  (setq circe-notifications-emacs-focused nil
        circe-notifications-alert-style
        (cond ((featurep :system 'macos) 'osx-notifier)
              ((featurep :system 'linux) 'libnotify)
              (circe-notifications-alert-style))))


(use-package! lui
  :commands lui-mode
  :config
  (define-key lui-mode-map "\C-u" #'lui-kill-to-beginning-of-line)
  (setq lui-fill-type nil)
  (setq lui-flyspell-p (modulep! :checkers spell +flyspell))

  (setq lui-time-stamp-format "%H:%M"
        lui-time-stamp-position 'right-margin)

  (enable-lui-autopaste)  ; prompt to use paste service for large pastes
  (enable-lui-track)      ; horizontal line marking last read message
  (enable-lui-irc-colors) ; enable IRC colors (see https://www.mirc.co.uk/colors.html)

  (add-hook! 'lui-pre-output-hook
    (defun +irc-truncate-nicks-h ()
      "Truncate long nicknames in chat output non-destructively."
      (when-let (beg (text-property-any (point-min) (point-max) 'lui-format-argument 'nick))
        (goto-char beg)
        (let ((end (next-single-property-change beg 'lui-format-argument))
              (nick (plist-get (plist-get (text-properties-at beg) 'lui-keywords)
                               :nick)))
          (when (> (length nick) +irc-left-padding)
            (compose-region (+ beg +irc-left-padding -1) end
                            +irc-truncate-nick-char))))))

  (after! evil
    (defun +irc-evil-insert-h ()
      "Ensure entering insert mode will put us at the prompt, unless editing
after prompt marker."
      (when (> (marker-position lui-input-marker) (point))
        (goto-char (point-max))))

    (add-hook! 'lui-mode-hook
      (add-hook 'evil-insert-state-entry-hook #'+irc-evil-insert-h
                nil 'local))

    (mapc (lambda (cmd) (push cmd +irc-scroll-to-bottom-on-commands))
          '(evil-paste-after evil-paste-before evil-open-above evil-open-below)))

  (defun +irc-preinput-scroll-to-bottom-h ()
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
    (add-hook 'pre-command-hook #'+irc-preinput-scroll-to-bottom-h nil t))

  (add-hook! 'lui-mode-hook
    (defun +irc-init-lui-margins-h ()
      (pcase lui-time-stamp-position
        (`right-margin (setq right-margin-width (length (format-time-string lui-time-stamp-format))))
        (`left-margin  (setq left-margin-width  (length (format-time-string lui-time-stamp-format))))))
    (defun +irc-init-lui-wrapping-a ()
      (setq fringes-outside-margins t
            word-wrap t
            wrap-prefix (make-string (+ +irc-left-padding 3) ? )))))


(use-package! lui-logging
  :after lui
  :init (setq lui-logging-directory (file-name-concat doom-profile-state-dir "lui"))
  :config (enable-lui-logging))
