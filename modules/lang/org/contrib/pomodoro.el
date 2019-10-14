;;; lang/org/contrib/pomodoro.el -*- lexical-binding: t; -*-
;;;###if (featurep! +pomodoro)

(use-package! org-pomodoro
  :defer t
  :config

  ;; prefer PulseAudio to ALSA in $current_year
  (setq org-pomodoro-audio-player (or (executable-find "paplay")
                                      org-pomodoro-audio-player))

  ;; configure pomodoro alerts to use growl or libnotify
  (alert-add-rule :category "org-pomodoro"
                  :style (cond (alert-growl-command
                                'growl)
                               (alert-libnotify-command
                                'libnotify)
                               (alert-default-style))))
