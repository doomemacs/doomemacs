;;; private/emms/config.el -*- lexical-binding: t; -*-

(use-package emms
  :config
  (emms-all))
  ;; Keys that make sense regardless of whether or not your in emms.
  (map! :prefix ("SPC e" . "emms")
        :n "p" #'emms-playlist-new
        :n "b" #'emms-smart-browse
        :n "r" #'emms-player-mpd-update-all-reset-cache
        :n "c" #'mpd/start-music-daemon
        :n "k" #'mpd/kill-music-daemon
        :n "u" #'mpc/update-database)
  (map! :prefix ((concat doom-leader-alt-key " e ") . "emms")
        :ie "p" #'emms-playlist-new
        :ie "b" #'emms-smart-browse
        :ie "r" #'emms-player-mpd-update-all-reset-cache
        :ie "c" #'mpd/start-music-daemon
        :ie "k" #'mpd/kill-music-daemon
        :ie "u" #'mpc/update-database)

  ;; Keybinds for emms that only make sense to use in an emms buffer.
  (map! :map emms-playlist-mode-map
        :localleader
        :n "l" #'emms-toggle-repeat-playlist
        :n "p" #'emms-insert-playlist
        :n "i" #'emms-insert-file
        :n "t" #'emms-toggle-repeat-track
        :n "s" #'emms-playlist-save
        :n "m" #'emms-shuffle)

  (defun mpd/start-music-daemon ()
    (interactive)
    (shell-command "mpd")
    (mpc/update-database)
    (emms-player-mpd-connect)
    (emms-cache-set-from-mpd-all)
    (message "MPD Started!"))

  (defun mpd/kill-music-daemon ()
    (interactive)
    (emms-stop)
    (call-process "killall" nil nil nil "mpd")
    (message "MPD Killed!"))

  (defun mpc/update-database ()
    (interactive)
    (call-process "mpc" nil nil nil "update")
    (message "MPD Database Updated!"))
