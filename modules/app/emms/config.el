;;; app/emms/config.el -*- lexical-binding: t; -*-

(use-package emms
  :defer t
  :init
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (setq mpc-host "localhost:8501"
        emms-directory (concat user-emacs-directory "/.local/emms")
        emms-history-file (concat emms-directory "/history")
        emms-cache-file (concat emms-directory "/cache")
        emms-score-file (concat emms-directory "/score")
        emms-seek-seconds 5
        emms-player-list '(emms-player-mpd)
        emms-info-functions '(emms-info-mpd)
        emms-player-mpd-server-name "localhost"
        emms-player-mpd-server-port "8501")
  :config
  (emms-all)
  (map! :map emms-playlist-mode-map
        :localleader
        :n "l" #'emms-toggle-repeat-playlist
        :n "p" #'emms-insert-playlist
        :n "i" #'emms-insert-file
        :n "t" #'emms-toggle-repeat-track
        :n "s" #'emms-playlist-save
        :n "m" #'emms-shuffle))
