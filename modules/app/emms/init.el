;;; private/emms/init.el -*- lexical-binding: t; -*-

;; use the same port number as the one you use in mpd.conf
(use-package emms
  :init
  (setq mpc-host "localhost:8501")
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (setq emms-seek-seconds 5)
  (setq emms-player-list '(emms-player-mpd))
  (setq emms-info-functions '(emms-info-mpd))
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "8501"))
