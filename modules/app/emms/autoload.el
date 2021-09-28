;;; app/emms/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +emms/mpd-start-music-daemon ()
  (interactive)
  (start-process "mpd" nil "mpd")
  (+emms/mpc-update-database)
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (message "MPD Started!"))

;;;###autoload
(defun +emms/mpd-kill-music-daemon ()
  (interactive)
  (emms-stop)
  (call-process "mpd" nil nil nil "--kill")
  (message "MPD Killed!"))

;;;###autoload
(defun +emms/mpc-update-database ()
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (message "MPD Database Updated!"))

;;;###autoload
(defun +emms/mpd-restart-music-daemon ()
  (interactive)
  (+emms/mpd-kill-music-daemon)
  (+emms/mpd-start-music-daemon)
  (message "MPD Restarted!"))
