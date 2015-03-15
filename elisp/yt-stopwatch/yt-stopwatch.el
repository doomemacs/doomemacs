
(defconst yt-stopwatch-script-path (file-name-directory load-file-name))

(defun yt-stopwatch-record-time ()
  (interactive)
  (beginning-of-line)
  (insert (format "* %ss - " (yt-stopwatch--reformat-time (cdr (yt-stopwatch--probe))))))

(defun yt-stopwatch--reformat-time (seconds)
  (format-time-string "%H:%M:%S" (encode-time (string-to-number (car seconds)) 0 0 0 0 0)))

(defun yt-stopwatch-video-name ()
  (interactive)
  (beginning-of-line)
  (insert (format "%s\n" (car (yt-stopwatch--probe)))))

(defun yt-stopwatch--probe ()
  (let ((out (shell-command-to-string (format "osascript %s/probe.scpt" yt-stopwatch-script-path))))
    (if (string-equal out "---\n")
        (user-error "VLC isn't running or no video playing")
      (split-string out " || " t "[\n \t]+"))))

(defun yt-stopwatch-test ()
  (interactive)
  (let ((data (yt-stopwatch--probe)))
    (message "Video: %s \n Time: %ss" (car data) (cdr data))))

(defvar yt-stopwatch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c y") 'yt-stopwatch-record-time)
    (define-key map (kbd "C-c C-y") 'yt-stopwatch-video-name)
    map))

(define-minor-mode yt-stopwatch-mode
  :keymap yt-stopwatch-mode-map
  :group yt-stopwatch-mode)

(provide 'yt-stopwatch)
;;; yt-stopwatch.el ends here
