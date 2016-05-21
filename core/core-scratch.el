;;; core-scratch.el

(setq initial-major-mode 'doom-mode
      initial-scratch-message "\n  Loading..."
      inhibit-startup-screen t) ; don't show emacs start screen

(defvar doom-buffer nil "")
(defvar doom-buffer-name "*doom*" "")
(define-derived-mode doom-mode text-mode "DOOM"
  "Major mode for special DOOM buffers.")

;; Don't kill the scratch buffer
(add-hook! 'kill-buffer-query-functions (not (string= doom-buffer-name (buffer-name))))

(defun doom-mode-init (&optional auto-detect-frame)
  (unless (buffer-live-p doom-buffer) (setq doom-buffer nil))
  (let ((old-scratch (get-buffer "*scratch*")))
    (when old-scratch
      (with-current-buffer old-scratch
        (rename-buffer doom-buffer-name)
        (setq doom-buffer old-scratch))))
  (unless doom-buffer
    (setq doom-buffer (get-buffer-create doom-buffer-name)))
  (with-current-buffer doom-buffer
    (doom-mode)
    (erase-buffer)
    (insert
     (let* ((auto-detect-frame (or auto-detect-frame (not (display-graphic-p))))
            (width (- (if auto-detect-frame (window-width) (cdr (assq 'width default-frame-alist))) 3))
            (lead (make-string (truncate (/ (- width 78) 2)) ? )))
       (concat
        (propertize
         (concat
          (make-string (min 3 (/ (if auto-detect-frame (window-height) (cdr (assq 'height default-frame-alist))) 5)) ?\n)
          lead "=================     ===============     ===============   ========  ========\n"
          lead "\\\\ . . . . . . .\\\\   //. . . . . . .\\\\   //. . . . . . .\\\\  \\\\. . .\\\\// . . //\n"
          lead "||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\\/ . . .||\n"
          lead "|| . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||\n"
          lead "||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||\n"
          lead "|| . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\\ . . . . ||\n"
          lead "||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\\_ . .|. .||\n"
          lead "|| . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\\ `-_/| . ||\n"
          lead "||_-' ||  .|/    || ||    \\|.  || `-_|| ||_-' ||  .|/    || ||   | \\  / |-_.||\n"
          lead "||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \\  / |  `||\n"
          lead "||    `'         || ||         `'    || ||    `'         || ||   | \\  / |   ||\n"
          lead "||            .===' `===.         .==='.`===.         .===' /==. |  \\/  |   ||\n"
          lead "||         .=='   \\_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \\/  |   ||\n"
          lead "||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \\/  |   ||\n"
          lead "||   .=='    _-'          '-__\\._-'         '-_./__-'         `' |. /|  |   ||\n"
          lead "||.=='    _-'                                                     `' |  /==.||\n"
          lead "=='    _-'                         E M A C S                          \\/   `==\n"
          lead "\\   _-'                                                                `-_   /\n"
          lead " `''                                                                     ``'")
         'face 'font-lock-comment-face)
        "\n\n"
        (propertize
         (s-trim-right
          (s-center (1- width) "Press `,m` to open recent files, or `,E` to access emacs.d"))
         'face 'font-lock-keyword-face)
        (if emacs-end-time
            (concat
             "\n\n"
             (s-trim-right (s-center (- width 2) (format "Loaded in %.3fs" emacs-end-time))))
          ""))))
    (back-to-indentation)
    (doom|update-scratch-buffer)))

(provide 'core-scratch)
;;; core-scratch.el ends here
