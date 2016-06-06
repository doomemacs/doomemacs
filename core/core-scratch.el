;;; core-scratch.el

(setq initial-major-mode 'doom-mode
      initial-scratch-message "\n  Loading..."
      inhibit-startup-screen t
      ;; shuts up emacs at startup
      inhibit-startup-echo-area-message user-login-name)

(defvar doom-buffer nil
  "The global and persistent scratch buffer for doom.")
(defvar doom-buffer-name "*doom*"
  "The name of the doom scratch buffer.")
(defvar-local doom-buffer-edited nil
  "If non-nil, the scratch buffer has been edited.")

(def-popup! "*doom*"                   :align below :size 35 :select t)
(def-popup! "^\\*doom.+\\*$" :regexp t :align below :size 12 :noselect t)

(define-derived-mode doom-mode fundamental-mode "DOOM"
  "Major mode for special DOOM buffers.")

;; Don't kill the scratch buffer
(add-hook! 'kill-buffer-query-functions
  (not (string= doom-buffer-name (buffer-name))))

(after! uniquify
  (push (regexp-quote doom-buffer-name) uniquify-ignore-buffers-re))

(add-hook! emacs-startup 'doom-mode-init)

(defun doom-mode-erase-on-insert ()
  (erase-buffer)
  (setq doom-buffer-edited t)
  (remove-hook 'evil-insert-state-entry-hook 'doom-mode-erase-on-insert t))

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
    (add-hook 'evil-insert-state-entry-hook 'doom-mode-erase-on-insert nil t)
    (add-hook 'after-change-major-mode-hook 'doom-mode-erase-on-insert nil t)
    (erase-buffer)
    (setq doom-buffer-edited nil)
    (insert
     (let* ((auto-detect-frame (or auto-detect-frame (not window-system)))
            (width (max 3 (- (if auto-detect-frame
                                 (window-width)
                               (cdr (assq 'width default-frame-alist))) 3)))
            (height (if auto-detect-frame
                        (window-height)
                      (cdr (assq 'height default-frame-alist))))
            (lead (make-string (truncate (max 0 (/ (- width 78) 2))) ? )))
       (concat
        (propertize
         (concat
          (make-string (max 5 (- (truncate (/ height 2)) 12)) ?\n)
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
          (s-center (max 0 (1- width)) "Press `,m` to open recent files, or `,E` to access emacs.d"))
         'face 'font-lock-keyword-face)
        (concat
         "\n\n"
         (s-trim-right (s-center (max 0 (- width 2))
                                 (format "Loaded in %.3fs"
                                         (float-time (time-subtract after-init-time emacs-start-time)))))))))
    (back-to-indentation)
    (doom|update-scratch-buffer nil t)))

(provide 'core-scratch)
;;; core-scratch.el ends here
