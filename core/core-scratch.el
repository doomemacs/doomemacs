;;; core-scratch.el

(setq initial-major-mode 'doom-mode
      initial-scratch-message "\n  Loading..."
      inhibit-startup-screen t
      ;; shuts up emacs at startup
      inhibit-startup-echo-area-message user-login-name)

(defvar doom-buffer nil
  "The global and persistent scratch buffer for doom.")
(defvar doom-buffer-name " *doom*"
  "The name of the doom scratch buffer.")
(defvar doom-buffer-edited nil
  "If non-nil, the scratch buffer has been edited.")

(define-derived-mode doom-mode fundamental-mode
  (concat "DOOM v" doom-version)
  "Major mode for special DOOM buffers.")

;; Don't kill the scratch buffer
(add-hook! 'kill-buffer-query-functions
  (not (eq doom-buffer (current-buffer))))

(add-hook 'emacs-startup-hook 'doom--reload-scratch-buffer)

;; Don't rename these buffers. That could cause problems.
(after! uniquify
  (setq uniquify-ignore-buffers-re (regexp-quote doom-buffer-name)))

(defun doom*scratch-split-hack (&rest _)
  "Removes the window margins before attempting a vertical-split on the scratch
buffer. Without this, it would refuse to split, saying 'too small to split'."
  (when (eq (current-buffer) doom-buffer)
    (set-window-margins nil 0 0)))
(advice-add 'split-window :before 'doom*scratch-split-hack)

(defun doom|mode-erase-on-insert ()
  "Erase the buffer and prepare it to be used like a normal buffer."
  (erase-buffer)
  (setq doom-buffer-edited t)
  (set-window-margins (get-buffer-window doom-buffer) 0 0)
  (remove-hook 'evil-insert-state-entry-hook 'doom|mode-erase-on-insert t))

(defun doom-reload-scratch-buffer (&optional dir)
  "Update the DOOM scratch buffer (or create it, if it doesn't exist)."
  (when (and (get-buffer-window-list doom-buffer nil t)
             (or (not doom-buffer-edited) dir)
             (not (minibuffer-window-active-p (minibuffer-window))))
    (doom--reload-scratch-buffer dir)))

(defun doom--reload-scratch-buffer (&optional dir)
  ;; Rename the old scratch buffer, if it exists.
  (let ((old-scratch (get-buffer "*scratch*")))
    (when old-scratch
      (with-current-buffer old-scratch
        (rename-buffer doom-buffer-name)
        (setq doom-buffer old-scratch))))
  ;; Ensure the doom buffer is alive!
  (unless (buffer-live-p doom-buffer)
    (setq doom-buffer nil))
  (unless doom-buffer
    (setq doom-buffer (get-buffer-create doom-buffer-name)))
  ;; Fill it with the splash screen content
  (with-current-buffer doom-buffer
    (doom-mode)
    (add-hook 'evil-insert-state-entry-hook 'doom|mode-erase-on-insert nil t)
    (add-hook 'after-change-major-mode-hook 'doom|mode-erase-on-insert nil t)
    (setq doom-buffer-edited nil)
    (let ((width 78) height)
      (mapc (lambda (window)
              (set-window-margins window 0 0)
              (let ((pad (max 0 (- (truncate (/ (window-width window) 2)) (truncate (/ width 2))))))
                (set-window-margins window pad pad)
                (setq height (max 0
                                  (min (or height 9999)
                                       (- (truncate (/ (window-height window) 2)) 13))))))
            (get-buffer-window-list doom-buffer nil t))
      (erase-buffer)
      (insert (propertize
               (concat
                (make-string (if height (max 0 height) 0) ?\n)
                "=================     ===============     ===============   ========  ========\n"
                "\\\\ . . . . . . .\\\\   //. . . . . . .\\\\   //. . . . . . .\\\\  \\\\. . .\\\\// . . //\n"
                "||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\\/ . . .||\n"
                "|| . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||\n"
                "||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||\n"
                "|| . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\\ . . . . ||\n"
                "||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\\_ . .|. .||\n"
                "|| . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\\ `-_/| . ||\n"
                "||_-' ||  .|/    || ||    \\|.  || `-_|| ||_-' ||  .|/    || ||   | \\  / |-_.||\n"
                "||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \\  / |  `||\n"
                "||    `'         || ||         `'    || ||    `'         || ||   | \\  / |   ||\n"
                "||            .===' `===.         .==='.`===.         .===' /==. |  \\/  |   ||\n"
                "||         .=='   \\_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \\/  |   ||\n"
                "||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \\/  |   ||\n"
                "||   .=='    _-'          '-__\\._-'         '-_./__-'         `' |. /|  |   ||\n"
                "||.=='    _-'                                                     `' |  /==.||\n"
                "=='    _-'                         E M A C S                          \\/   `==\n"
                "\\   _-'                                                              `-_   /\n"
                " `''                                                                     ``'")
               'face 'font-lock-comment-face)
              "\n\n"
              (propertize
               (s-center 78 "Press `,m` to open recent files, or `,E` to access emacs.d")
               'face 'font-lock-keyword-face)
              "\n\n"
              (s-center 78 (format "Loaded in %s" (emacs-init-time))))
      (back-to-indentation))
    ;;
    (when dir (setq default-directory dir))
    ;;
    (setq mode-line-format (doom-modeline 'scratch))
    ;; Readjust the scratch buffer if it is visible, when the window config changes.
    (add-hook 'window-configuration-change-hook 'doom-reload-scratch-buffer)))

(provide 'core-scratch)
;;; core-scratch.el ends here
