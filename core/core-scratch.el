;;; core-scratch.el

(setq initial-major-mode 'doom-mode
      initial-scratch-message "\n  Loading..."
      inhibit-startup-screen t
      ;; shuts up emacs at startup
      inhibit-startup-echo-area-message user-login-name)

(defvar doom-buffer nil
  "The global and persistent scratch buffer for doom.")

(defvar doom-scratch-name " *doom*"
  "The name of the doom scratch buffer.")

(defvar doom-scratch-edited nil
  "If non-nil, the scratch buffer has been edited.")

(defvar doom-scratch-inhibit-refresh nil
  "If non-nil, the doom buffer won't be refreshed.")

(defvar doom-scratch-modeline (doom-modeline 'scratch)
  "Modeline format for doom scratch buffer.")

(defvar doom-scratch-widgets '(banner shortmenu loaded)
  "List of widgets to display in a blank scratch buffer.")

(define-derived-mode doom-mode fundamental-mode
  (concat "v" doom-version)
  "Major mode for the DOOM scratch buffer.")

(defvar doom-scratch--width 0)
(defvar doom-scratch--height 0)


;;
(add-hook 'emacs-startup-hook 'doom-scratch)
(add-hook! 'kill-buffer-query-functions (not (doom-scratch-buffer-p)))
(add-hook! window-setup
  (add-hook 'window-configuration-change-hook 'doom-scratch-reload)
  (doom-scratch-reload))


;;
(defun doom-scratch-buffer-p (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (eq buffer doom-buffer))))

(defun doom-scratch-buffer ()
  "Ensure the scratch buffer exists and is alive (otherwise create it)."
  ;; Rename the old scratch buffer, if it exists.
  (let ((old-scratch (get-buffer "*scratch*")))
    (when old-scratch (kill-buffer old-scratch)))
  ;; Ensure the doom buffer is alive!
  (unless (buffer-live-p doom-buffer)
    (setq doom-buffer nil))
  (unless doom-buffer
    (setq doom-buffer (get-buffer-create doom-scratch-name)))
  doom-buffer)

(defun doom-scratch ()
  (interactive)
  (doom-scratch-reload)
  (switch-to-buffer doom-buffer)
  nil)

(defun doom-scratch-force-reload ()
  (setq doom-scratch-edited nil)
  (doom-scratch-reload))

(defun doom|scratch-clear-on-insert ()
  "Erase the buffer and prepare it to be used like a normal buffer."
  (erase-buffer)
  ;; (set-window-margins (get-buffer-window doom-buffer) 0 0)
  (setq doom-scratch-edited t
        mode-line-format (doom-modeline))
  (remove-hook 'evil-insert-state-entry-hook 'doom|mode-erase-on-insert t))

(defun doom-scratch-reload (&optional dir)
  "Update the DOOM scratch buffer (or create it, if it doesn't exist)."
  (when (and (not doom-scratch-inhibit-refresh)
             (not (minibuffer-window-active-p (minibuffer-window)))
             (get-buffer-window-list doom-buffer nil t)
             (or (not doom-scratch-edited) dir))
    (let ((old-pwd (or dir default-directory)))
      (with-current-buffer (doom-scratch-buffer)
        (doom-mode)
        (add-hook 'evil-insert-state-entry-hook 'doom|scratch-clear-on-insert nil t)
        (add-hook 'after-change-major-mode-hook 'doom|scratch-clear-on-insert nil t)
        (setq doom-scratch-edited nil)

        (erase-buffer)
        (let ((doom-scratch--width (1- (window-width (get-buffer-window doom-buffer))))
              (doom-scratch--height (window-height (get-buffer-window doom-buffer))))
          (insert (make-string (max 0 (- (truncate (/ doom-scratch--height 2)) 12)) ?\n))
          (mapc (lambda (widget-name)
                  (funcall (intern (format "doom-scratch-widget-%s" widget-name)))
                  (insert "\n\n"))
                doom-scratch-widgets))

        (setq default-directory old-pwd)
        (setq mode-line-format (doom-modeline 'scratch)))))
  t)

(defun doom-scratch-widget-banner ()
  (mapc (lambda (line)
          (insert "\n")
          (insert (propertize (s-center doom-scratch--width line)
                              'face 'font-lock-comment-face) " "))
        '("=================     ===============     ===============   ========  ========"
          "\\\\ . . . . . . .\\\\   //. . . . . . .\\\\   //. . . . . . .\\\\  \\\\. . .\\\\// . . //"
          "||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\\/ . . .||"
          "|| . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||"
          "||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||"
          "|| . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\\ . . . . ||"
          "||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\\_ . .|. .||"
          "|| . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\\ `-_/| . ||"
          "||_-' ||  .|/    || ||    \\|.  || `-_|| ||_-' ||  .|/    || ||   | \\  / |-_.||"
          "||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \\  / |  `||"
          "||    `'         || ||         `'    || ||    `'         || ||   | \\  / |   ||"
          "||            .===' `===.         .==='.`===.         .===' /==. |  \\/  |   ||"
          "||         .=='   \\_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \\/  |   ||"
          "||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \\/  |   ||"
          "||   .=='    _-'          '-__\\._-'         '-_./__-'         `' |. /|  |   ||"
          "||.=='    _-'                                                     `' |  /==.||"
          "=='    _-'                         E M A C S                          \\/   `=="
          "\\   _-'                                                                `-_   /"
          " `''                                                                      ``'")))

(defun doom-scratch-widget-loaded ()
  (insert
   (s-center (1- doom-scratch--width)
             (propertize
              (format "Loaded %d packages in %s"
                      (length doom-packages)
                      (emacs-init-time))
              'face '(:inherit font-lock-comment-face
                               :height 0.9)))))

(defun doom-scratch-widget-shortmenu ()
  (let ((all-the-icons-scale-factor 1.3)
        (all-the-icons-default-adjust -0.05)
        (start (point))
        (sep "   ")
        (last-session-p (and (featurep 'workgroups2)
                             (f-exists-p wg-session-file)))
        end)
    (unless last-session-p
      (setq sep "     "))
    (insert
     (s-center (- doom-scratch--width 5)
               (with-temp-buffer
                 (insert-text-button
                  (concat (all-the-icons-octicon
                           "mark-github"
                           :face 'font-lock-keyword-face)
                          (propertize " Homepage" 'face 'font-lock-keyword-face))
                  'action '(lambda (_) (browse-url "https://github.com/hlissner/.emacs.d"))
                  'follow-link t)

                 (insert sep " ")

                 (insert-text-button
                  (concat (all-the-icons-octicon
                           "file-text"
                           :face 'font-lock-keyword-face)
                          (propertize " Recent files" 'face 'font-lock-keyword-face))
                  'action '(lambda (_) (call-interactively 'counsel-recentf))
                  'follow-link t)

                 (insert sep)

                 (insert-text-button
                  (concat (all-the-icons-octicon
                           "tools"
                           :face 'font-lock-keyword-face)
                          (propertize " Edit emacs.d" 'face 'font-lock-keyword-face))
                  'action '(lambda (_) (find-file (f-expand "init.el" doom-emacs-dir)))
                  'follow-link t)

                 (when last-session-p
                   (insert sep)

                   (insert-text-button
                    (concat (all-the-icons-octicon
                             "history"
                             :face 'font-lock-keyword-face)
                            (propertize " Reload last session" 'face 'font-lock-keyword-face))
                    'action '(lambda (_) (doom:workgroup-load))
                    'follow-link t))

                 (setq end (point))
                 (buffer-string))))))

(provide 'core-scratch)
;;; core-scratch.el ends here
