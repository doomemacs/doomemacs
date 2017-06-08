;;; ui/doom-dashboard/config.el -*- lexical-binding: t; -*-

(defvar +doom-dashboard-name " *doom*"
  "TODO")

(defvar +doom-dashboard-modeline nil
  "TODO")

(defvar +doom-dashboard-inhibit-refresh nil
  "If non-nil, the doom buffer won't be refreshed.")

(defvar +doom-dashboard-widgets '(banner shortmenu loaded)
  "List of widgets to display in a blank scratch buffer.")

(define-derived-mode +doom-dashboard-mode fundamental-mode
  (concat "v" doom-version)
  "Major mode for the DOOM dashboard buffer.")

(defvar +doom-dashboard--width 0)
(defvar +doom-dashboard--height 0)
(defvar +doom-dashboard--old-fringe-indicator fringe-indicator-alist)
(defvar +doom-dashboard--old-modeline nil)


;;
(after! evil
  (map! :map +doom-dashboard-mode-map
        "n" #'+doom-dashboard/next-button
        "p" #'+doom-dashboard/previous-button
        :em "j" #'+doom-dashboard/next-button
        :em "k" #'+doom-dashboard/previous-button
        [remap evil-insert]      #'evil-normal-state
        [remap evil-change]      #'evil-normal-state
        [remap evil-delete]      #'evil-normal-state
        [remap evil-delete-char] #'evil-normal-state)

  (defun +doom-dashboard/next-button ()
    (interactive)
    (ignore-errors
      (goto-char (next-button (point)))))

  (defun +doom-dashboard/previous-button ()
    (interactive)
    (ignore-errors
      (goto-char (previous-button (point))))))


(if (display-graphic-p)
    (require 'all-the-icons)
  (defalias 'all-the-icons-octicon    #'ignore)
  (defalias 'all-the-icons-faicon     #'ignore)
  (defalias 'all-the-icons-fileicon   #'ignore)
  (defalias 'all-the-icons-wicon      #'ignore)
  (defalias 'all-the-icons-alltheicon #'ignore))


;;
(setq doom-fallback-buffer +doom-dashboard-name)

(defun +doom-dashboard|kill-buffer-query-fn ()
  (or (not (+doom-dashboard-p))
      (ignore (ignore-errors (+doom-dashboard-reload))
              (bury-buffer))))

(defun +doom-dashboard|init (&rest _)
  (add-hook 'after-make-frame-functions #'+doom-dashboard-deferred-reload)
  (add-hook 'window-configuration-change-hook #'+doom-dashboard-reload)
  (add-hook 'kill-buffer-query-functions #'+doom-dashboard|kill-buffer-query-fn)
  (+doom-dashboard-reload)
  (when (equal (buffer-name) "*scratch*")
    (switch-to-buffer (doom-fallback-buffer))))

(add-hook 'window-setup-hook #'+doom-dashboard|init)

;; Compatibility with `midnight-mode' and `clean-buffer-list'
(after! midnight-mode
  (push +doom-dashboard-name clean-buffer-list-kill-never-buffer-names)
  (push "^\\s-*\\*doom.+" clean-buffer-list-kill-never-regexps))


;;
(defun +doom-dashboard/open ()
  "Open the dashboard buffer."
  (interactive)
  (+doom-dashboard-reload)
  (switch-to-buffer (doom-fallback-buffer)))

(defun +doom-dashboard-p (&optional buffer)
  "Returns t if BUFFER is the dashboard buffer."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (eq buffer (doom-fallback-buffer)))))

(defun +doom-dashboard-deferred-reload (&rest _)
  "Reload the dashboard after a brief pause. This is necessary for new frames,
whose dimensions may not be fully initialized by the time this is run."
  (run-with-timer 0.1 nil #'+doom-dashboard-reload))

(defun +doom-dashboard-reload (&optional dir)
  "Update the DOOM scratch buffer (or create it, if it doesn't exist)."
  (when (and (not +doom-dashboard-inhibit-refresh)
             (not (minibuffer-window-active-p (minibuffer-window)))
             (get-buffer-window-list (doom-fallback-buffer) nil t))
    (unless +doom-dashboard-modeline
      (setq +doom-dashboard--old-modeline mode-line-format)
      (setq +doom-dashboard-modeline
            (or (and (featurep! :ui doom-modeline)
                     (doom-modeline 'project))
                mode-line-format)))
    (let ((old-pwd (or dir default-directory))
          (inhibit-read-only t))
      (with-current-buffer (doom-fallback-buffer)
        (read-only-mode +1)
        (+doom-dashboard-mode)
        (setq fringe-indicator-alist (cl-loop for (car . _cdr) in fringe-indicator-alist
                                              collect (cons car nil)))
        (erase-buffer)
        (let* ((window (get-buffer-window (doom-fallback-buffer)))
               (+doom-dashboard--width  (window-width  window))
               (+doom-dashboard--height (window-height window)))
          (insert (make-string (max 0 (- (truncate (/ +doom-dashboard--height 2)) 16)) ?\n))
          (dolist (widget-name +doom-dashboard-widgets)
            (funcall (intern (format "doom-dashboard-widget--%s" widget-name)))
            (insert "\n")))
        (setq default-directory old-pwd
              mode-line-format +doom-dashboard-modeline)
        (unless (button-at (point))
          (goto-char (point-min))
          (goto-char (next-button (point)))))))
  t)

(defun doom-dashboard-widget--banner ()
  (mapc (lambda (line)
          (insert (propertize (s-center +doom-dashboard--width line)
                              'face 'font-lock-comment-face) " ")
          (insert "\n"))
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

(defun doom-dashboard-widget--loaded ()
  (insert
   (propertize
    (s-center +doom-dashboard--width
              (format "Loaded %d packages in %.03fs "
                      (- (length load-path) (length doom--base-load-path))
                      doom-init-time))
    'face 'font-lock-comment-face)
   "\n"))

(defvar all-the-icons-scale-factor)
(defvar all-the-icons-default-adjust)
(defun doom-dashboard-widget--shortmenu ()
  (let ((all-the-icons-scale-factor 1.3)
        (all-the-icons-default-adjust -0.05)
        (last-session-p (and (and (featurep 'persp-mode) persp-mode)
                             (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))))
    (mapc (lambda (btn)
            (when btn
              (let ((label (car btn))
                    (icon (nth 1 btn))
                    (fn (nth 2 btn)))
                (insert
                 (with-temp-buffer
                   (insert-text-button
                    (concat (all-the-icons-octicon
                             icon
                             :face 'font-lock-keyword-face)
                            (propertize (concat " " label) 'face 'font-lock-keyword-face))
                    'action fn
                    'follow-link t)
                   (s-center (1- +doom-dashboard--width) (buffer-string))))
                (insert "\n\n"))))
          `(("Homepage" "mark-github"
             (lambda (_) (browse-url "https://github.com/hlissner/.emacs.d")))
            ,(when last-session-p
               '("Reload last session" "history"
                 (lambda (_) (+workspace/load-session))))
            ("Recently opened files" "file-text"
             (lambda (_) (call-interactively (command-remapping 'recentf))))
            ("Recent opened projects" "briefcase"
             (lambda (_) (call-interactively (command-remapping 'projectile-switch-project))))
            ("Jump to bookmark" "bookmark"
             (lambda (_) (call-interactively (command-remapping 'bookmark-jump))))
            ("Edit emacs.d" "tools"
             (lambda (_) (find-file (expand-file-name "init.el" doom-emacs-dir))))))))
