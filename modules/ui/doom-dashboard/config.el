;;; ui/doom-dashboard/config.el -*- lexical-binding: t; -*-

(defvar +doom-dashboard-name " *doom*"
  "TODO")

(defvar +doom-dashboard-modeline nil
  "TODO")

(defvar +doom-dashboard-inhibit-refresh nil
  "If non-nil, the doom buffer won't be refreshed.")

(defvar +doom-dashboard-widgets '(banner shortmenu loaded)
  "List of widgets to display in a blank scratch buffer.")

(define-derived-mode +doom-dashboard-mode special-mode
  (concat "v" doom-version)
  "Major mode for the DOOM dashboard buffer."
  (read-only-mode +1)
  (setq truncate-lines t
        mode-line-format +doom-dashboard-modeline)
  (cl-loop for (car . _cdr) in fringe-indicator-alist
           collect (cons car nil) into alist
           finally do (setq fringe-indicator-alist alist)))


(defvar +doom-dashboard--width 0)
(defvar +doom-dashboard--height 0)
(defvar +doom-dashboard--old-fringe-indicator fringe-indicator-alist)
(defvar +doom-dashboard--old-modeline nil)

(setq doom-fallback-buffer +doom-dashboard-name)


;;
(map! :map +doom-dashboard-mode-map
      "n" #'+doom-dashboard/next-button
      "p" #'+doom-dashboard/previous-button
      "N" #'+doom-dashboard/last-button
      "P" #'+doom-dashboard/first-button
      :em "j" #'+doom-dashboard/next-button
      :em "k" #'+doom-dashboard/previous-button
      :em "gg" #'+doom-dashboard/first-button
      :em "G"  #'+doom-dashboard/last-button
      [remap evil-insert]      #'evil-normal-state
      [remap evil-change]      #'evil-normal-state
      [remap evil-visual-char] #'evil-normal-state
      [remap evil-visual-line] #'evil-normal-state
      [remap evil-delete]      #'evil-normal-state
      [remap evil-delete-char] #'evil-normal-state)

;;
(defun +doom-dashboard|init ()
  "Initialize doom-dashboard and set up its hooks; possibly open the dashboard
if in a GUI/non-daemon session."
  (add-hook 'window-configuration-change-hook #'+doom-dashboard-reload)
  (add-hook 'kill-buffer-query-functions #'+doom-dashboard|kill-buffer-query-fn)
  (when (and (display-graphic-p) (not (daemonp)))
    (+doom-dashboard/open (selected-frame))))

(defun +doom-dashboard|kill-buffer-query-fn ()
  (or (not (+doom-dashboard-p))
      (ignore (let (+doom-dashboard-inhibit-refresh)
                (ignore-errors (+doom-dashboard-reload))))))

(defun +doom-dashboard|make-frame (frame)
  "Reload the dashboard after a brief pause. This is necessary for new frames,
whose dimensions may not be fully initialized by the time this is run."
  (run-with-timer 0.1 nil #'+doom-dashboard/open frame))

(defun +doom-dashboard|server-visit (&rest _)
  "Inhibit dashboard refresh when opening files via emacsclient."
  (setq +doom-dashboard-inhibit-refresh t))

(add-hook 'window-setup-hook #'+doom-dashboard|init)
(add-hook 'after-make-frame-functions #'+doom-dashboard|make-frame)
(add-hook 'server-visit-hook #'+doom-dashboard|server-visit)
(add-hook '+doom-dashboard-mode-hook #'doom|disable-vi-tilde-fringe)


;;
(defun +doom-dashboard/open (frame)
  (interactive (list (selected-frame)))
  (unless +doom-dashboard-inhibit-refresh
    (with-selected-frame frame
      (switch-to-buffer (doom-fallback-buffer))
      (+doom-dashboard-reload)))
  (setq +doom-dashboard-inhibit-refresh nil))

(defun +doom-dashboard-p (&optional buffer)
  "Returns t if BUFFER is the dashboard buffer."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (eq buffer (doom-fallback-buffer)))))

(defun +doom-dashboard-center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))

(defun +doom-dashboard-deferred-reload (frame)
  "Reload the dashboard after a brief pause. This is necessary for new frames,
whose dimensions may not be fully initialized by the time this is run."
  (run-with-timer 0.05 nil
                  (lambda (frame)
                    (with-selected-frame frame
                      (+doom-dashboard/open t)))
                  frame))

(defun +doom-dashboard-reload (&optional dir)
  "Update the DOOM scratch buffer (or create it, if it doesn't exist)."
  (when (and (not +doom-dashboard-inhibit-refresh)
             (not (window-minibuffer-p (frame-selected-window)))
             (get-buffer-window (doom-fallback-buffer)))
    (unless +doom-dashboard-modeline
      (setq +doom-dashboard--old-modeline mode-line-format
            +doom-dashboard-modeline
            (or (and (featurep! :ui doom-modeline)
                     (doom-modeline 'project))
                mode-line-format)))
    (let ((old-pwd (or dir default-directory)))
      (with-current-buffer (doom-fallback-buffer)
        (with-silent-modifications
          (unless (eq major-mode '+doom-dashboard-mode)
            (+doom-dashboard-mode))
          (erase-buffer)
          (setq default-directory old-pwd)
          (let ((+doom-dashboard--width  (window-width))
                (+doom-dashboard--height (window-height)))
            (insert (make-string (max 0 (- (truncate (/ +doom-dashboard--height 2)) 16)) ?\n))
            (dolist (widget-name +doom-dashboard-widgets)
              (funcall (intern (format "doom-dashboard-widget--%s" widget-name)))
              (insert "\n")))
          (unless (button-at (point))
            (goto-char (next-button (point-min))))))))
  t)

;; widgets
(defun doom-dashboard-widget--banner ()
  (mapc (lambda (line)
          (insert (propertize (+doom-dashboard-center +doom-dashboard--width line)
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
    (+doom-dashboard-center
     +doom-dashboard--width
     (format "Loaded %d packages in %.03fs "
             (- (length load-path) (length doom--base-load-path))
             (if (floatp doom-init-time) doom-init-time 0.0)))
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
              (cl-destructuring-bind (label icon fn) btn
                (insert
                 (with-temp-buffer
                   (insert-text-button
                    (concat (all-the-icons-octicon icon :face 'font-lock-keyword-face)
                            (propertize (concat " " label) 'face 'font-lock-keyword-face))
                    'action `(lambda (_) ,fn)
                    'follow-link t)
                   (+doom-dashboard-center (1- +doom-dashboard--width) (buffer-string)))
                 "\n\n"))))
          `(("Homepage" "mark-github"
             (browse-url "https://github.com/hlissner/.emacs.d"))
            ,(when last-session-p
               '("Reload last session" "history"
                 (+workspace/load-session)))
            ("Recently opened files" "file-text"
             (call-interactively (command-remapping 'recentf)))
            ("Recent opened projects" "briefcase"
             (call-interactively (command-remapping 'projectile-switch-project)))
            ("Jump to bookmark" "bookmark"
             (call-interactively (command-remapping 'bookmark-jump)))
            ("Edit emacs.d" "tools"
             (find-file (expand-file-name "init.el" doom-emacs-dir)))))))
