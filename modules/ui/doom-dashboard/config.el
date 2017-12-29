;;; ui/doom-dashboard/config.el -*- lexical-binding: t; -*-

(defvar +doom-dashboard-name " *doom*"
  "The name to use for the dashboard buffer.")

(defvar +doom-dashboard-widgets '(banner shortmenu loaded)
  "List of widgets to display in a blank scratch buffer.")

(defvar +doom-dashboard-inhibit-refresh nil
  "If non-nil, the doom buffer won't be refreshed.")

(defvar +doom-dashboard-inhibit-functions ()
  "A list of functions that determine whether to inhibit the dashboard from
loading.")

(defvar +doom-dashboard-pwd-policy 'last-project
  "The policy to use when setting the `default-directory' in the dashboard.

Possible values:

  'last-project  the `doom-project-root' of the last open buffer
  'last          the `default-directory' of the last open buffer
  a FUNCTION     a function run with the `default-directory' of the last
                 open buffer, that returns a directory path
  a STRING       a fixed path
  nil            `default-directory' will never change")

;;
(defvar +doom-dashboard--last-cwd nil)
(defvar +doom-dashboard--width 80)
(defvar +doom-dashboard--height 0)
(defvar +doom-dashboard--old-fringe-indicator fringe-indicator-alist)

(defvar all-the-icons-scale-factor)
(defvar all-the-icons-default-adjust)

;;
(setq doom-fallback-buffer +doom-dashboard-name)


(define-derived-mode +doom-dashboard-mode special-mode
  (format "DOOM v%s" doom-version)
  "Major mode for the DOOM dashboard buffer."
  (read-only-mode +1)
  (setq truncate-lines t)
  (setq-local whitespace-style nil)
  (setq-local show-trailing-whitespace nil)
  (cl-loop for (car . _cdr) in fringe-indicator-alist
           collect (cons car nil) into alist
           finally do (setq fringe-indicator-alist alist)))

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
  (add-hook 'focus-in-hook #'+doom-dashboard-reload)
  (add-hook 'kill-buffer-query-functions #'+doom-dashboard|reload-on-kill)
  (when (and (display-graphic-p) (not (daemonp)))
    (let ((default-directory doom-emacs-dir))
      (+doom-dashboard/open (selected-frame)))))

(defun +doom-dashboard|reload-on-kill ()
  "If this isn't a dashboard buffer, move along, but record its
`default-directory' if the buffer is real. See `doom-real-buffer-p' for an
explanation for what 'real' means.

If this is the dashboard buffer, reload the dashboard."
  (or (unless (+doom-dashboard-p)
        (when (doom-real-buffer-p)
          (setq +doom-dashboard--last-cwd default-directory)
          (+doom-dashboard-update-pwd))
        t)
      (ignore
       (let (+doom-dashboard-inhibit-refresh)
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


;;
(defun +doom-dashboard/open (frame)
  (interactive (list (selected-frame)))
  (unless (run-hook-with-args-until-success '+doom-dashboard-inhibit-functions)
    (unless +doom-dashboard-inhibit-refresh
      (with-selected-frame frame
        (switch-to-buffer (doom-fallback-buffer))
        (+doom-dashboard-reload)))
    (setq +doom-dashboard-inhibit-refresh nil)))

;
(defun +doom-dashboard-p (&optional buffer)
  "Returns t if BUFFER is the dashboard buffer."
  (eq (or buffer (current-buffer))
      (doom-fallback-buffer)))

(defun +doom-dashboard-update-pwd ()
  "TODO"
  (with-current-buffer (doom-fallback-buffer)
    (cd (or (+doom-dashboard--get-pwd)
            default-directory))))

(defun +doom-dashboard-reload (&optional force)
  "Update the DOOM scratch buffer (or create it, if it doesn't exist)."
  (let ((fallback-buffer (doom-fallback-buffer)))
    (when (or (and after-init-time
                   (not +doom-dashboard-inhibit-refresh)
                   (get-buffer-window fallback-buffer)
                   (not (window-minibuffer-p (frame-selected-window))))
              force)
      (with-current-buffer fallback-buffer
        (+doom-dashboard-update-pwd)
        (with-silent-modifications
          (unless (eq major-mode '+doom-dashboard-mode)
            (+doom-dashboard-mode))
          (erase-buffer)
          (let ((+doom-dashboard--height
                 (window-height (get-buffer-window fallback-buffer)))
                (lines 1)
                content)
            (with-temp-buffer
              (dolist (widget-name +doom-dashboard-widgets)
                (funcall (intern (format "doom-dashboard-widget--%s" widget-name)))
                (insert "\n"))
              (setq content (buffer-string)
                    lines (count-lines (point-min) (point-max))))
            (insert (make-string (max 0 (- (/ +doom-dashboard--height 2)
                                           (/ lines 2)))
                                 ?\n)
                    content))
          (unless (button-at (point))
            (goto-char (next-button (point-min)))))))
    ;; Update all dashboard windows
    (dolist (win (get-buffer-window-list fallback-buffer nil t))
      (set-window-fringes win 0 0)
      (set-window-margins
       win (max 0 (/ (- (window-total-width win) +doom-dashboard--width) 2)))))
  t)

;; helpers
(defun +doom-dashboard--center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))

(defun +doom-dashboard--get-pwd ()
  (let ((lastcwd +doom-dashboard--last-cwd)
        (policy +doom-dashboard-pwd-policy))
    (cond ((null policy)
           default-directory)
          ((stringp policy)
           (expand-file-name policy lastcwd))
          ((functionp policy)
           (funcall policy lastcwd))
          ((null lastcwd)
           default-directory)
          ((eq policy 'last-project)
           (let ((cwd default-directory)
                 (default-directory lastcwd))
             (if (doom-project-p)
                 (doom-project-root)
               cwd)))
          ((eq policy 'last)
           lastcwd)
          (t
           (warn "`+doom-dashboard-pwd-policy' has an invalid value of '%s'"
                 policy)))))

;; widgets
(defun doom-dashboard-widget--banner ()
  (mapc (lambda (line)
          (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
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
   "\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (format "Loaded %d packages in %d modules in %.02fs"
             (length doom--package-load-path)
             (hash-table-size doom-modules)
             (if (floatp doom-init-time) doom-init-time 0.0)))
    'face 'font-lock-comment-face)
   "\n"))

(defun doom-dashboard-widget--shortmenu ()
  (let ((all-the-icons-scale-factor 1.45)
        (all-the-icons-default-adjust -0.02))
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
                   (+doom-dashboard--center (- +doom-dashboard--width 2) (buffer-string)))
                 "\n\n"))))
          `(("Homepage" "mark-github"
             (browse-url "https://github.com/hlissner/doom-emacs"))
            ,(when (and (featurep! :feature workspaces)
                        (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
               '("Reload last session" "history"
                 (+workspace/load-session)))
            ,(when (featurep! :org org)
               '("See agenda for this week" "calendar"
                 (call-interactively 'org-agenda-list)))
            ("Recently opened files" "file-text"
             (call-interactively (command-remapping 'recentf-open-files)))
            ("Open project" "briefcase"
             (call-interactively (command-remapping 'projectile-switch-project)))
            ("Jump to bookmark" "bookmark"
             (call-interactively (command-remapping 'bookmark-jump)))
            ("Edit emacs.d" "tools"
             (find-file (expand-file-name "init.el" doom-emacs-dir)))))))
