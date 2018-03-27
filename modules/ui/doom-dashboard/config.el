;;; ui/doom-dashboard/config.el -*- lexical-binding: t; -*-

(defvar +doom-dashboard-name " *doom*"
  "The name to use for the dashboard buffer.")

(defvar +doom-dashboard-functions '(doom-dashboard-widget-banner
                                    doom-dashboard-widget-shortmenu
                                    doom-dashboard-widget-loaded)
  "List of widget functions to run in the dashboard buffer to construct the
dashboard. These functions take no arguments and the dashboard buffer is current
while they run.")

(defvar +doom-dashboard-inhibit-refresh nil
  "If non-nil, the doom buffer won't be refreshed.")

(defvar +doom-dashboard-inhibit-functions ()
  "A list of functions which take no arguments. If any of them return non-nil,
dashboard reloading is inhibited.")

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
(defvar +doom-dashboard--pwd-alist ())

(defvar all-the-icons-scale-factor)
(defvar all-the-icons-default-adjust)


;;
;; Bootstrap
;;

(setq doom-fallback-buffer-name +doom-dashboard-name
      initial-buffer-choice #'+doom-dashboard-initial-buffer)

(add-hook 'window-setup-hook #'+doom-dashboard|init)


;;
;; Major mode
;;

(define-derived-mode +doom-dashboard-mode special-mode
  (format "DOOM v%s" doom-version)
  :syntax-table nil
  :abbrev-table nil
  "Major mode for the DOOM dashboard buffer."
  (setq truncate-lines t
        buffer-read-only t)
  (setq-local whitespace-style nil)
  (setq-local show-trailing-whitespace nil)
  (cl-loop for (car . _cdr) in fringe-indicator-alist
           collect (cons car nil) into alist
           finally do (setq fringe-indicator-alist alist))
  (add-hook 'post-command-hook #'+doom-dashboard|reposition-point nil t))

(map! :map +doom-dashboard-mode-map
      "n"            #'forward-button
      :gn [down]     #'forward-button
      :gn "C-n"      #'forward-button
      :gn [tab]      #'forward-button
      :gn "TAB"      #'forward-button
      "p"            #'backward-button
      :gn [up]       #'backward-button
      :gn "C-p"      #'backward-button
      :gn [backtab]  #'backward-button
      :gn "S-TAB"    #'backward-button
      (:when (featurep! :feature evil)
        :m "j" #'forward-button
        :m "k" #'backward-button

        [remap evil-delete]        #'ignore
        [remap evil-delete-line]   #'ignore
        [remap evil-insert]        #'ignore
        [remap evil-append]        #'ignore
        [remap evil-replace]       #'ignore
        [remap evil-replace-state] #'ignore
        [remap evil-change]        #'ignore
        [remap evil-change-line]   #'ignore
        [remap evil-visual-char]   #'ignore
        [remap evil-visual-line]   #'ignore))


;;
;; Hooks
;;

(defun +doom-dashboard|reposition-point ()
  "Trap the point in the buttons."
  (when (region-active-p)
    (deactivate-mark t)
    (when (bound-and-true-p evil-mode)
      (evil-change-to-previous-state)))
  (or (ignore-errors
        (if (button-at (point))
            (forward-button 0)
          (backward-button 1)))
      (progn (goto-char (point-min))
             (forward-button 1))))

(defun +doom-dashboard|init ()
  "Initializes Doom's dashboard."
  (add-hook 'window-configuration-change-hook #'+doom-dashboard|resize)
  (add-hook 'kill-buffer-query-functions #'+doom-dashboard|reload-on-kill)
  (add-hook 'doom-after-switch-buffer-hook #'+doom-dashboard|reload-on-kill)
  (when (daemonp)
    (add-hook 'after-make-frame-functions #'+doom-dashboard|make-frame))
  ;; `persp-mode' integration: update `default-directory' when switching
  (add-hook 'persp-created-functions #'+doom-dashboard|record-project)
  (add-hook 'persp-activated-functions #'+doom-dashboard|detect-project)
  (add-hook 'persp-before-switch-functions #'+doom-dashboard|record-project)
  (+doom-dashboard-reload t))

(defun +doom-dashboard|reload-on-kill ()
  "A `kill-buffer-query-functions' hook. If this isn't a dashboard buffer, move
along, but record its `default-directory' if the buffer is real. See
`doom-real-buffer-p' for an explanation for what 'real' means.

If this is the dashboard buffer, reload the dashboard."
  (or (let ((buf (current-buffer)))
        (unless (+doom-dashboard-p buf)
          (when (doom-real-buffer-p buf)
            (setq +doom-dashboard--last-cwd default-directory)
            (+doom-dashboard-update-pwd))
          t))
      (ignore
       (let (+doom-dashboard-inhibit-refresh)
         (ignore-errors (+doom-dashboard-reload))))))

(defun +doom-dashboard|make-frame (frame)
  "Reload the dashboard after a brief pause. This is necessary for new frames,
whose dimensions may not be fully initialized by the time this is run."
  (run-with-timer 0.1 nil #'+doom-dashboard/open frame))

(defun +doom-dashboard|resize ()
  "Resize the margins and fringes on dashboard windows."
  (dolist (win (get-buffer-window-list (doom-fallback-buffer) nil t))
    (set-window-fringes win 0 0)
    (set-window-margins
     win (max 0 (/ (- (window-total-width win) +doom-dashboard--width) 2)))))

(defun +doom-dashboard|detect-project (&rest _)
  "Check for a `last-project-root' parameter in the perspective, and set the
dashboard's `default-directory' to it if it exists.

This and `+doom-dashboard|record-project' provides `persp-mode' integration with
the Doom dashboard. It ensures that the dashboard is always in the correct
project (which may be different across perspective)."
  (when (bound-and-true-p persp-mode)
    (when-let* ((pwd (persp-parameter 'last-project-root)))
      (+doom-dashboard-update-pwd pwd))))

(defun +doom-dashboard|record-project (&optional persp &rest _)
  "Record the last `doom-project-root' for the current perspective. See
`+doom-dashboard|detect-project' for more information."
  (when (bound-and-true-p persp-mode)
    (set-persp-parameter
     'last-project-root (doom-project-root)
     (if (perspective-p persp) persp (get-current-persp)))))


;;
;; Library
;;

(defun +doom-dashboard-initial-buffer ()
  "Returns buffer to display on startup. Designed for `initial-buffer-choice'."
  (if (doom-real-buffer-p)
      (current-buffer)
    (doom-fallback-buffer)))

(defun +doom-dashboard-p (buffer)
  "Returns t if BUFFER is the dashboard buffer."
  (eq buffer (doom-fallback-buffer)))

(defun +doom-dashboard-update-pwd (&optional pwd)
  "Update `default-directory' in the Doom dashboard buffer. What it is set to is
controlled by `+doom-dashboard-pwd-policy'."
  (if pwd
      (with-current-buffer (doom-fallback-buffer)
        (setq-local default-directory pwd))
    (let ((new-pwd (+doom-dashboard--get-pwd)))
      (when (and new-pwd (file-directory-p new-pwd))
        (unless (string-suffix-p "/" new-pwd)
          (setq new-pwd (concat new-pwd "/")))
        (+doom-dashboard-update-pwd new-pwd)))))

(defun +doom-dashboard-reload (&optional force)
  "Update the DOOM scratch buffer (or create it, if it doesn't exist)."
  (when (or (and (not +doom-dashboard-inhibit-refresh)
                 (get-buffer-window (doom-fallback-buffer))
                 (not (window-minibuffer-p (frame-selected-window)))
                 (not (run-hook-with-args-until-success '+doom-dashboard-inhibit-functions)))
            force)
    (with-current-buffer (doom-fallback-buffer)
      (with-silent-modifications
        (save-excursion
          (unless (eq major-mode '+doom-dashboard-mode)
            (+doom-dashboard-mode))
          (erase-buffer)
          (save-excursion (mapc #'funcall +doom-dashboard-functions))
          (insert
           (make-string (max 0 (- (/ (window-height (get-buffer-window)) 2)
                                  (/ (count-lines (point-min) (point-max)) 2)))
                        ?\n))))
      (+doom-dashboard|detect-project)
      (+doom-dashboard|resize)
      (+doom-dashboard-update-pwd)
      (current-buffer))))

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


;;
;; Widgets
;;

(defun doom-dashboard-widget-banner ()
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

(defun doom-dashboard-widget-loaded ()
  (insert
   "\n\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (doom-packages--benchmark))
    'face 'font-lock-comment-face)
   "\n\n"))

(defun doom-dashboard-widget-shortmenu ()
  (let ((all-the-icons-scale-factor 1.45)
        (all-the-icons-default-adjust -0.02))
    (insert "\n")
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
            ,(when (featurep! :lang org)
               '("See agenda for this week" "calendar"
                 (call-interactively #'org-agenda-list)))
            ("Recently opened files" "file-text"
             (call-interactively (or (command-remapping #'recentf-open-files)
                                     #'recentf-open-files)))
            ("Open project" "briefcase"
             (call-interactively (or (command-remapping #'projectile-switch-project)
                                     #'projectile-switch-project)))
            ("Jump to bookmark" "bookmark"
             (call-interactively (or (command-remapping #'bookmark-jump)
                                     #'bookmark-jump)))
            ,(when (featurep! :config private)
               '("Open private configuration" "settings"
                 (find-file (expand-file-name "config.el" +private-config-path))))
            ("Edit my modules list" "pencil"
             (if (featurep! :config private)
                 (let ((init-file (expand-file-name "init.el" +private-config-path)))
                   (unless (file-exists-p init-file)
                     (make-directory (file-name-directory init-file) t)
                     (copy-file (expand-file-name "init.example.el" doom-emacs-dir) init-file t))
                   (find-file init-file))
               (find-file user-init-file)))
            ("Edit Doom Emacs" "tools"
             (doom-project-find-file doom-emacs-dir))))))
