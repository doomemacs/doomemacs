;;; ui/dashboard/config.el -*- lexical-binding: t; -*-

(defgroup +dashboard nil
  "Manage how Doom's dashboard is coloured and themed."
  :group 'doom+)

(defcustom +dashboard-name "*doom*"
  "The name to use for the dashboard buffer."
  :type 'string
  :group '+dashboard)

(defcustom +dashboard-functions
  `(+dashboard-widget-banner
    +dashboard-widget-shortmenu
    +dashboard-widget-footer
    +dashboard-widget-loaded)
  "List of widget functions to run in the dashboard buffer to construct the
dashboard. These functions take no arguments and the dashboard buffer is current
while they run."
  :type 'hook
  :group '+dashboard)

(defcustom +dashboard-banner-file "default.png"
  "The path to the image file to be used in on the dashboard. The path is
relative to `+dashboard-banner-dir'. If nil, always use the ASCII banner."
  :type 'string
  :group '+dashboard)

(defcustom +dashboard-banner-dir (concat (dir!) "/banners/")
  "Where to look for `+dashboard-banner-file'."
  :type 'directory
  :group '+dashboard)

(defcustom +dashboard-ascii-banner-fn #'+dashboard-draw-ascii-banner-fn
  "The function used to generate the ASCII banner on Doom's dashboard."
  :type 'function
  :group '+dashboard)

(defcustom +dashboard-banner-vertical-padding '(2 . 3)
  "Number of newlines to pad the banner with, above and below, respectively."
  :type '(cons integer integer)
  :group '+dashboard)

(defcustom +dashboard-pwd-policy 'last-project
  "The policy to use when setting the `default-directory' in the dashboard.

Possible values:
  \\='last-project
    The `doom-project-root' of the last open buffer. Falls back to
    `default-directory' if not in a project.
  \\='last
    The `default-directory' of the last open buffer
  a FUNCTION
    A function run with the `default-directory' of the last open buffer, that
    returns a directory path
  a STRING
    A fixed path
  nil
    `default-directory' will never change"
  :type '(radio
          (const :tag "The project root of the last open buffer (or `default-directory')" last-project)
          (const :tag "The `default-directory' of the last open buffer." last)
          (function :tag "Return what directory to use")
          (directory :tag "A fixed directory path")
          (const :tag "Never change the dashboard's `default-directory'" nil))
  :group '+dashboard)

(defcustom +dashboard-menu-sections
  '(("Recently opened files"
     :icon (nerd-icons-faicon "nf-fa-file_text" :face '+dashboard-menu-title)
     :action recentf-open-files)
    ("Reload last session"
     :icon (nerd-icons-octicon "nf-oct-history" :face '+dashboard-menu-title)
     :when (cond ((modulep! :ui workspaces)
                  (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
                 ((require 'desktop nil t)
                  (file-exists-p (desktop-full-file-name))))
     :action doom/quickload-session)
    ("Open org-agenda"
     :icon (nerd-icons-octicon "nf-oct-calendar" :face '+dashboard-menu-title)
     :when (fboundp 'org-agenda)
     :action org-agenda)
    ("Open project"
     :icon (nerd-icons-octicon "nf-oct-briefcase" :face '+dashboard-menu-title)
     :action projectile-switch-project)
    ("Jump to bookmark"
     :icon (nerd-icons-octicon "nf-oct-bookmark" :face '+dashboard-menu-title)
     :action bookmark-jump)
    ("Open private configuration"
     :icon (nerd-icons-octicon "nf-oct-tools" :face '+dashboard-menu-title)
     :when (file-directory-p doom-user-dir)
     :action doom/open-private-config)
    ("Open documentation"
     :icon (nerd-icons-octicon "nf-oct-book" :face '+dashboard-menu-title)
     :action doom/help))
  "An alist of menu buttons used by `+dashboard-widget-shortmenu'. Each
element is a cons cell (LABEL . PLIST). LABEL is a string to display after the
icon and before the key string.

PLIST can have the following properties:

  :icon FORM
    Uses the return value of FORM as an icon (can be literal string).
  :key STRING
    The keybind displayed next to the button.
  :when FORM
    If FORM returns nil, don't display this button.
  :face FACE
    Displays the icon and text with FACE (a face symbol).
  :action FORM
    Run FORM when the button is pushed."
  :type 'alist
  :group '+dashboard)

;;
(defvar +dashboard-inhibit-refresh nil
  "If non-nil, the doom buffer won't be refreshed.")

(defvar +dashboard-inhibit-functions ()
  "A list of functions which take no arguments. If any of them return non-nil,
dashboard reloading is inhibited.")

(defvar +dashboard--last-cwd nil)
(defvar +dashboard--reload-timer nil)


;;
;;; Faces

(defface +dashboard-banner '((t (:inherit font-lock-comment-face)))
  "Face used for the DOOM banner on the dashboard"
  :group '+dashboard)

(defface +dashboard-footer '((t (:inherit font-lock-keyword-face)))
  "Face used for the footer on the dashboard"
  :group '+dashboard)

(defface +dashboard-footer-icon '((t (:inherit nerd-icons-green)))
  "Face used for the icon of the footer on the dashboard"
  :group '+dashboard)

(defface +dashboard-loaded '((t (:inherit font-lock-comment-face)))
  "Face used for the loaded packages benchmark"
  :group '+dashboard)

(defface +dashboard-menu-desc '((t (:inherit font-lock-constant-face)))
  "Face used for the key description of menu widgets on the dashboard"
  :group '+dashboard)

(defface +dashboard-menu-title '((t (:inherit font-lock-keyword-face)))
  "Face used for the title of menu widgets on the dashboard"
  :group '+dashboard)


;;
;;; Major mode

(define-derived-mode +dashboard-mode special-mode
  (format "DOOM v%s" doom-version)
  "Major mode for the DOOM dashboard buffer."
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq-local revert-buffer-function #'+dashboard-revert-buffer-fn)
  (setq truncate-lines t)
  (setq-local whitespace-style nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local hscroll-margin 0)
  (setq-local tab-width 2)
  ;; Don't scroll to follow cursor
  (setq-local scroll-preserve-screen-position nil)
  (setq-local auto-hscroll-mode nil)
  ;; Line numbers are ugly with large margins
  (setq-local display-line-numbers-type nil)
  ;; Ensure the ever-changing margins don't screw with the mode-line's
  ;; right-alignment (see #8114).
  (setq-local mode-line-right-align-edge 'right-margin)
  ;; Ensure point is always on a button
  (add-hook 'post-command-hook #'+dashboard-reposition-point-h nil 'local)
  ;; hl-line produces an ugly cut-off line highlight in the dashboard, so don't
  ;; activate it there (by pretending it's already active).
  (setq-local hl-line-mode t)
  ;; Local variables are never important in the dashboard, and may cause repeat
  ;; prompts about unsafe/risky variables.
  (setq-local enable-local-variables nil))

(define-key! +dashboard-mode-map
  [left-margin mouse-1]   #'ignore
  [remap forward-button]  #'+dashboard/forward-button
  [remap backward-button] #'+dashboard/backward-button
  "n"       #'forward-button
  "p"       #'backward-button
  "C-n"     #'forward-button
  "C-p"     #'backward-button
  [down]    #'forward-button
  [up]      #'backward-button
  [tab]     #'forward-button
  [backtab] #'backward-button

  ;; Evil remaps
  [remap evil-next-line]     #'forward-button
  [remap evil-previous-line] #'backward-button
  [remap evil-next-visual-line]     #'forward-button
  [remap evil-previous-visual-line] #'backward-button
  [remap evil-paste-pop-next] #'forward-button
  [remap evil-paste-pop]      #'backward-button
  [remap evil-delete]         #'ignore
  [remap evil-delete-line]    #'ignore
  [remap evil-insert]         #'ignore
  [remap evil-append]         #'ignore
  [remap evil-replace]        #'ignore
  [remap evil-enter-replace-state] #'ignore
  [remap evil-change]         #'ignore
  [remap evil-change-line]    #'ignore
  [remap evil-visual-char]    #'ignore
  [remap evil-visual-line]    #'ignore)


;;
;;; Bootstrap

(defun +dashboard-init-h ()
  "Initializes Doom's dashboard."
  (unless noninteractive
    (setq doom-fallback-buffer-name +dashboard-name
          initial-buffer-choice #'doom-fallback-buffer)
    ;; Ensure the dashboard becomes Emacs' go-to buffer when there's nothing
    ;; else to show.
    (unless fancy-splash-image
      (setq fancy-splash-image
            (expand-file-name +dashboard-banner-file
                              +dashboard-banner-dir)))
    (+dashboard-reload)
    (add-hook 'doom-load-theme-hook #'+dashboard-reload-on-theme-change-h)
    ;; Ensure the dashboard is up-to-date whenever it is switched to or resized.
    (add-hook 'window-size-change-functions #'+dashboard-resize-h)
    (add-hook 'doom-switch-buffer-hook #'+dashboard-reload-maybe-h)
    (add-hook 'delete-frame-functions #'+dashboard-reload-frame-h)
    ;; `persp-mode' integration: update `default-directory' when switching perspectives
    (add-hook 'persp-created-functions #'+dashboard--persp-record-project-h)
    (add-hook 'persp-activated-functions #'+dashboard--persp-detect-project-h)
    ;; Fix #2219 where, in GUI daemon frames, the dashboard loses center
    ;; alignment after switching (or killing) workspaces.
    (when (daemonp)
      (add-hook 'persp-activated-functions #'+dashboard-reload-maybe-h))
    (add-hook 'persp-before-switch-functions #'+dashboard--persp-record-project-h)))

(add-hook 'doom-init-ui-hook #'+dashboard-init-h 'append)

;; PERF: Make sure the dashboard is ready early, so as to avoid triggering
;;   `doom-first-buffer-hook' later, when switching to it.
(when (and (doom-context-p 'startup)
           (equal (buffer-name) "*scratch*"))
  (let (buffer-list-update-hook
        doom-first-buffer-hook)
    (switch-to-buffer +dashboard-name)))


;;
;;; Hooks

(defun +dashboard-revert-buffer-fn (&optional _ignore-auto _no-confirm)
  "`revert-buffer-function' for `+dashboard-mode'."
  (+dashboard-reload t))

(defun +dashboard-reposition-point-h ()
  "Trap the point in the buttons."
  (when (region-active-p)
    (setq deactivate-mark t)
    (when (bound-and-true-p evil-local-mode)
      (evil-change-to-previous-state)))
  (or (ignore-errors
        (if (button-at (point))
            (forward-button 0)
          (backward-button 1)))
      (ignore-errors
        (goto-char (point-min))
        (forward-button 1)))
  (unless (button-at (point))
    (setq-local cursor-type nil
                evil-normal-state-cursor nil)))

(defun +dashboard-reload-maybe-h (&rest _)
  "Reload the dashboard or its state.

If this isn't a dashboard buffer, move along, but record its `default-directory'
if the buffer is real. See `doom-real-buffer-p' for an explanation for what
\\='real' means.

If this is the dashboard buffer, reload it completely."
  (cond ((+dashboard-buffer-p (current-buffer))
         (let (+dashboard-inhibit-refresh)
           (ignore-errors (+dashboard-reload))))
        ((and (not (file-remote-p default-directory))
              (doom-real-buffer-p (current-buffer)))
         (setq +dashboard--last-cwd default-directory)
         (+dashboard-update-pwd-h))))

(defun +dashboard-reload-frame-h (_frame)
  "Reload the dashboard after a brief pause. This is necessary for new frames,
whose dimensions may not be fully initialized by the time this is run."
  (when (timerp +dashboard--reload-timer)
    (cancel-timer +dashboard--reload-timer)) ; in case this function is run rapidly
  (setq +dashboard--reload-timer
        (run-with-timer 0.1 nil #'+dashboard-reload t)))

(defun +dashboard-resize-h (&rest _)
  "Recenter the dashboard, and reset its margins and fringes."
  (let (buffer-list-update-hook
        window-configuration-change-hook
        window-size-change-functions)
    (when-let* ((windows (get-buffer-window-list (doom-fallback-buffer) nil t)))
      (dolist (w windows)
        (unless (= (window-start w) 1)
          (set-window-start w 0))
        (cl-destructuring-bind (left right &rest) (window-fringes w)
          (unless (and (= left 0)
                       (= right 0))
            (set-window-fringes w 0 0))))
      (with-current-buffer (doom-fallback-buffer)
        (save-excursion
          (with-silent-modifications
            (goto-char (point-min))
            (delete-region (line-beginning-position)
                           (save-excursion (skip-chars-forward "\n")
                                           (point)))
            (insert
             (make-string
              (+ (max 0 (- (/ (window-height (get-buffer-window)) 2)
                           (round (/ (count-lines (point-min) (point-max))
                                     2))))
                 (car +dashboard-banner-vertical-padding))
              ?\n))))))))

(defun +dashboard--persp-detect-project-h (&rest _)
  "Set dashboard's PWD to current persp's `last-project-root', if it exists.

This and `+dashboard--persp-record-project-h' provides `persp-mode'
integration with the Doom dashboard. It ensures that the dashboard is always in
the correct project (which may be different across perspective)."
  (when (bound-and-true-p persp-mode)
    (when-let* ((pwd (persp-parameter 'last-project-root)))
      (+dashboard-update-pwd-h pwd))))

(defun +dashboard--persp-record-project-h (&optional persp &rest _)
  "Record the last `doom-project-root' for the current persp.
See `+dashboard--persp-detect-project-h' for more information."
  (when (bound-and-true-p persp-mode)
    (set-persp-parameter
     'last-project-root (doom-project-root)
     (if (persp-p persp)
         persp
       (get-current-persp)))))


;;
;;; Library

(defun +dashboard-buffer-p (buffer)
  "Returns t if BUFFER is the dashboard buffer."
  (eq buffer (get-buffer +dashboard-name)))

(defun +dashboard-update-pwd-h (&optional pwd)
  "Update `default-directory' in the Doom dashboard buffer.
What it is set to is controlled by `+dashboard-pwd-policy'."
  (if pwd
      (with-current-buffer (doom-fallback-buffer)
        (doom-log "Changed dashboard's PWD to %s" pwd)
        (setq-local default-directory pwd))
    (let ((new-pwd (+dashboard--pwd)))
      (when (and new-pwd (file-accessible-directory-p new-pwd))
        (+dashboard-update-pwd-h
         (concat (directory-file-name new-pwd)
                 "/"))))))

(defun +dashboard-reload-on-theme-change-h ()
  "Forcibly reload the Doom dashboard when theme changes post-startup."
  (when after-init-time
    (+dashboard-reload 'force)))

(defun +dashboard-reload (&optional force)
  "Update the DOOM scratch buffer (or create it, if it doesn't exist)."
  (when (or (and (not +dashboard-inhibit-refresh)
                 (get-buffer-window (doom-fallback-buffer))
                 (not (window-minibuffer-p (frame-selected-window)))
                 (not (run-hook-with-args-until-success '+dashboard-inhibit-functions)))
            force)
    (with-current-buffer (doom-fallback-buffer)
      (doom-log "Reloading dashboard at %s" (format-time-string "%T"))
      (with-silent-modifications
        (let ((pt (point)))
          (unless (eq major-mode '+dashboard-mode)
            (+dashboard-mode))
          (erase-buffer)
          (run-hooks '+dashboard-functions)
          (goto-char pt)
          (+dashboard-reposition-point-h))
        (+dashboard-resize-h)
        (+dashboard--persp-detect-project-h)
        (+dashboard-update-pwd-h)
        (current-buffer)))))

;; helpers
(defun +dashboard-strlen (s)
  (let ((width (frame-char-width))
        (len (string-pixel-width s)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))

(defun +dashboard-maxlen (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let ((width 0))
      (while (< (point) (point-max))
        (let* ((line (buffer-substring (pos-bol) (pos-eol)))
               (len (+dashboard-strlen line)))
          (setq width (max width len)))
        (forward-line 1))
      width)))

(defun +dashboard-center (str)
  (let* ((width (+dashboard-maxlen str))
         (prefix (propertize " " 'display `(space . (:align-to (- center ,(/ (float width) 2)))))))
    (propertize str 'line-prefix prefix 'indent-prefix prefix)))

(defun +dashboard-insert-centered (&rest lines)
  (let* ((width (+dashboard-maxlen (string-join lines "\n")))
         (prefix (propertize " " 'display `(space . (:align-to (- center ,(/ (float width) 2)))))))
    (add-text-properties
     (point) (progn (mapc (lambda (l) (insert l "\n")) lines)
                    (point))
     `(line-prefix ,prefix indent-prefix ,prefix))))

(defun +dashboard--pwd ()
  (let ((lastcwd +dashboard--last-cwd)
        (policy +dashboard-pwd-policy))
    (cond ((null policy)
           default-directory)
          ((stringp policy)
           (expand-file-name policy lastcwd))
          ((functionp policy)
           (funcall policy lastcwd))
          ((null lastcwd)
           default-directory)
          ((eq policy 'last-project)
           (or (doom-project-root lastcwd)
               lastcwd))
          ((eq policy 'last)
           lastcwd)
          ((warn "`+dashboard-pwd-policy' has an invalid value of '%s'"
                 policy)))))


;;
;;; Widgets

(defun +dashboard-draw-ascii-banner-fn ()
  (propertize
   (string-join
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
      " `''                                                                      ``'")
    "\n")
   'face '+dashboard-banner))

(defun +dashboard-widget-banner ()
  (when-let*
      ((banner (and (functionp +dashboard-ascii-banner-fn)
                    (funcall +dashboard-ascii-banner-fn))))
    (let* ((width (+dashboard-maxlen banner))
           (text-prefix `(space . (:align-to (- center ,(/ width 2)))))
           (beg (point)))
      (insert banner)
      (if-let* (((stringp fancy-splash-image))
                ((file-readable-p fancy-splash-image))
                (image (create-image (fancy-splash-image-file)))
                (image-prefix `(space . (:align-to (- center (0.5 . ,image)))))
                (prefix
                 (propertize
                  " " 'display `((when (display-graphic-p) . ,image-prefix)
                                 (when (not (display-graphic-p)) . ,text-prefix)))))
          (progn
            (add-text-properties
             beg (point) `(display ,image line-prefix ,prefix wrap-prefix ,prefix))
            (insert "\n"))
        (add-text-properties beg (point) `(line-prefix ,text-prefix indent-prefix ,text-prefix)))
      (insert
       "\n" (propertize " " 'display
                        `(space
                          . (:height ,(or (cdr +dashboard-banner-vertical-padding) 0))))))))

(defun +dashboard-widget-loaded ()
  (when doom-init-time
    (+dashboard-insert-centered
     (propertize (doom-display-benchmark-h 'return)
                 'face '+dashboard-loaded))))

(defun +dashboard-widget-shortmenu ()
  (insert "\n")
  (dolist (section +dashboard-menu-sections)
    (cl-destructuring-bind (label &key icon action when face key) section
      (when (and (fboundp action)
                 (or (null when)
                     (eval when t)))
        (+dashboard-insert-centered
         (let ((icon (if (stringp icon) icon (eval icon t))))
           (format (format "%s%%s%%10s" (if icon "%3s\t" "%3s"))
                   (or icon "")
                   (with-temp-buffer
                     (insert-text-button
                      label
                      'action
                      `(lambda (_)
                         (call-interactively (or (command-remapping #',action)
                                                 #',action)))
                      'face (or face '+dashboard-menu-title)
                      'follow-link t
                      'help-echo
                      (format "%s (%s)" label
                              (propertize (symbol-name action) 'face '+dashboard-menu-desc)))
                     (format "%-38s" (buffer-string)))
                   ;; Lookup command keys dynamically
                   (propertize
                    (or key
                        (when-let*
                            ((keymaps
                              (delq
                               nil (list (when (bound-and-true-p evil-local-mode)
                                           (evil-get-auxiliary-keymap +dashboard-mode-map 'normal))
                                         +dashboard-mode-map)))
                             (key
                              (or (when keymaps
                                    (where-is-internal action keymaps t))
                                  (where-is-internal action nil t))))
                          (with-temp-buffer
                            (save-excursion (insert (key-description key)))
                            (while (re-search-forward "<\\([^>]+\\)>" nil t)
                              (let ((str (match-string 1)))
                                (replace-match
                                 (upcase (if (< (length str) 3)
                                             str
                                           (substring str 0 3))))))
                            (buffer-string)))
                        "")
                    'face '+dashboard-menu-desc)))
         (propertize "\n" 'display '(space . (:relative-height 0.01))))))))

(defun +dashboard-widget-footer ()
  (+dashboard-insert-centered
   (with-temp-buffer
     (insert (propertize " " 'display '(space . (:relative-height 2.0))) "\n")
     (insert-text-button (or (nerd-icons-codicon "nf-cod-octoface" :face '+dashboard-footer-icon :height 1.3 :v-adjust -0.15)
                             (propertize "github" 'face '+dashboard-footer))
                         'action (lambda (_) (browse-url "https://github.com/doomemacs/doomemacs"))
                         'follow-link t
                         'help-echo "Open Doom Emacs github page")
     (insert "\n" )
     (buffer-string))))

(defun +dashboard-widget-spacer ()
  (+dashboard-insert-centered
   (propertize "\n" 'display `(space . (:relative-height 0.5)))))
