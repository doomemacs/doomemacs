;;; core-modeline.el

;; This file tries to be an almost self-contained configuration of my mode-line.
;;
;; It depends on the following external packages:
;;   + REQUIRED
;;       + f
;;       + s
;;       + powerline
;;       + projectile
;;   + OPTIONAL
;;       + evil-mode
;;       + anzu + evil-anzu
;;       + iedit and evil-multiedit
;;       + flycheck
;;
;; Both are simple, isolated functions and, besides projectile, have no other
;; dependencies.

(defvar doom-modeline-height 29
  "How tall the mode-line should be. This is only respected in GUI emacs.")

(defvar doom-modeline-bar-width 3
  "How wide the mode-line bar should be. This is only respected in GUI emacs.")

;; Custom faces
(defface doom-modeline-alternate '((t (:inherit mode-line)))
  "Secondary color for the modeline.")

(defface doom-modeline-highlight '((t (:inherit mode-line)))
  "Face for bright segments of the mode-line.")

(defface doom-modeline-count '((t (:inherit mode-line)))
  "Face for 'X out of Y' segments, such as `*anzu', `*evil-substitute' and
`iedit'")

(defface doom-modeline-bar '((t (:inherit doom-modeline-highlight)))
  "The face used for the left-most bar on the mode-line of an active window.")

(defface doom-modeline-eldoc-bar '((t (:inherit doom-modeline-bar)))
  "The face used for the left-most bar on the mode-line when eldoc-eval is
active.")

(defface doom-modeline-inactive-bar '((t (:inherit mode-line-inactive)))
  "The face used for the left-most bar on the mode-line of an inactive window.")

(defface doom-modeline-count '((t (:inherit mode-line)))
  "Face for anzu/evil-substitute/evil-search number-of-matches display.")

(defface doom-modeline-info    '((t (:inherit success)))
  "Face for info-level messages in the modeline. Used by `*vc'.")
(defface doom-modeline-warning '((t (:inherit warning)))
  "Face for warnings in the modeline. Used by `*flycheck'")
(defface doom-modeline-urgent  '((t (:inherit error)))
  "Face for errors in the modeline. Used by `*flycheck'")


;;
;; Dependencies
;;

(require 'powerline)

(require 'all-the-icons)

(use-package eldoc-eval
  :config
  (setq eldoc-in-minibuffer-show-fn 'doom-eldoc-show-in-mode-line)
  (eldoc-in-minibuffer-mode +1))


;;
;; Functions
;;

(defun doom-ml-flycheck-count (state)
  "Return flycheck information for the given error type STATE."
  (when (flycheck-has-current-errors-p state)
    (if (eq 'running flycheck-last-status-change)
        "?"
      (cdr-safe (assq state (flycheck-count-errors flycheck-current-errors))))))

;; pyenv/rbenv version segment
(defvar doom-ml-env-version-hook '()
  "Hook that runs whenever the environment version changes (e.g. rbenv/pyenv)")

(defun doom-ml|env-update ()
  (when doom-ml--env-command
    (let ((default-directory (projectile-project-root)))
      (let ((s (shell-command-to-string doom-ml--env-command)))
        (setq doom-ml--env-version (if (string-match "[ \t\n\r]+\\'" s)
                                    (replace-match "" t t s)
                                  s))
        (run-hook-with-args 'doom-ml-env-version-hook doom-ml--env-version)))))

(defmacro def-version-cmd! (mode command)
  "Define a COMMAND for MODE that will set `doom-ml--env-command' when that mode
is activated, which should return the version number of the current environment.
It is used by `doom-ml|env-update' to display a version number in the modeline.
For instance:

  (def-version-cmd! ruby-mode \"ruby --version | cut -d' ' -f2\")

This will display the ruby version in the modeline in ruby-mode buffers. It is
cached the first time."
  (add-hook 'focus-in-hook 'doom-ml|env-update)
  (add-hook 'find-file-hook 'doom-ml|env-update)
  `(add-hook ',mode (lambda () (setq doom-ml--env-command ,command))))

(defun doom-make-xpm (color height width)
  "Create an XPM bitmap."
  (let ((data nil)
        (i 0))
    (setq data (make-list height (make-list width 1)))
    (pl/make-xpm "percent" color color (reverse data))))

(pl/memoize 'doom-make-xpm)


;;
;; Initialization
;;

;; Where (py|rb)env version strings will be stored
(defvar-local doom-ml--env-version nil)
(defvar-local doom-ml--env-command nil)

;; So the mode-line can keep track of "the current window"
(defvar doom-ml-selected-window nil)
(defun doom|set-selected-window (&rest _)
  (let ((window (frame-selected-window)))
    (when (and (windowp window)
               (not (minibuffer-window-active-p window)))
      (setq doom-ml-selected-window window))))
(add-hook 'window-configuration-change-hook #'doom|set-selected-window)
(add-hook 'focus-in-hook #'doom|set-selected-window)
(advice-add 'select-window :after 'doom|set-selected-window)
(advice-add 'select-frame  :after 'doom|set-selected-window)


;;
;; Mode-line segments
;;

(defun *buffer-path ()
  "Displays the buffer's full path relative to the project root (includes the
project root). Excludes the file basename. See `*buffer-name' for that."
  (when buffer-file-name
     (f-dirname
      (let ((buffer-path (file-relative-name buffer-file-name (projectile-project-root)))
            (max-length (truncate (/ (window-body-width) 1.75))))
        (concat (projectile-project-name) "/"
                (if (> (length buffer-path) max-length)
                    (let ((path (reverse (split-string buffer-path "/" t)))
                          (output ""))
                      (when (and path (equal "" (car path)))
                        (setq path (cdr path)))
                      (while (and path (<= (length output) (- max-length 4)))
                        (setq output (concat (car path) "/" output))
                        (setq path (cdr path)))
                      (when path
                        (setq output (concat "../" output)))
                      (when (string-suffix-p "/" output)
                        (setq output (substring output 0 -1)))
                      output)
                  buffer-path))))))

(defun *buffer-name ()
  "The buffer's base name or id."
  (if buffer-file-name
      (f-filename buffer-file-name)
    (s-trim-left (format-mode-line "%b"))))

(defun *buffer-project ()
  "Displays `default-directory', for special buffers like the scratch buffer."
  (concat
   " "
   (all-the-icons-octicon
    "file-directory"
    :face 'doom-modeline-alternate :v-adjust -0.05 :height 1.3)
   (propertize (concat " " (abbreviate-file-name default-directory))
               'face `(:inherit doom-modeline-alternate))))

(defun *buffer-state ()
  "Displays symbols representing the buffer's state; which can be one or more
of: non-existent, modified or read-only."
  (let ((state (list))
        (base-face (if active 'doom-default 'mode-line-inactive)))
    (when buffer-file-name
      (if (file-exists-p buffer-file-name)
          (when (buffer-modified-p)
            (push (all-the-icons-octicon "primitive-dot" :face `(minibuffer-prompt ,base-face) :height 1.1 :v-adjust -0.05) state))
        (push (all-the-icons-octicon "circle-slash" :face `(doom-modeline-urgent ,base-face) :height 1.1 :v-adjust -0.05) state)))
    (when buffer-read-only
      (push (all-the-icons-octicon "lock" :face `(doom-modeline-urgent ,base-face) :height 1.1 :v-adjust -0.05) state))
    (when state
      (concat (s-join (propertize " " 'face base-face) state)))))

(defun *buffer-info ()
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (let ((face (if active 'doom-default 'mode-line-inactive)))
    (concat (propertize " " 'face face)
            (propertize (or (*buffer-path) "") 'face `(:inherit (,face doom-modeline-alternate)))
            (propertize (or (*buffer-name) "") 'face face)
            (propertize " " 'face face)
            (let ((state (*buffer-state)))
              (if state (concat state (propertize " " 'face face)))))))

(defun *buffer-encoding-abbrev ()
  "The line ending convention used in the buffer (if it isn't unix) and its
character encoding (if it isn't UTF-8)."
  (let ((sys (symbol-name buffer-file-coding-system)))
    (concat (cond ((string-suffix-p "-mac" sys)
                   "MAC ")
                  ((string-suffix-p "-dos" sys)
                   "DOS ")
                  (t ""))
            (if (string-match-p "u\\(tf-8\\|ndecided\\)" sys)
                ""
              (concat (s-chop-suffixes '("-unix" "-dos" "-mac") sys) " ")))))

(defun *major-mode ()
  "The major mode, including process, environment and text-scale info."
  (propertize
   (concat " "
           (format-mode-line mode-name)
           (if (stringp mode-line-process) mode-line-process)
           (if doom-ml--env-version (concat " " doom-ml--env-version))
           (and (featurep 'face-remap)
                (/= text-scale-mode-amount 0)
                (format " (%+d)" text-scale-mode-amount))
           " ")
   'face (if active 'mode-line 'mode-line-inactive)))

(defun *vc ()
  "Displays the current branch, colored based on its state."
  (when vc-mode
    (let ((backend (substring vc-mode (+ 2 (length (symbol-name (vc-backend buffer-file-name))))))
          (face (let ((state (vc-state buffer-file-name)))
                  (cond ((memq state '(edited added))
                         'doom-modeline-info)
                        ((memq state '(removed needs-merge needs-update conflict unregistered))
                         'doom-modeline-urgent)
                        (t 'doom-modeline-warning)))))
      (concat
       " "
       (propertize (all-the-icons-octicon "git-branch")
                   'face `(:inherit ,(if active face 'mode-line-inactive)
                           :family ,(all-the-icons-octicon-family)
                           :height 1.25)
                   'display '(raise -0.1))
       " "
       (propertize backend 'face (if active face 'mode-line-inactive))
       " "))))

(defvar-local doom--flycheck-err-cache nil "")
(defvar-local doom--flycheck-cache nil "")
(defun *flycheck ()
  "Persistent and cached flycheck indicators in the mode-line."
  (when (and (featurep 'flycheck) flycheck-mode)
    (if (or flycheck-current-errors
            (eq 'running flycheck-last-status-change))
        (or (and (or (eq doom--flycheck-err-cache doom--flycheck-cache)
                     (memq flycheck-last-status-change '(running not-checked)))
                 doom--flycheck-cache)
            (and (setq doom--flycheck-err-cache flycheck-current-errors)
                 (setq doom--flycheck-cache
                       (let ((fw (doom-ml-flycheck-count 'warning))
                             (fe (doom-ml-flycheck-count 'error)))
                         (concat (if fe (concat
                                         (all-the-icons-octicon "x" :face 'doom-modeline-urgent :height 1.2 :v-adjust -0.06)
                                         (propertize (format " %d " fe) 'face 'doom-modeline-urgent)))
                                 (if fw (concat
                                         (all-the-icons-octicon "x" :face 'doom-modeline-warning :height 1.2 :v-adjust -0.06)
                                         (propertize (format " %d " fw) 'face 'doom-modeline-warning)))
                                 (unless (or fe fw)
                                   (when active
                                     (all-the-icons-octicon "check" :face 'doom-modeline-info :height 1.2 :v-adjust -0.06)))
                                 " ")))))
      (concat
       (all-the-icons-octicon "check" :face 'doom-modeline-info :height 1.2 :v-adjust -0.06)
       " "))))

(defun *selection-info ()
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and active (evil-visual-state-p))
    (propertize
     (let ((reg-beg (region-beginning))
           (reg-end (region-end))
           (evil (eq 'visual evil-state)))
       (let ((lines (count-lines reg-beg (min (1+ reg-end) (point-max))))
             (chars (- (1+ reg-end) reg-beg))
             (cols (1+ (abs (- (evil-column reg-end)
                               (evil-column reg-beg))))))
         (cond
          ;; rectangle selection
          ((or (bound-and-true-p rectangle-mark-mode)
               (and evil (eq 'block evil-visual-selection)))
           (format " %dx%dB " lines (if evil cols (1- cols))))
          ;; line selection
          ((or (> lines 1) (eq 'line evil-visual-selection))
           (if (and (eq evil-state 'visual) (eq evil-this-type 'line))
               (format " %dL " lines)
             (format " %dC %dL " chars lines)))
          (t (format " %dC " (if evil chars (1- chars)))))))
     'face 'doom-modeline-highlight)))

(defun *macro-recording ()
  "Display current macro being recorded."
  (when (and active defining-kbd-macro)
    (propertize
     (format " %s • " (char-to-string evil-this-macro))
     'face 'doom-modeline-highlight)))

(make-variable-buffer-local 'anzu--state)
(defun *anzu ()
  "Show the current match number and the total number of matches. Requires anzu
to be enabled."
  (when (and (featurep 'evil-anzu) (evil-ex-hl-active-p 'evil-ex-search))
    (propertize
     (format " %s/%d%s "
             anzu--current-position anzu--total-matched
             (if anzu--overflow-p "+" ""))
     'face (if active 'doom-modeline-count 'mode-line-inactive))))

(defun *evil-substitute ()
  "Show number of :s matches in real time."
  (when (and (evil-ex-p) (evil-ex-hl-active-p 'evil-ex-substitute))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches "
                   (count-matches pattern (car range) (cdr range))
                   evil-ex-argument)
         " ... "))
     'face (if active 'doom-modeline-count 'mode-line-inactive))))

(defun *iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and (boundp 'iedit-mode) iedit-mode)
    (propertize
     (let ((this-oc (let (message-log-max) (iedit-find-current-occurrence-overlay)))
           (length (or (ignore-errors (length iedit-occurrences-overlays)) 0)))
       (format
        " %s/%s "
        (save-excursion
          (unless this-oc
            (iedit-prev-occurrence)
            (setq this-oc (iedit-find-current-occurrence-overlay)))
          (if this-oc
              ;; NOTE: Not terribly reliable
              (- length (-elem-index this-oc iedit-occurrences-overlays))
            "-"))
        length))
     'face (if active 'doom-modeline-count 'mode-line-inactive))))

(defun *buffer-position ()
  "A more vim-like buffer position."
  (let ((start (window-start))
        (end (window-end))
        (pend (point-max)))
    (propertize
     (concat
      " %l:%c :"
      (if (and (= start 1)
               (= end pend))
          "All"
        (cond ((= start 1) "Top")
              ((= end pend) "Bot")
              (t (format "%d%%%%" (/ end 0.01 pend))))))
     'face (if active 'doom-modeline-alternate 'mode-line-inactive))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar doom-modeline-bar-active (face-background 'doom-modeline-bar)
  "The color to use for the bar in active window mode-lines.")

(defvar doom-modeline-bar-inactive (face-background 'mode-line-inactive)
  "The color to use for the bar in inactive window mode-lines.")

(defun doom-modeline (&optional id)
  `(:eval
    (let* ((active (eq (selected-window) doom-ml-selected-window))
           (lhs (list (propertize
                       " " 'display (doom-make-xpm (if active
                                                       doom-modeline-bar-active
                                                     doom-modeline-bar-inactive)
                                                   doom-modeline-height
                                                   doom-modeline-bar-width))
                      (*macro-recording)
                      (*selection-info)
                      (*anzu)
                      (*evil-substitute)
                      (*iedit)
                      ,(if (eq id 'scratch)
                           '(*buffer-project)
                         '(*buffer-info))
                      " "
                      (*flycheck)))
           (rhs (list (*buffer-encoding-abbrev)
                      (*vc)
                      (*major-mode)
                      (*buffer-position)))
           (middle (propertize
                    " " 'display `((space :align-to (- (+ right right-fringe right-margin)
                                                       ,(1+ (string-width (format-mode-line rhs)))))))))
      (list lhs middle rhs))))

(setq-default mode-line-format (doom-modeline))


;;
;; Eldoc-in-mode-line support (for `eval-expression')
;;

(defvar doom-modeline-eldoc-bar-color (face-background 'doom-modeline-eldoc-bar)
  "The color to use for the bar when eldoc uses the mode-line.")

(defun doom-eldoc-modeline ()
  `(:eval
    (let ((active (eq (selected-window) doom-ml-selected-window)))
      (list (list (propertize " " 'display (doom-make-xpm doom-modeline-eldoc-bar-color
                                                          doom-modeline-height
                                                          doom-modeline-bar-width))
                  (and (bound-and-true-p str) str))
            (propertize " " 'display `((space :align-to (1- (+ right right-fringe right-margin)))))))))

(defun doom-eldoc-show-in-mode-line (input)
  "Display string STR in the mode-line next to minibuffer."
  (with-current-buffer (eldoc-current-buffer)
    (let* ((max              (window-width (selected-window)))
           (str              (and (stringp input) (concat " " input)))
           (len              (length str))
           (tmp-str          str)
           (mode-line-format (or (and str (doom-eldoc-modeline))
                                 mode-line-format))
           roll mode-line-in-non-selected-windows)
      (catch 'break
        (if (and (> len max) eldoc-mode-line-rolling-flag)
            (progn
              (while (setq roll (sit-for 0.3))
                (setq tmp-str (substring tmp-str 2)
                      mode-line-format (concat tmp-str " [<]" str))
                (force-mode-line-update)
                (when (< (length tmp-str) 2) (setq tmp-str str)))
              (unless roll
                (when eldoc-mode-line-stop-rolling-on-input
                  (setq eldoc-mode-line-rolling-flag nil))
                (throw 'break nil)))
          (force-mode-line-update)
          (sit-for eldoc-show-in-mode-line-delay))))
    (force-mode-line-update)))

(provide 'core-modeline)
;;; core-modeline.el ends here
