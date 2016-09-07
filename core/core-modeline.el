;;; core-modeline.el

;; This file tries to be an almost self-contained configuration of my mode-line.
;;
;; It depends on the following external packages:
;;   + REQUIRED
;;       + powerline
;;       + evil-mode
;;       + projectile
;;   + OPTIONAL
;;       + anzu
;;       + iedit and evil-multiedit
;;       + flycheck
;;
;; The only external functions used are:
;;  `doom-fix-unicode'  in core/core-defuns.el
;;  `doom/project-root' in core/defuns/defuns-projectile.el
;;
;; Both are simple, isolated functions and, besides projectile, has no other
;; dependencies.

(defvar mode-line-height 30
  "How tall the mode-line should be. This is only respected in GUI emacs.")

;; Load powerline only when uncompiled, in order to generate the xpm bitmaps for
;; the mode-line. This is the tall blue bar on the left of the mode-line.
;; NOTE Compile this file for a faster startup!
(eval-when-compile (require 'powerline))
;; FIXME Don't hardcode colors in
(defvar mode-line-bar          (! (pl/percent-xpm mode-line-height 100 0 100 0 3 "#00B3EF" nil)))
(defvar mode-line-eldoc-bar    (! (pl/percent-xpm mode-line-height 100 0 100 0 3 "#B3EF00" nil)))
(defvar mode-line-inactive-bar (! (pl/percent-xpm mode-line-height 100 0 100 0 3 nil nil)))

;; Custom faces
(defface mode-line-is-modified nil
  "Face for mode-line modified symbol")

(defface mode-line-2 nil
  "The alternate color for mode-line text.")

(defface mode-line-highlight nil
  "Face for bright segments of the mode-line.")

(defface mode-line-count-face nil
  "Face for anzu/evil-substitute/evil-search number-of-matches display.")

;; Git/VCS segment faces
(defface mode-line-vcs-info '((t (:inherit warning)))
  "")
(defface mode-line-vcs-warning '((t (:inherit warning)))
  "")

;; Flycheck segment faces
(defface doom-flycheck-error '((t (:inherit error)))
  "Face for flycheck error feedback in the modeline.")
(defface doom-flycheck-warning '((t (:inherit warning)))
  "Face for flycheck warning feedback in the modeline.")


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
    (let ((default-directory (doom/project-root)))
      (let ((s (shell-command-to-string doom-ml--env-command)))
        (setq doom-ml--env-version (if (string-match "[ \t\n\r]+\\'" s)
                                    (replace-match "" t t s)
                                  s))
        (run-hook-with-args 'doom-ml-env-version-hook doom-ml--env-version)))))

(defmacro def-version-cmd! (modes command)
  "Define a COMMAND for MODE that will set `doom-ml--env-command' when that mode is
activated, which should return the version number of the current environment. It is used
by `doom-ml|env-update' to display a version number in the modeline. For instance:

  (def-version-cmd! ruby-mode \"ruby --version | cut -d' ' -f2\")

This will display the ruby version in the modeline in ruby-mode buffers. It is cached the
first time."
  (add-hook! (focus-in find-file) 'doom-ml|env-update)
  `(add-hook! ,modes (setq doom-ml--env-command ,command)))


;;
;; Initialization
;;

;; Where (py|rb)env version strings will be stored
(defvar-local doom-ml--env-version nil)
(defvar-local doom-ml--env-command nil)

;; Make certain unicode glyphs bigger for the mode-line.
;; FIXME Replace with all-the-icons?
(doom-fix-unicode '("DejaVu Sans Mono" 15) ?✱) ;; modified symbol
(let ((font "DejaVu Sans Mono for Powerline"))
  (doom-fix-unicode (list font 12) ?)  ;; git symbol
  (doom-fix-unicode (list font 16) ?∄)  ;; non-existent-file symbol
  (doom-fix-unicode (list font 15) ?)) ;; read-only symbol

;; So the mode-line can keep track of "the current window"
(defvar mode-line-selected-window nil)
(defun doom|set-selected-window (&rest _)
  (let ((window (frame-selected-window)))
    (unless (minibuffer-window-active-p window)
      (setq mode-line-selected-window window))))
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
    (propertize
     (f-dirname
      (let ((buffer-path (file-relative-name buffer-file-name (doom/project-root)))
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
                  buffer-path))))
     'face (if active 'mode-line-2))))

(defun *buffer-name ()
  "The buffer's base name or id."
  ;; FIXME Don't show uniquify tags
  (s-trim-left (format-mode-line "%b")))

(defun *buffer-pwd ()
  "Displays `default-directory', for special buffers like the scratch buffer."
  (propertize
   (concat "[" (abbreviate-file-name default-directory) "]")
   'face 'mode-line-2))

(defun *buffer-state ()
  "Displays symbols representing the buffer's state
(non-existent/modified/read-only)"
  (when buffer-file-name
    (propertize
     (concat (if (not (file-exists-p buffer-file-name))
                 "∄"
               (if (buffer-modified-p) "✱"))
             (if buffer-read-only ""))
     'face 'mode-line-is-modified)))

(defun *buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (if (memq buffer-file-coding-system '(utf-8 utf-8-unix))
      ""
    (symbol-name buffer-file-coding-system)))

(defun *major-mode ()
  "The major mode, including process, environment and text-scale info."
  (concat (format-mode-line mode-name)
          (if (stringp mode-line-process) mode-line-process)
          (if doom-ml--env-version (concat " " doom-ml--env-version))
          (and (featurep 'face-remap)
               (/= text-scale-mode-amount 0)
               (format " (%+d)" text-scale-mode-amount))))

(defun *vc ()
  "Displays the current branch, colored based on its state."
  (when vc-mode
    (let ((backend (concat " " (substring vc-mode (+ 2 (length (symbol-name (vc-backend buffer-file-name)))))))
          (face (let ((state (vc-state buffer-file-name)))
                  (cond ((memq state '(edited added))
                         'mode-line-vcs-info)
                        ((memq state '(removed needs-merge needs-update conflict removed unregistered))
                         'mode-line-vcs-warning)))))
      (if active
          (propertize backend 'face face)
        backend))))

(defvar-local doom--flycheck-err-cache nil "")
(defvar-local doom--flycheck-cache nil "")
(defun *flycheck ()
  "Persistent and cached flycheck indicators in the mode-line."
  (when (and (featurep 'flycheck)
             flycheck-mode
             (or flycheck-current-errors
                 (eq 'running flycheck-last-status-change)))
    (or (and (or (eq doom--flycheck-err-cache doom--flycheck-cache)
                 (memq flycheck-last-status-change '(running not-checked)))
             doom--flycheck-cache)
        (and (setq doom--flycheck-err-cache flycheck-current-errors)
             (setq doom--flycheck-cache
                   (let ((fe (doom-ml-flycheck-count 'error))
                         (fw (doom-ml-flycheck-count 'warning)))
                     (concat
                      (if fe (propertize (format " •%d " fe)
                                         'face (if active
                                                   'doom-flycheck-error
                                                 'mode-line)))
                      (if fw (propertize (format " •%d " fw)
                                         'face (if active
                                                   'doom-flycheck-warning
                                                 'mode-line))))))))))

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
     'face 'mode-line-highlight)))

(defun *macro-recording ()
  "Display current macro being recorded."
  (when (and active defining-kbd-macro)
    (propertize
     (format " %s ▶ " (char-to-string evil-this-macro))
     'face 'mode-line-highlight)))

(make-variable-buffer-local 'anzu--state)
(defun *anzu ()
  "Show the current match number and the total number of matches. Requires anzu
to be enabled."
  (when (and (featurep 'evil-anzu) (evil-ex-hl-active-p 'evil-ex-search))
    (propertize
     (format " %s/%d%s "
             anzu--current-position anzu--total-matched
             (if anzu--overflow-p "+" ""))
     'face (if active 'mode-line-count-face))))

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
     'face (if active 'mode-line-count-face))))

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
     'face (if active 'mode-line-count-face))))

(defun *buffer-position ()
  "A more vim-like buffer position."
  (let ((start (window-start))
        (end (window-end))
        (pend (point-max)))
    (if (and (= start 1)
             (= end pend))
        ":All"
      (cond ((= start 1) ":Top")
            ((= end pend) ":Bot")
            (t (format ":%d%%%%" (/ end 0.01 pend)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun doom-mode-line (&optional id)
  `(:eval
    (let* ((active (eq (selected-window) mode-line-selected-window))
           (lhs (list (propertize " " 'display (if active mode-line-bar mode-line-inactive-bar))
                      (*flycheck)
                      (*macro-recording)
                      (*selection-info)
                      (*anzu)
                      (*evil-substitute)
                      (*iedit)
                      " "
                      (*buffer-path)
                      (*buffer-name)
                      " "
                      (*buffer-state)
                      ,(if (eq id 'scratch) '(*buffer-pwd))))
           (rhs (list (*buffer-encoding-abbrev)
                      (*vc)
                      "  " (*major-mode) "  "
                      (propertize
                       (concat "(%l,%c) " (*buffer-position))
                       'face (if active 'mode-line-2))))
           (middle (propertize
                    " " 'display `((space :align-to (- (+ right right-fringe right-margin)
                                                       ,(1+ (string-width (format-mode-line rhs)))))))))
      (list lhs middle rhs))))

(setq-default mode-line-format (doom-mode-line))

(provide 'core-modeline)
;;; core-modeline.el ends here
