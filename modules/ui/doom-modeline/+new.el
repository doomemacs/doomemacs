;;; ui/doom-modeline/config.el -*- lexical-binding: t; -*-
;;;###if (featurep! +new)

;; This mode-line is experimental, may have bugs and is likely to change. It
;; also doesn't have all the features of the old modeline (yet).
;;
;; However, it is at least twice as fast as the original modeline, and a little
;; more flexible, what with `mode-line-format-left' and
;; `mode-line-format-right'. It also exposes a more powerful API for defining
;; modelines and modeline segments that make use of variable watchers and hooks
;; to update them.

;;;; Benchmarks
;; (benchmark-run 1000 (format-mode-line mode-line-format))
;; Old system: ~0.198
;; New system: ~0.056

(defvar +doom-modeline-height 25
  "How tall the mode-line should be (only respected in GUI emacs).")

(defvar +doom-modeline-buffer-name-function
  #'doom-modeline--file-path
  "TODO")

(defvar anzu--state nil)
(defvar evil-mode nil)
(defvar evil-state nil)
(defvar evil-visual-selection nil)
(defvar iedit-mode nil)
(defvar all-the-icons-scale-factor)
(defvar all-the-icons-default-adjust)

(defvar-local mode-line-format-left  ())
(defvar-local mode-line-format-right ())
(put 'mode-line-format-left  'risky-local-variable t)
(put 'mode-line-format-right 'risky-local-variable t)


;;
;; Plugins
;;

(def-package! anzu
  :after-call isearch-mode
  :config
  (setq anzu-cons-mode-line-p nil
        anzu-minimum-input-length 1
        anzu-search-threshold 250)
  (global-anzu-mode +1)

  (defun +doom-modeline*fix-anzu-count (positions here)
    (cl-loop for (start . end) in positions
             collect t into before
             when (and (>= here start) (<= here end))
             return (length before)
             finally return 0))
  (advice-add #'anzu--where-is-here :override #'+doom-modeline*fix-anzu-count)

  ;; Avoid anzu conflicts across buffers
  (mapc #'make-variable-buffer-local
        '(anzu--total-matched anzu--current-position anzu--state
          anzu--cached-count anzu--cached-positions anzu--last-command
          anzu--last-isearch-string anzu--overflow-p))
  ;; Ensure anzu state is cleared when searches & iedit are done
  (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
  (add-hook 'doom-escape-hook #'anzu--reset-status t)
  (add-hook 'iedit-mode-end-hook #'anzu--reset-status))


(def-package! evil-anzu
  :when (featurep! :feature evil)
  :after-call (evil-ex-start-search evil-ex-start-word-search))


;;
;; Helpers
;;

;; Keep `+doom-modeline-current-window' up-to-date
(defvar +doom-modeline-current-window (frame-selected-window))
(defun +doom-modeline|set-selected-window (&rest _)
  "Sets `+doom-modeline-current-window' appropriately"
  (when-let* ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq +doom-modeline-current-window win)
      (force-mode-line-update))))

(defun +doom-modeline|unset-selected-window ()
  (setq +doom-modeline-current-window nil)
  (force-mode-line-update))

(add-hook 'window-configuration-change-hook #'+doom-modeline|set-selected-window)
(add-hook 'doom-enter-window-hook #'+doom-modeline|set-selected-window)
(if (not (boundp 'after-focus-change-function))
    (progn
      (add-hook 'focus-in-hook  #'+doom-modeline|set-selected-window)
      (add-hook 'focus-out-hook #'+doom-modeline|unset-selected-window))
  (defun +doom-modeline|refresh-frame ()
    (setq +doom-modeline-current-window nil)
    (cl-loop for frame in (frame-list)
             if (eq (frame-focus-state frame) t)
             return (setq +doom-modeline-current-window (frame-selected-window frame)))
    (force-mode-line-update t))
  (add-function :after after-focus-change-function #'+doom-modeline|refresh-frame))

(defsubst active ()
  (eq (selected-window) +doom-modeline-current-window))

;; xpm generator
(defun doom-modeline--make-xpm (width height &optional color)
  "Create an XPM bitmap. Inspired by `powerline''s `pl/make-xpm'."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (ignore-errors
       (create-image
        (concat
         (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
                 (length (car data))
                 (length data)
                 color
                 color)
         (apply #'concat
                (cl-loop with idx = 0
                         with len = (length data)
                         for dl in data
                         do (cl-incf idx)
                         collect
                         (concat "\""
                                 (cl-loop for d in dl
                                          if (= d 0) collect (string-to-char " ")
                                          else collect (string-to-char "."))
                                 (if (eq idx len) "\"};" "\",\n")))))
        'xpm t :ascent 'center)))))

(defun doom-modeline--file-path (&optional path)
  (let ((buffer-file-name (or path buffer-file-name))
        (root (doom-project-root))
        (active (active)))
    (cond ((null root)
           (propertize "%b" 'face (if active 'doom-modeline-buffer-file)))
          ((or (null buffer-file-name)
               (directory-name-p buffer-file-name))
           (propertize (abbreviate-file-name (or buffer-file-name default-directory))
                       'face (if active 'doom-modeline-buffer-path)))
          ((let* ((modified-faces (if (buffer-modified-p) 'doom-modeline-buffer-modified))
                  (true-filename (file-truename buffer-file-name))
                  (relative-dirs (file-relative-name (file-name-directory true-filename)
                                                     (concat root "../")))
                  (relative-faces (or modified-faces (if active 'doom-modeline-buffer-path)))
                  (file-faces (or modified-faces (if active 'doom-modeline-buffer-file))))
             (if (equal "./" relative-dirs) (setq relative-dirs ""))
             (concat (propertize relative-dirs 'face (if relative-faces `(:inherit ,relative-faces)))
                     (propertize (file-name-nondirectory true-filename)
                                 'face (if file-faces `(:inherit ,file-faces)))))))))


;;
;; Macros
;;

(cl-defmacro def-modeline-segment! (name &rest rest &key init faces hooks vars &allow-other-keys)
  "TODO"
  (let ((body rest))
    (while (keywordp (car body))
      (setq body (cddr body)))
    (setq rest body))
  (unless EMACS26+
    (setq vars nil))
  (let ((docstring (if (stringp (car rest)) (pop rest)))
        (realvar (if (and rest faces) (intern (format "doom-modeline--var-%s" name)) name)))
    (macroexp-progn
     (append (when rest
               (if (or hooks vars)
                   (let ((setterfn    (intern (format "doom-modeline--set-%s" name)))
                         (varsetterfn (intern (format "doom-modeline--setvar-%s" name))))
                     (append `((fset ',setterfn
                                     (lambda (&rest _)
                                       (when (or (memq ',name mode-line-format-left)
                                                 (memq ',name mode-line-format-right))
                                         (setq-local ,realvar (progn ,@rest))))))
                             (mapcar (lambda (hook) `(add-hook ',hook #',setterfn))
                                     hooks)
                             (when vars
                               `((fset ',varsetterfn
                                       (lambda (sym val op where)
                                         (and (eq op 'set) where
                                              (with-current-buffer where
                                                (set sym val)
                                                (,setterfn)))))
                                 ,@(mapcar (lambda (var) `(add-variable-watcher ',var #',varsetterfn))
                                           vars)))))
                 (setq init `(quote (:eval ,@rest)))
                 nil))
             (if (eq realvar name)
                 `((defvar-local ,name ,init ,docstring))
               `((defvar-local ,realvar nil)
                 (defvar-local ,name
                   '(:eval (cond ((active) ,realvar) (,realvar (substring-no-properties ,realvar))))
                   ,docstring)))
             `((put ',name 'risky-local-variable t))))))

;;
(defvar doom-mode-line-alist nil)

(defun def-modeline! (name left &optional right)
  (setf (alist-get name doom-mode-line-alist) (list left right)))

(defun set-modeline! (name &optional default)
  (let ((modeline (cdr (assq name doom-mode-line-alist))))
    (when modeline
      (if default
          (setq-default mode-line-format-left  `("" ,@(car  modeline))
                        mode-line-format-right `("" ,@(cadr modeline)))
        (setq mode-line-format-left  `("" ,@(car  modeline))
              mode-line-format-right `("" ,@(cadr modeline))))
      (force-mode-line-update))))


;;
;; Bars
;;

(defvar mode-line-bar-active nil "TODO")
(defvar mode-line-bar-inactive nil "TODO")
(defun doom-modeline|setup-bars ()
  (setq mode-line-bar-active
        (doom-modeline--make-xpm 3 +doom-modeline-height (face-background 'doom-modeline-bar))
        mode-line-bar-inactive
        (doom-modeline--make-xpm 3 +doom-modeline-height)))
(add-hook 'doom-load-modeline-hook #'doom-modeline|setup-bars)

(defun doom-modeline|setup-bars-after-change (_sym val op _where)
  (when (eq op 'set)
    (let ((+doom-modeline-height val))
      (doom-modeline|setup-bars))))
(add-variable-watcher '+doom-modeline-height #'doom-modeline|setup-bars-after-change)

(def-modeline-segment! +mode-line-bar
  (if (active) mode-line-bar-active mode-line-bar-inactive))


;;
;; Segments
;;

(defun +doom-modeline|update-on-change ()
  (doom-modeline--set-+mode-line-buffer-id)
  (remove-hook 'post-command-hook #'+doom-modeline|update-on-change t))
(defun +doom-modeline|start-update-on-change ()
  (add-hook 'post-command-hook #'+doom-modeline|update-on-change nil t))
(add-hook 'first-change-hook #'+doom-modeline|start-update-on-change)

(advice-add #'undo :after #'doom-modeline--set-+mode-line-buffer-id)
(advice-add #'undo-tree-undo :after #'doom-modeline--set-+mode-line-buffer-id)

(def-modeline-segment! +mode-line-buffer-id
  :hooks (find-file-hook after-change-functions read-only-mode-hook after-save-hook after-revert-hook)
  :faces t
  (concat (cond (buffer-read-only
                 (concat (all-the-icons-octicon
                          "lock"
                          :face 'doom-modeline-warning
                          :v-adjust -0.05)
                         " "))
                ((buffer-modified-p)
                 (concat (all-the-icons-faicon
                          "floppy-o"
                          :face 'doom-modeline-buffer-modified
                          :v-adjust -0.0575)
                         " "))
                ((and buffer-file-name (not (file-exists-p buffer-file-name)))
                 (concat (all-the-icons-octicon
                          "circle-slash"
                          :face 'doom-modeline-urgent
                          :v-adjust -0.05)
                         " ")))
          (if buffer-file-name
              (funcall +doom-modeline-buffer-name-function buffer-file-name)
            "%b")))

(def-modeline-segment! +mode-line-buffer-directory
  (let ((face (if (active) 'doom-modeline-buffer-path)))
    (concat (if (display-graphic-p) " ")
            (all-the-icons-octicon
             "file-directory"
             :face face
             :v-adjust -0.05
             :height 1.25)
            (propertize (concat " " (abbreviate-file-name default-directory))
                        'face face))))

(def-modeline-segment! +mode-line-vcs
  :vars (vc-mode)
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (state   (vc-state buffer-file-name backend)))
      (let ((face    'mode-line-inactive)
            (active  (active))
            (all-the-icons-default-adjust -0.1))
        (concat "  "
                (cond ((memq state '(edited added))
                       (if active (setq face 'doom-modeline-info))
                       (all-the-icons-octicon
                        "git-compare"
                        :face face
                        :v-adjust -0.05))
                      ((eq state 'needs-merge)
                       (if active (setq face 'doom-modeline-info))
                       (all-the-icons-octicon "git-merge" :face face))
                      ((eq state 'needs-update)
                       (if active (setq face 'doom-modeline-warning))
                       (all-the-icons-octicon "arrow-down" :face face))
                      ((memq state '(removed conflict unregistered))
                       (if active (setq face 'doom-modeline-urgent))
                       (all-the-icons-octicon "alert" :face face))
                      (t
                       (if active (setq face 'font-lock-doc-face))
                       (all-the-icons-octicon
                        "git-compare"
                        :face face
                        :v-adjust -0.05)))
                " "
                (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                            'face (if active face))
                " ")))))

(def-modeline-segment! +mode-line-encoding
  :hooks (after-save-hook find-file-hook)
  :vars (buffer-file-coding-system)
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF  ")
            (1 "CRLF  ")
            (2 "CR  "))
          (let ((sys (coding-system-plist buffer-file-coding-system)))
            (if (memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                "UTF-8"
              (upcase (symbol-name (plist-get sys :name)))))
          "  "))

(def-modeline-segment! +mode-line-major-mode
  :vars (mode-name)
  :faces t
  (propertize (format-mode-line mode-name) 'face 'font-lock-keyword-face))


(defun +doom-modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (active) (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face 'doom-modeline-panel)))
      (concat sep
              (propertize (if (bound-and-true-p evil-this-macro)
                              (char-to-string evil-this-macro)
                            "Macro")
                          'face 'doom-modeline-panel)
              sep
              (all-the-icons-octicon "triangle-right"
                                     :face 'doom-modeline-panel
                                     :v-adjust -0.05)
              sep))))

(defsubst +doom-modeline--anzu ()
  "Show the match index and total number thereof. Requires `anzu', also
`evil-anzu' if using `evil-mode' for compatibility with `evil-search'."
  (when (and anzu--state (not iedit-mode))
    (propertize
     (let ((here anzu--current-position)
           (total anzu--total-matched))
       (cond ((eq anzu--state 'replace-query)
              (format " %d replace " total))
             ((eq anzu--state 'replace)
              (format " %d/%d " here total))
             (anzu--overflow-p
              (format " %s+ " total))
             (t
              (format " %s/%d " here total))))
     'face (if (active) 'doom-modeline-panel))))

(defsubst +doom-modeline--evil-substitute ()
  "Show number of matches for evil-ex substitutions and highlights in real time."
  (when (and evil-mode
             (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                 (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                 (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches " (how-many pattern (car range) (cdr range)))
         " - "))
     'face (if (active) 'doom-modeline-panel))))

(defun doom-themes--overlay-sort (a b)
  (< (overlay-start a) (overlay-start b)))

(defsubst +doom-modeline--iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and iedit-mode iedit-occurrences-overlays)
    (propertize
     (let ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (progn (iedit-prev-occurrence)
                               (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format " %s/%d "
               (if this-oc
                   (- length
                      (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                  #'doom-themes--overlay-sort)))
                      -1)
                 "-")
               length))
     'face (if (active) 'doom-modeline-panel))))

(def-modeline-segment! +mode-line-matches
  "Displays: 1. the currently recording macro, 2. A current/total for the
current search term (with anzu), 3. The number of substitutions being conducted
with `evil-ex-substitute', and/or 4. The number of active `iedit' regions."
  (let ((meta (concat (+doom-modeline--macro-recording)
                      (+doom-modeline--anzu)
                      (+doom-modeline--evil-substitute)
                      (+doom-modeline--iedit)
                      " ")))
     (or (and (not (equal meta " ")) meta)
         (if buffer-file-name " %I "))))

;;
(defsubst doom-column (pos)
  (save-excursion (goto-char pos)
                  (current-column)))

(defvar-local +doom-modeline-enable-word-count nil
  "If non-nil, a word count will be added to the selection-info modeline
segment.")

(defun +doom-modeline|enable-word-count () (setq +doom-modeline-enable-word-count t))
(add-hook 'text-mode-hook #'+doom-modeline|enable-word-count)

(def-modeline-segment! +mode-line-selection-info
  (when mark-active
    (cl-destructuring-bind (beg . end)
        (if (eq evil-state 'visual)
            (cons evil-visual-beginning evil-visual-end)
          (cons (region-beginning) (region-end)))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (concat (cond ((or (bound-and-true-p rectangle-mark-mode)
                            (eq 'block evil-visual-selection))
                        (let ((cols (abs (- (doom-column end)
                                            (doom-column beg)))))
                          (format "%dx%dB" lines cols)))
                       ((eq evil-visual-selection 'line)
                        (format "%dL" lines))
                       ((> lines 1)
                        (format "%dC %dL" (- end beg) lines))
                       ((format "%dC" (- end beg))))
                 (when +doom-modeline-enable-word-count
                   (format " %dW" (count-words beg end)))))
       'face 'doom-modeline-highlight))))

(def-modeline! :main
  '(+mode-line-bar +mode-line-matches " " +mode-line-buffer-id "  %2l:%c %p  " +mode-line-selection-info)
  '(+mode-line-encoding +mode-line-major-mode +mode-line-vcs))

(def-modeline! :project
  '(+mode-line-bar +mode-line-buffer-directory)
  '(+mode-line-major-mode))


;;
;;
;;

(def-modeline-segment! mode-line-rest
  (let ((rhs-str (format-mode-line mode-line-format-right)))
    (list (propertize
           " " 'display
           `((space :align-to (- (+ right right-fringe right-margin)
                                 ,(1+ (string-width rhs-str))))))
          rhs-str)))

(setq-default mode-line-format '("" mode-line-format-left mode-line-rest))


;;
(set-modeline! :main t)

(add-hook! '+doom-dashboard-mode-hook (set-modeline! :project))


;;
(defun doom-modeline-init () (run-hooks 'doom-load-modeline-hook))
(add-hook 'doom-load-theme-hook #'doom-modeline-init)

;; Ensure modeline is inactive when Emacs is unfocused (and active otherwise)
(defvar +doom-modeline-remap-face-cookie nil)
(defun +doom-modeline|focus ()
  (when +doom-modeline-remap-face-cookie
    (require 'face-remap)
    (face-remap-remove-relative +doom-modeline-remap-face-cookie)))
(defun +doom-modeline|unfocus ()
  (setq +doom-modeline-remap-face-cookie (face-remap-add-relative 'mode-line 'mode-line-inactive)))
(add-hook 'focus-in-hook #'+doom-modeline|focus)
(add-hook 'focus-out-hook #'+doom-modeline|unfocus)
