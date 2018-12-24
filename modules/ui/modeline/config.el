;;; ui/modeline/config.el -*- lexical-binding: t; -*-

;; This mode-line is experimental, Emacs 26+ only, may have buggy and is likely
;; to change. It also isn't feature complete, compared to :ui doom-modeline, but
;; it will eventually replace it.
;;
;; However, it is at least ten times faster than the original modeline, and more
;; flexible, what with `+modeline-format-left', `+modeline-format-right', and a
;; more powerful API for defining modelines and modeline segments.

;;;; Benchmarks
;; (benchmark-run 1000 (format-mode-line mode-line-format))
;; Old system: ~0.563 - 0.604
;; New system: ~0.036 - 0.061

(defvar +modeline-width 3
  "How wide the mode-line bar should be (only respected in GUI emacs).")

(defvar +modeline-height 25
  "How tall the mode-line should be (only respected in GUI emacs).")

(defvar +modeline-bar-at-end nil
  "If non-nil, the bar is placed at the end, instead of at the beginning of the
modeline.")

(defvar +modeline-bar-invisible nil
  "If non-nil, the bar is transparent, and only used to police the height of the
mode-line.")

(defvar +modeline-buffer-path-function #'+modeline-file-path-with-project
  "A function that returns, in list form, components of the buffer file name
display in the mode-line.

Each item should either be a string or a a cons cell whose CAR is the path
component and CDR is the name of a face.

Currently available functions:

+ `+modeline-file-path-with-project': project/src/lib/file.c
+ `+modeline-file-path-from-project': src/lib/file.c
+ `+modeline-file-path-truncated-with-project': project/s/l/file.c
+ `+modeline-file-path-truncated-upto-project': ~/w/project/src/lib/file.c
+ `+modeline-file-path-truncated-upto-project-root': ~/w/p/s/lib/file.c
+ `+modeline-file-path-truncated': ~/w/p/s/l/file.c
+ `+modeline-file-name': file.c")

;; Convenience aliases
(defvaralias 'mode-line-format-left '+modeline-format-left)
(defvaralias 'mode-line-format-right '+modeline-format-right)
;;
(defvar-local +modeline-format-left  () "TODO")
(defvar-local +modeline-format-right () "TODO")
(put '+modeline-format-left  'risky-local-variable t)
(put '+modeline-format-right 'risky-local-variable t)

;; Otherwise appended segments will produce *Invalid*
(setq global-mode-string '(""))
;; We handle this ourselves
(setq projectile-dynamic-mode-line nil)

;;
(defvar +modeline--vspc (propertize " " 'face 'variable-pitch))

;; externs
(defvar anzu--state nil)
(defvar evil-mode nil)
(defvar evil-state nil)
(defvar evil-visual-selection nil)
(defvar evil-visual-beginning nil)
(defvar evil-visual-end nil)
(defvar iedit-mode nil)
(defvar all-the-icons-scale-factor)
(defvar all-the-icons-default-adjust)


;;
;; Custom faces

(defgroup +modeline nil
  "TODO"
  :group 'faces)

(defface doom-modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group '+modeline)

(defface doom-modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path."
  :group '+modeline)

(defface doom-modeline-buffer-project-root
  '((t (:inherit doom-modeline-buffer-path)))
  "Face used for the project root at the beginning of the mode-line path."
  :group '+modeline)

(defface doom-modeline-buffer-modified '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group '+modeline)

(defface doom-modeline-buffer-major-mode '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group '+modeline)

(defface doom-modeline-highlight '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line."
  :group '+modeline)

(defface doom-modeline-panel '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `+modeline--anzu',
`+modeline--evil-substitute' and `iedit'"
  :group '+modeline)

(defface doom-modeline-info `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*vc'."
  :group '+modeline)

(defface doom-modeline-warning `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'"
  :group '+modeline)

(defface doom-modeline-urgent `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*flycheck'"
  :group '+modeline)

(defface doom-modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window."
  :group '+modeline)


;;
;; Packages

(def-package! anzu
  :after-call isearch-mode
  :config
  (setq anzu-cons-mode-line-p nil
        anzu-minimum-input-length 1
        anzu-search-threshold 250)
  (global-anzu-mode +1)

  (defun +modeline*fix-anzu-count (positions here)
    (cl-loop for (start . end) in positions
             collect t into before
             when (and (>= here start) (<= here end))
             return (length before)
             finally return 0))
  (advice-add #'anzu--where-is-here :override #'+modeline*fix-anzu-count)

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
;; Hacks

;; Keep `+modeline-current-window' up-to-date
(defvar +modeline-current-window (frame-selected-window))

(defun +modeline|set-selected-window (&rest _)
  "Sets `+modeline-current-window' appropriately"
  (when-let* ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq +modeline-current-window win)
      (force-mode-line-update))))

(defun +modeline|unset-selected-window ()
  (setq +modeline-current-window nil)
  (force-mode-line-update))

(add-hook 'window-configuration-change-hook #'+modeline|set-selected-window)
(add-hook 'doom-enter-window-hook #'+modeline|set-selected-window)
(if (not (boundp 'after-focus-change-function))
    (progn
      (add-hook 'focus-in-hook  #'+modeline|set-selected-window)
      (add-hook 'focus-out-hook #'+modeline|unset-selected-window))
  (defun +modeline|refresh-frame ()
    (setq +modeline-current-window nil)
    (cl-loop for frame in (frame-list)
             if (eq (frame-focus-state frame) t)
             return (setq +modeline-current-window (frame-selected-window frame)))
    (force-mode-line-update t))
  (add-function :after after-focus-change-function #'+modeline|refresh-frame))

(defsubst active ()
  (eq (selected-window) +modeline-current-window))

;; Ensure modeline is inactive when Emacs is unfocused (and active otherwise)
(defvar +modeline-remap-face-cookies nil)

(defun +modeline|focus-all-windows (&rest _)
  (cl-loop for (buffer . cookie) in +modeline-remap-face-cookies
           if (buffer-live-p buffer)
           do (with-current-buffer buffer
                (face-remap-remove-relative cookie))))

(defun +modeline|unfocus-all-windows (&rest _)
  (setq +modeline-remap-face-cookies
        (cl-loop for window in (window-list)
                 for buffer = (window-buffer window)
                 if (buffer-live-p buffer)
                 collect
                 (with-current-buffer buffer
                   (cons buffer
                         (face-remap-add-relative 'mode-line
                                                  'mode-line-inactive))))))

(add-hook 'focus-in-hook #'+modeline|focus-all-windows)
(add-hook 'focus-out-hook #'+modeline|unfocus-all-windows)
(advice-add #'posframe-hide :after #'+modeline|focus-all-windows)
(advice-add #'posframe-delete :after #'+modeline|focus-all-windows)
(when (featurep! :completion helm)
  (add-hook 'helm-before-initialize-hook #'+modeline|unfocus-all-windows)
  (add-hook 'helm-cleanup-hook #'+modeline|focus-all-windows))


;;
;; Helpers

(defun +modeline--make-xpm (width height &optional color)
  "Create an XPM bitmap. Inspired by `powerline''s `pl/make-xpm'."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (ignore-errors
       (create-image
        (concat
         (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
                 (length (car data)) (length data) color color)
         (cl-loop with idx = 0
                  with len = (length data)
                  for dl in data
                  do (cl-incf idx)
                  concat "\""
                  concat (cl-loop for d in dl
                                  if (= d 0) collect (string-to-char " ")
                                  else collect (string-to-char "."))
                  concat (if (eq idx len) "\"};" "\",\n")))
        'xpm t :ascent 'center)))))

(defun +modeline-build-path (path)
  "Construct the file path for the `+modeline-buffer-id' segment using
`+mdoeline-buffer-path-function'. If the buffer has no `buffer-file-name', just
use `buffer-name'."
  (let ((buffer-file-name (or path buffer-file-name)))
    (if (or (eq major-mode 'dired-mode)
            (null buffer-file-name))
        (propertize "%s" 'face 'doom-modeline-buffer-path)
      (cl-loop for spec in (funcall +modeline-buffer-path-function)
               if (stringp spec) concat spec
               else if (not (null spec))
               concat (propertize (car spec) 'face (cdr spec))))))


;;
;; Buffer file path styles

(defun +modeline-file-path-with-project ()
  "Returns the unaltered buffer file path relative to the project root's
parent.

e.g. project/src/lib/file.c"
  (let* ((base (buffer-base-buffer))
         (filename (file-truename (buffer-file-name base))))
    (append (if (doom-project-p)
                (let* ((project-root (doom-project-root))
                       (relative-dirs (file-relative-name (file-name-directory filename)
                                                          (file-truename project-root))))
                  (list (cons (concat (doom-project-name) "/")
                              'doom-modeline-buffer-project-root)
                        (unless (equal "./" relative-dirs)
                          (cons relative-dirs 'doom-modeline-buffer-path))))
              (list nil (cons (abbreviate-file-name (file-name-directory filename))
                              'doom-modeline-buffer-path)))
            (list (cons (file-name-nondirectory filename)
                        'doom-modeline-buffer-file)))))

(defun +modeline-file-path-from-project ()
  "Returns file path relative to the project root.

e.g. src/lib/file.c

Meant for `+modeline-buffer-path-function'."
  (cdr (+modeline-file-path-with-project)))

(defun +modeline-file-path-truncated-with-project ()
  "Returns file path relative to (and including) project root, with descendent
folders truncated.

e.g. project/s/l/file.c

Meant for `+modeline-buffer-path-function'."
  (let* ((parts (+modeline-file-path-with-project))
         (dirs (car (nth 1 parts))))
    (setcar (nth 1 parts)
            (shrink-path--dirs-internal dirs t))
    parts))

(defun +modeline-file-path-truncated-upto-project ()
  "Returns file path, truncating segments prior to the project.

e.g. ~/w/project/src/lib/file.c

Meant for `+modeline-buffer-path-function'."
  (pcase-let
      ((`(,root-parent ,root ,dir, file)
        (let ((buffer-file-name (or buffer-file-name (buffer-file-name (buffer-base-buffer)))))
          (shrink-path-file-mixed (or (doom-project-root) default-directory)
                                  (file-name-directory buffer-file-name)
                                  buffer-file-name))))
    (list (cons root-parent 'font-lock-comment-face)
          (cons root 'doom-modeline-buffer-project-root)
          (cons (concat "/" dir) 'doom-modeline-buffer-path)
          (cons file 'doom-modeline-buffer-file))))

(defun +modeline-file-path-truncated-upto-project-root ()
  "Return file path, truncating segemnts prior to (and including) the project
root.

e.g. ~/w/p/src/lib/file.c

Meant for `+modeline-buffer-path-function'."
  (let* ((parts (+modeline-file-path-truncated-upto-project))
         (root (car (nth 1 parts))))
    (setcar (nth 1 parts)
            (let ((first (substring root 0 1)))
              (if (equal first ".")
                  (substring root 0 2)
                first)))
    parts))

(defun +modeline-file-path-truncated ()
  "Return absolute file path with all directories truncated.

e.g. ~/w/p/s/l/file.c

Meant for `+modeline-buffer-path-function'."
  (pcase-let ((`(,dir . ,file)
               (shrink-path-prompt (buffer-file-name (buffer-base-buffer)))))
    (list (cons dir 'doom-modeline-buffer-path)
          (cons file 'doom-modeline-buffer-file))))

(defun +modeline-file-name ()
  "Return buffer name.

e.g. file.c

Meant for `+modeline-buffer-path-function'."
  (list (cons "%b" 'doom-modeline-buffer-path)))


;;
;; Bars

(defvar +modeline-bar-start nil "TODO")
(put '+modeline-bar-start 'risky-local-variable t)
(defvar +modeline-bar-end nil "TODO")
(put '+modeline-bar-end 'risky-local-variable t)

(defvar +modeline-bar-active nil "TODO")
(defvar +modeline-bar-inactive nil "TODO")
(defun +modeline|setup-bars ()
  (setq +modeline-bar-active
        (+modeline--make-xpm +modeline-width +modeline-height
                             (unless +modeline-bar-invisible
                               (face-background 'doom-modeline-bar nil t)))
        +modeline-bar-inactive
        (+modeline--make-xpm +modeline-width +modeline-height))
  (setq +modeline-bar-start nil
        +modeline-bar-end nil)
  (if +modeline-bar-at-end
      (setq +modeline-bar-end '+modeline-bar)
    (setq +modeline-bar-start '+modeline-bar)))
(add-hook 'doom-load-theme-hook #'+modeline|setup-bars)

(defun +modeline|setup-bars-after-change (sym val op _where)
  (when (eq op 'set)
    (set sym val)
    (+modeline|setup-bars)))
(add-variable-watcher '+modeline-width  #'+modeline|setup-bars-after-change)
(add-variable-watcher '+modeline-height #'+modeline|setup-bars-after-change)
(add-variable-watcher '+modeline-bar-at-end #'+modeline|setup-bars-after-change)
(add-variable-watcher '+modeline-bar-invisible #'+modeline|setup-bars-after-change)

(def-modeline-segment! +modeline-bar
  (if (active) +modeline-bar-active +modeline-bar-inactive))


;;
;; Segments

(def-modeline-segment! +modeline-buffer-state
  (let* ((base (buffer-base-buffer))
         (icon (cond (buffer-read-only
                      (all-the-icons-octicon
                       "lock"
                       :face 'doom-modeline-warning
                       :v-adjust -0.05))
                     ((buffer-modified-p base)
                      (all-the-icons-faicon
                       "floppy-o"
                       :face 'doom-modeline-buffer-modified
                       :v-adjust -0.05))
                     ((and (buffer-file-name base)
                           (not (file-exists-p (buffer-file-name base))))
                      (all-the-icons-octicon
                       "circle-slash"
                       :face 'doom-modeline-urgent
                       :v-adjust -0.05)))))
    (if icon (concat icon " "))))

(def-modeline-segment! +modeline-buffer-id
  :on-hooks (find-file-hook after-save-hook after-revert-hook)
  :init (propertize "%b" 'face 'doom-modeline-buffer-file)
  :faces t
  (let ((file-path (buffer-file-name (buffer-base-buffer))))
    (propertize (+modeline-build-path file-path)
                'help-echo file-path)))

(def-modeline-segment! +modeline-buffer-directory
  (let ((face (if (active) 'doom-modeline-buffer-path)))
    (propertize
     (concat (if (display-graphic-p) " ")
             (all-the-icons-octicon
              "file-directory"
              :face face
              :v-adjust -0.1
              :height 1.25)
             " "
             (propertize (abbreviate-file-name default-directory)
                         'face face))
     'help-echo default-directory)))

(def-modeline-segment! +modeline-vcs
  :on-set (vc-mode)
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (state   (vc-state buffer-file-name backend)))
      (let ((face    'mode-line-inactive)
            (active  (active))
            (all-the-icons-default-adjust -0.1))
        (concat (cond ((memq state '(edited added))
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
                +modeline--vspc
                (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                            'face (if active face)))))))

(def-modeline-segment! +modeline-indent-style
  :on-hooks (after-revert-hook after-save-hook find-file-hook)
  :on-set (indent-tabs-mode tab-width)
  (propertize (format "%s%d  "
                      (if indent-tabs-mode "⭾" "␣")
                      tab-width)
              'help-echo
              (format "Indentation: %d %s wide"
                      tab-width
                      (if indent-tabs-mode "tabs" "spaces"))))

(def-modeline-segment! +modeline-encoding
  :on-hooks (after-revert-hook after-save-hook find-file-hook)
  :on-set (buffer-file-coding-system)
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 (propertize "LF" 'help-echo "EOL convention: \\n (Unix)"))
            (1 (propertize "CRLF" 'help-echo "EOL convention: \\r\\n (Windows, Symbian OS, etc)"))
            (2 (propertize "CR" 'help-echo "EOL convention: \\r (pre-OSX MacOS)")))
          "  "
          (let* ((sys (coding-system-plist buffer-file-coding-system))
                 (category (plist-get sys :category)))
            (propertize
             (cond ((eq category 'coding-category-undecided)
                    "")
                   ((or (eq category 'coding-category-utf-8)
                        (eq (plist-get sys :name) 'prefer-utf-8))
                    "UTF-8  ")
                   ((concat (upcase (symbol-name (plist-get sys :name)))
                            "  ")))
             'help-echo (plist-get (coding-system-plist buffer-file-coding-system) :docstring)))))

(def-modeline-segment! +modeline-major-mode
  (propertize (format-mode-line mode-name)
              'face (if (active) 'doom-modeline-buffer-major-mode)))

(defun +modeline--macro-recording ()
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

(defsubst +modeline--anzu ()
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
             ((format " %s/%d " here total))))
     'face (if (active) 'doom-modeline-panel))))

(defsubst +modeline--evil-substitute ()
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

(defsubst +modeline--iedit ()
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

(def-modeline-segment! +modeline-matches
  "Displays: 1. the currently recording macro, 2. A current/total for the
current search term (with anzu), 3. The number of substitutions being conducted
with `evil-ex-substitute', and/or 4. The number of active `iedit' regions."
  (let ((meta (concat (+modeline--macro-recording)
                      (+modeline--anzu)
                      (+modeline--evil-substitute)
                      (+modeline--iedit)
                      " ")))
     (or (and (not (equal meta " ")) meta)
         (if buffer-file-name " %I "))))

;;
(defsubst doom-column (pos)
  (save-excursion (goto-char pos)
                  (current-column)))

(defvar-local +modeline-enable-word-count nil
  "If non-nil, a word count will be added to the selection-info modeline
segment.")

(defun +modeline|enable-word-count ()
  (setq +modeline-enable-word-count t))
(add-hook 'text-mode-hook #'+modeline|enable-word-count)

(def-modeline-segment! +modeline-selection-info
  (let ((beg (or evil-visual-beginning (region-beginning)))
        (end (or evil-visual-end (region-end))))
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
               (when +modeline-enable-word-count
                 (format " %dW" (count-words beg end)))))
     'face 'doom-modeline-highlight)))

(defun +modeline|enable-selection-info ()
  (add-to-list '+modeline-format-left '+modeline-selection-info t #'eq))
(defun +modeline|disable-selection-info ()
  (setq +modeline-format-left (delq '+modeline-selection-info +modeline-format-left)))
(cond ((featurep 'evil)
       (add-hook 'evil-visual-state-entry-hook #'+modeline|enable-selection-info)
       (add-hook 'evil-visual-state-exit-hook #'+modeline|disable-selection-info))
      ((add-hook 'activate-mark-hook #'+modeline|enable-selection-info)
       (add-hook 'deactivate-mark-hook #'+modeline|disable-selection-info)))

;; flycheck
(defun +doom-ml-icon (icon &optional text face voffset)
  "Displays an octicon ICON with FACE, followed by TEXT. Uses
`all-the-icons-octicon' to fetch the icon."
  (concat (when icon
            (concat
             (all-the-icons-material icon :face face :height 1.1 :v-adjust (or voffset -0.2))
             (if text +modeline--vspc)))
          (if text (propertize text 'face face))))

(defun +modeline-flycheck-status (status)
  (pcase status
    (`finished (if flycheck-current-errors
                   (let-alist (flycheck-count-errors flycheck-current-errors)
                     (let ((sum (+ (or .error 0) (or .warning 0))))
                       (+doom-ml-icon "do_not_disturb_alt"
                                      (number-to-string sum)
                                      (if .error 'doom-modeline-urgent 'doom-modeline-warning)
                                      -0.25)))
                 (+doom-ml-icon "check" nil 'doom-modeline-info)))
    (`running     (+doom-ml-icon "access_time" nil 'font-lock-doc-face -0.25))
    ;; (`no-checker  (+doom-ml-icon "sim_card_alert" "-" 'font-lock-doc-face))
    (`errored     (+doom-ml-icon "sim_card_alert" "Error" 'doom-modeline-urgent))
    (`interrupted (+doom-ml-icon "pause" "Interrupted" 'font-lock-doc-face))))

(defun +doom-modeline|update-flycheck-segment (&optional status)
  (setq +modeline-flycheck
        (when-let* ((status-str (+modeline-flycheck-status status)))
          (concat +modeline--vspc status-str " "))))
(add-hook 'flycheck-mode-hook #'+doom-modeline|update-flycheck-segment)
(add-hook 'flycheck-status-changed-functions #'+doom-modeline|update-flycheck-segment)

(def-modeline-segment! +modeline-flycheck
  "Displays color-coded flycheck error status in the current buffer with pretty
icons."
  :init nil)


;;
;; Preset modeline formats

(def-modeline-format! :main
  '(+modeline-matches " "
    +modeline-buffer-state
    +modeline-buffer-id
    "  %2l:%c %p  ")
  `(mode-line-misc-info
    +modeline-indent-style
    +modeline-encoding
    +modeline-major-mode " "
    (vc-mode (" " +modeline-vcs " "))
    mode-line-process
    +modeline-flycheck))

(def-modeline-format! :minimal
  '(+modeline-matches " "
    +modeline-buffer-state
    +modeline-buffer-id)
  '(+modeline-major-mode))

(def-modeline-format! :special
  '(+modeline-matches +modeline-buffer-state " %b " +modeline-buffer-position)
  '(+modeline-encoding +modeline-major-mode mode-line-process))

(def-modeline-format! :project
  '(+modeline-buffer-directory)
  '(+modeline-major-mode))


;;
(def-modeline-segment! +modeline--rest
  (let ((rhs-str (format-mode-line +modeline-format-right)))
    (list (propertize
           " " 'display
           `((space :align-to (- (+ right right-fringe right-margin)
                                 ,(1+ (string-width rhs-str))))))
          rhs-str)))

(setq-default mode-line-format '("" +modeline-bar-start +modeline-format-left +modeline--rest +modeline-bar-end))


;;
(set-modeline! :main t)
(add-hook! '+doom-dashboard-mode-hook (set-modeline! :project))
(add-hook! 'doom-scratch-buffer-hook  (set-modeline! :special))
