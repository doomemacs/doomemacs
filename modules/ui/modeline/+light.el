;;; ui/modeline/+default.el -*- lexical-binding: t; -*-

(defvar +modeline-height 33)
(defvar +modeline-bar-width 3)

;; This is a slimmed down version of `doom-modeline' that manipulates
;; `mode-line-format' directly. Its purpose is to be a *significantly* lighter
;; modeline for doom. Upstream has generalized and grown too much so I've
;; returned to the roots.
;;
;; TODO Refactor me

(defface +modeline-success-highlight '((t (:inherit mode-line-highlight)))
  "TODO")

(defvar +modeline--redisplayed-p nil)
(defadvice! modeline-recalculate-height-a (&optional _force &rest _ignored)
  :before '(fit-window-to-buffer resize-temp-buffer-window)
  (unless +modeline--redisplayed-p
    (setq-local +modeline--redisplayed-p t)
    (redisplay t)))

;;; `active'
(defvar selected-window (selected-window))
(defun active () (eq (selected-window) selected-window))
(add-hook! 'pre-redisplay-functions
  (defun set-selected-window (&rest _)
    "Set the variable `selected-window' appropriately."
    (let ((win (selected-window)))
      (unless (minibuffer-window-active-p win)
        (setq selected-window (frame-selected-window))))))

;;; Helpers
(defun +modeline--make-xpm (color width height)
  "Create an XPM bitmap via COLOR, WIDTH and HEIGHT. Inspired by `powerline''s `pl/+modeline--make-xpm'."
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

(defun +modeline-format-icon (icon label &optional face help-echo voffset)
  (propertize (concat (all-the-icons-material
                       icon
                       :face face
                       :height 1.1
                       :v-adjust (or voffset -0.225))
                      (propertize label 'face face))
              'help-echo help-echo))


;;
;;; Segments

;;; `+modeline-bar'
(defvar +modeline-bar "")
(defvar +modeline-inactive-bar "")
(put '+modeline-bar 'risky-local-variable t)
(put '+modeline-inactive-bar 'risky-local-variable t)

(defface +modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window.")

(defface +modeline-bar-inactive '((t (:inherit mode-line-inactive)))
  "The face used for the left-most bar on the mode-line of an inactive window.")

(add-hook! 'doom-load-theme-hook
  (defun +modeline-refresh-bars-h ()
    (let ((width (or +modeline-bar-width 1)))
      (setq +modeline-bar
            (+modeline--make-xpm (if +modeline-bar-width (face-background '+modeline-bar nil 'inherit))
                                 width (max +modeline-height (frame-char-height)))
            +modeline-inactive-bar
            (+modeline--make-xpm (if +modeline-bar-width (face-background '+modeline-bar-inactive nil 'inherit))
                                 width (max +modeline-height (frame-char-height)))))))

(defvar +modeline--old-height nil)
(defun +modeline-adjust-height-h ()
  (unless +modeline--old-height
    (setq +modeline--old-height +modeline-height))
  (let ((default-height +modeline--old-height)
        (scale (or (frame-parameter nil 'font-scale) 0)))
    (if (> scale 0)
        (let* ((font-size (string-to-number
                           (aref (doom--font-name (frame-parameter nil 'font)
                                                  (selected-frame))
                                 xlfd-regexp-pixelsize-subnum)))
               (scale (frame-parameter nil 'font-scale)))
          (setq +modeline-height (+ default-height (* scale doom-font-increment))))
      (setq +modeline-height default-height))
    (setq +modeline-bar (+modeline--make-xpm nil 1 +modeline-height))))
(add-hook 'doom-change-font-size-hook #'+modeline-adjust-height-h)


;;; `+modeline-matches'
(progn
  (use-package! anzu
    :after-call isearch-mode
    :config
    ;; anzu and evil-anzu expose current/total state that can be displayed in the
    ;; mode-line.
    (defun doom-modeline-fix-anzu-count (positions here)
      "Calulate anzu counts via POSITIONS and HERE."
      (cl-loop for (start . end) in positions
               collect t into before
               when (and (>= here start) (<= here end))
               return (length before)
               finally return 0))

    (advice-add #'anzu--where-is-here :override #'doom-modeline-fix-anzu-count)

    (setq anzu-cons-mode-line-p nil) ; manage modeline segment ourselves
    ;; Ensure anzu state is cleared when searches & iedit are done
    (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
    (add-hook 'iedit-mode-end-hook #'anzu--reset-status)
    (advice-add #'evil-force-normal-state :before #'anzu--reset-status)
    ;; Fix matches segment mirroring across all buffers
    (mapc #'make-variable-buffer-local
          '(anzu--total-matched anzu--current-position anzu--state
                                anzu--cached-count anzu--cached-positions anzu--last-command
                                anzu--last-isearch-string anzu--overflow-p)))

  (use-package! evil-anzu
    :when (featurep! :editor evil)
    :after-call (evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight))

  (defun +modeline--anzu ()
    "Show the match index and total number thereof.
Requires `anzu', also `evil-anzu' if using `evil-mode' for compatibility with
`evil-search'."
    (when (and (bound-and-true-p anzu--state)
               (not (bound-and-true-p iedit-mode)))
      (propertize
       (let ((here anzu--current-position)
             (total anzu--total-matched))
         (cond ((eq anzu--state 'replace-query)
                (format " %d replace " anzu--cached-count))
               ((eq anzu--state 'replace)
                (format " %d/%d " here total))
               (anzu--overflow-p
                (format " %s+ " total))
               (t
                (format " %s/%d " here total))))
       'face (if (active) 'mode-line-highlight))))

  (defun +modeline--evil-substitute ()
    "Show number of matches for evil-ex substitutions and highlights in real time."
    (when (and (bound-and-true-p evil-local-mode)
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
       'face (if (active) 'mode-line-highlight))))

  (defun +mode-line--multiple-cursors ()
    "Show the number of multiple cursors."
    (when (bound-and-true-p evil-mc-cursor-list)
      (let ((count (length evil-mc-cursor-list)))
        (when (> count 0)
          (let ((face (cond ((not (active)) 'mode-line-inactive)
                            (evil-mc-frozen 'mode-line-highlight)
                            ('+modeline-success-highlight))))
            (concat (propertize " " 'face face)
                    (all-the-icons-faicon "i-cursor" :face face :v-adjust -0.0575)
                    (propertize " " 'face `(:inherit (variable-pitch ,face)))
                    (propertize (format "%d " count)
                                'face face)))))))

  (defun mode-line--overlay< (a b)
    "Sort overlay A and B."
    (< (overlay-start a) (overlay-start b)))

  (defun +modeline--iedit ()
    "Show the number of iedit regions matches + what match you're on."
    (when (and (bound-and-true-p iedit-mode)
               (bound-and-true-p iedit-occurrences-overlays))
      (propertize
       (let ((this-oc (or (let ((inhibit-message t))
                            (iedit-find-current-occurrence-overlay))
                          (save-excursion
                            (iedit-prev-occurrence)
                            (iedit-find-current-occurrence-overlay))))
             (length (length iedit-occurrences-overlays)))
         (format " %s/%d "
                 (if this-oc
                     (- length
                        (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                    #'mode-line--overlay<)))
                        -1)
                   "-")
                 length))
       'face (if (active) 'mode-line-highlight))))

  (defun +modeline--macro-recording ()
    "Display current Emacs or evil macro being recorded."
    (when (and (active)
               (or defining-kbd-macro
                   executing-kbd-macro))
      (let ((sep (propertize " " 'face 'mode-line-highlight)))
        (concat sep
                (propertize (if (bound-and-true-p evil-this-macro)
                                (char-to-string evil-this-macro)
                              "Macro")
                            'face 'mode-line-highlight)
                sep
                (all-the-icons-octicon "triangle-right"
                                       :face 'mode-line-highlight
                                       :v-adjust -0.05)
                sep))))

  (defvar +modeline-matches
    '(:eval
      (let ((meta (concat (+modeline--macro-recording)
                          (+modeline--anzu)
                          (+modeline--evil-substitute)
                          (+modeline--iedit)
                          (+mode-line--multiple-cursors))))
        (or (and (not (equal meta "")) meta)
            " %I "))))
  (put '+modeline-matches 'risky-local-variable t))


;;; `+modeline-modes'
(defvar +modeline-modes ; remove minor modes
 '(""
   (:propertize mode-name
                face bold
                mouse-face mode-line-highlight)
   mode-line-process
   "%n"
   "%]"
   " "))


;;; `+modeline-buffer-identification'
(defconst +modeline-buffer-identification ; slightly more informative buffer id
  '((:eval
     (propertize
      (let ((buffer-file-name (buffer-file-name (buffer-base-buffer))))
        (or (when buffer-file-name
              (if-let (project (doom-project-root buffer-file-name))
                  (let ((filename (or buffer-file-truename (file-truename buffer-file-name))))
                    (file-relative-name filename (concat project "..")))))
            "%b"))
      'face (cond ((buffer-modified-p)
                   '(error bold mode-line-buffer-id))
                  ((active)
                   'mode-line-buffer-id))
      'help-echo buffer-file-name))
    (buffer-read-only (:propertize " RO" face warning))))


;;; `+modeline-position'
(defvar +modeline-position '("  %l:%C %p  "))


;;; `+modeline-checker'
(defvar-local +modeline-checker nil
  "Displays color-coded error status in the current buffer with pretty
icons.")
(put '+modeline-checker 'risky-local-variable t)

(defun +modeline-checker-update (&optional status)
  "Update flycheck text via STATUS."
  (setq +modeline-checker
        (pcase status
          (`finished
           (if flycheck-current-errors
               (let-alist (flycheck-count-errors flycheck-current-errors)
                 (let ((error (or .error 0))
                       (warning (or .warning 0))
                       (info (or .info 0)))
                   (+modeline-format-icon "do_not_disturb_alt"
                                          (number-to-string (+ error warning info))
                                          (cond ((> error 0)   'error)
                                                ((> warning 0) 'warning)
                                                ('success))
                                          (format "Errors: %d, Warnings: %d, Debug: %d"
                                                  error
                                                  warning
                                                  info))))
             (+modeline-format-icon "check" "" 'success)))
          (`running     (+modeline-format-icon "access_time" "*" 'font-lock-comment-face "Running..."))
          (`errored     (+modeline-format-icon "sim_card_alert" "!" 'error "Errored!"))
          (`interrupted (+modeline-format-icon "pause" "!" 'font-lock-comment-face "Interrupted"))
          (`suspicious  (+modeline-format-icon "priority_high" "!" 'error "Suspicious")))))
(add-hook 'flycheck-status-changed-functions #'+modeline-checker-update)
(add-hook 'flycheck-mode-hook #'+modeline-checker-update)


;;; `+modeline-selection-info'
(defsubst doom-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(defun add-selection-segment ()
  (add-to-list '+modeline-format-left '+modeline-selection-info 'append))
(defun remove-selection-segment ()
  (delq! '+modeline-selection-info +modeline-format-left))

(if (featurep 'evil)
    (progn
      (add-hook 'evil-visual-state-entry-hook #'add-selection-segment)
      (add-hook 'evil-visual-state-exit-hook #'remove-selection-segment))
  (add-hook 'activate-mark-hook #'add-selection-segment)
  (add-hook 'deactivate-mark-hook #'remove-selection-segment))

(defvar +modeline-selection-info
  '(:eval
    (when (or mark-active
              (and (bound-and-true-p evil-local-mode)
                   (eq evil-state 'visual)))
      (cl-destructuring-bind (beg . end)
          (if (boundp 'evil-local-mode)
              (cons evil-visual-beginning evil-visual-end)
            (cons (region-beginning) (region-end)))
        (propertize
         (let ((lines (count-lines beg (min end (point-max)))))
           (concat " "
                   (cond ((or (bound-and-true-p rectangle-mark-mode)
                              (and (bound-and-true-p evil-visual-selection)
                                   (eq 'block evil-visual-selection)))
                          (let ((cols (abs (- (doom-modeline-column end)
                                              (doom-modeline-column beg)))))
                            (format "%dx%dB" lines cols)))
                         ((and (bound-and-true-p evil-visual-selection)
                               (eq evil-visual-selection 'line))
                          (format "%dL" lines))
                         ((> lines 1)
                          (format "%dC %dL" (- end beg) lines))
                         ((format "%dC" (- end beg))))
                   (when (derived-mode-p 'text-mode)
                     (format " %dW" (count-words beg end)))
                   " "))
         'face (if (active) 'success)))))
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection.")
(put '+modeline-selection-info 'risky-local-variable t)


;;; `+modeline-encoding'
(defconst +modeline-encoding
  '(:eval
    (concat (pcase (coding-system-eol-type buffer-file-coding-system)
              (0 " LF ")
              (1 " RLF ")
              (2 " CR "))
            (let ((sys (coding-system-plist buffer-file-coding-system)))
              (if (memq (plist-get sys :category)
                        '(coding-category-undecided coding-category-utf-8))
                  "UTF-8"
                (upcase (symbol-name (plist-get sys :name)))))
            "  ")))
(put '+modeline-encoding 'risky-local-variable t)


;;
;;; Setup

(defvar-local +modeline-format-left nil)
(put '+modeline-format-left 'risky-local-variable t)


(defvar-local +modeline-format-right nil)
(put '+modeline-format-right 'risky-local-variable t)

(setq-default
 +modeline-format-left
 '(""
   +modeline-matches
   " "
   +modeline-buffer-identification
   +modeline-position)

 +modeline-format-right
 `(""
   mode-line-misc-info
   +modeline-modes
   (vc-mode ("  "
             ,(all-the-icons-octicon "git-branch" :v-adjust 0.0)
             vc-mode " "))
   " "
   +modeline-encoding
   (+modeline-checker ("" +modeline-checker "   ")))

 ;;
 mode-line-format
 '(""
   +modeline-bar
   +modeline-format-left
   (:eval
    (propertize
     " "
     'display
     `((space :align-to (- (+ right right-fringe right-margin)
                           ,(string-width
                             (format-mode-line '("" +modeline-format-right))))))))
   +modeline-format-right))

(with-current-buffer "*Messages*"
  (setq mode-line-format (default-value 'mode-line-format)))


;;
;;; Other modelines

(defun set-project-modeline ()
  (setq +modeline-format-left
        `(" "
          ,(all-the-icons-octicon
            "file-directory"
            :face 'bold
            :v-adjust -0.05
            :height 1.25)
          (:propertize (" " (:eval (abbreviate-file-name default-directory)))
                       face bold))
        +modeline-format-right
        '("" +modeline-modes)))

(defun set-special-modeline ()
  (setq +modeline-format-left
        '(""
          +modeline-matches
          " "
          +modeline-buffer-identification)
        +modeline-format-right
        '("" +modeline-modes)))

(defun set-pdf-modeline ()) ; TODO `set-pdf-modeline'


;;
;;; Bootstrap

(size-indication-mode +1) ; filesize in modeline
(add-hook '+doom-dashboard-mode-hook #'set-project-modeline)
(add-hook 'doom-load-theme-hook #'+modeline-refresh-bars-h)

;; Other modes
(defun set-modeline-in-magit ()
  (if (eq major-mode 'magit-status-mode)
      (set-project-modeline)
    (hide-mode-line-mode)))
(add-hook 'magit-mode-hook #'set-modeline-in-magit)

(add-hook 'special-mode-hook #'set-special-modeline)
(add-hook 'image-mode-hook #'set-special-modeline)
(add-hook 'circe-mode-hook #'set-special-modeline)
(add-hook 'pdf-tools-enabled-hook #'set-pdf-modeline)
