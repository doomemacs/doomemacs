;;; config.el --- make mode-lines sexy again

;; all-the-icons doesn't work in the terminal, so we "disable" it.
(unless (display-graphic-p)
  (defun all-the-icons-octicon (&rest _) "" "")
  (defun all-the-icons-faicon (&rest _) "" "")
  (defun all-the-icons-fileicon (&rest _) "" "")
  (defun all-the-icons-wicon (&rest _) "" "")
  (defun all-the-icons-alltheicon (&rest _) "" ""))

(after! eldoc-eval
  ;; Show eldoc in the mode-line with `eval-expression'
  (setq eldoc-in-minibuffer-show-fn '+doom-modeline--show-eldoc)
  (eldoc-in-minibuffer-mode +1))


;;; Flash the mode-line on error
;; TODO More flexible colors (only suits dark themes)
;; FIXME fast key-repeat can make the mode-line bg get stuck (rare)
(defvar doom--visual-bell-bg nil)
(setq ring-bell-function 'doom-visual-bell
      visible-bell nil)
(defun doom-visual-bell ()
  "Blink the mode-line red briefly."
  (unless doom--visual-bell-bg
    (setq doom--visual-bell-bg (face-attribute 'mode-line :background)))
  (set-face-attribute 'mode-line nil :background "#54252C")
  (run-with-timer
   0.1 nil
   (lambda () (set-face-attribute 'mode-line nil :background doom--visual-bell-bg))))


;; Don't show modeline in popup windows without a :modeline rule. If one exists
;; and it's a symbol, use it to find the modeline format in
;; `doom-modeline-formats'. If nil, show the mode-line as normal. If t, then
;; hide the modeline entirely.
(add-hook! doom-popup-mode
  (if (and (not doom-popup-mode)
           doom-hide-modeline-mode)
      (doom-hide-modeline-mode -1)
    (let ((modeline (plist-get doom-popup-rules :modeline)))
      (cond ((eq modeline 'nil)
             (doom-hide-modeline-mode +1))
            ((symbolp modeline)
             (let ((doom--hidden-modeline-format (+doom-modeline modeline)))
               (doom-hide-modeline-mode +1)))))))


;;
;; Variables
;;

(defvar doom-modeline-formats '()
  "An alist of mode-line formats. Use `+doom-modeline' to retrieve one.")

(defvar doom-modeline-height 29
  "How tall the mode-line should be (only respected in GUI emacs).")

(defvar doom-modeline-bar-width 3
  "How wide the mode-line bar should be (only respected in GUI emacs).")


;;
;; Custom faces
;;

(defface doom-modeline-buffer-path
  '((t (:inherit mode-line :bold t)))
  "Face used for the dirname part of the buffer path.")

(defface doom-modeline-buffer-project
  '((t (:inherit doom-modeline-buffer-path :bold nil)))
  "Face used for the filename part of the mode-line buffer path.")

(defface doom-modeline-buffer-modified
  '((t (:inherit highlight :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line.")

(defface doom-modeline-buffer-major-mode
  '((t (:inherit mode-line :bold t)))
  "Face used for the major-mode segment in the mode-line.")

(defface doom-modeline-highlight
  '((t (:inherit mode-line)))
  "Face for bright segments of the mode-line.")

(defface doom-modeline-panel
  '((t (:inherit mode-line)))
  "Face for 'X out of Y' segments, such as `*anzu', `*evil-substitute' and
`iedit'")

(defface doom-modeline-info
  `((t (:inherit success)))
  "Face for info-level messages in the modeline. Used by `*vc'.")

(defface doom-modeline-warning
  `((t (:inherit warning)))
  "Face for warnings in the modeline. Used by `*flycheck'")

(defface doom-modeline-urgent `((t (:inherit error)))
  "Face for errors in the modeline. Used by `*flycheck'")

;; Bar
(defface doom-modeline-bar '((t (:inherit highlight :foreground nil)))
  "The face used for the left-most bar on the mode-line of an active window.")

(defface doom-modeline-eldoc-bar '((t (:inherit shadow :foreground nil)))
  "The face used for the left-most bar on the mode-line when eldoc-eval is
active.")

(defface doom-modeline-inactive-bar '((t (:inherit mode-line-inactive)))
  "The face used for the left-most bar on the mode-line of an inactive window.")


;;
;; Bootstrap
;;

;; all-the-icons doesn't work in the terminal, so we "disable" it.
(unless (display-graphic-p)
  (defun all-the-icons-octicon (&rest _) "" "")
  (defun all-the-icons-faicon (&rest _) "" "")
  (defun all-the-icons-fileicon (&rest _) "" "")
  (defun all-the-icons-wicon (&rest _) "" "")
  (defun all-the-icons-alltheicon (&rest _) "" ""))

;; Show version string for multi-version managers like rvm, rbenv, pyenv, etc.
(defvar-local +doom-modeline-env-version nil)
(defvar-local +doom-modeline-env-command nil)
(add-hook 'focus-in-hook  '+doom-modeline|update-env)
(add-hook 'find-file-hook '+doom-modeline|update-env)
(defun +doom-modeline|update-env ()
  (when +doom-modeline-env-command
    (let* ((default-directory (doom-project-root))
           (s (shell-command-to-string doom-ml--env-command)))
      (setq +doom-modeline-env-version (if (string-match "[ \t\n\r]+\\'" s)
                                          (replace-match "" t t s)
                                        s)))))

;; Only support python and ruby for now
(add-hook! python-mode (setq +doom-modeline-env-command "python --version 2>&1 | cut -d' ' -f2"))
(add-hook! ruby-mode   (setq +doom-modeline-env-command "ruby --version 2>&1 | cut -d' ' -f2"))


;;
;; Library
;;

(defun +doom-modeline (key)
  (let ((modeline (assq key doom-modeline-formats)))
    (unless modeline
      (error "Modeline preset doesn't exist: %s" key))
    (cdr modeline)))

(defun +doom-modeline-define (name &rest forms)
  "Define a modeline format."
  (push `(,name . ,forms) doom-modeline-formats))


;;
;; Modeline helpers
;;

(defsubst active () (eq (selected-window) powerline-selected-window))

(defun +doom-modeline--flycheck-count (state)
  "Return flycheck information for the given error type STATE."
  (when (flycheck-has-current-errors-p state)
    (if (eq 'running flycheck-last-status-change)
        "?"
      (cdr-safe (assq state (flycheck-count-errors flycheck-current-errors))))))

(defun +doom-modeline--make-xpm (color height width)
  "Create an XPM bitmap."
  (when (display-graphic-p)
    (propertize
     " " 'display
     (let ((data nil)
           (i 0))
       (setq data (make-list height (make-list width 1)))
       (pl/make-xpm "percent" color color (reverse data))))))
;; Definitely not premature optimization, I-I swear!
(pl/memoize '+doom-modeline--make-xpm)

(defun +doom-modeline--buffer-path ()
  "Displays the buffer's full path relative to the project root (includes the
project root). Excludes the file basename. See `doom-buffer-name' for that."
  (if buffer-file-name
    (let* ((default-directory (f-dirname buffer-file-name))
           (buffer-path (f-relative buffer-file-name (doom-project-root)))
           (max-length (truncate (* (window-body-width) 0.4))))
      (when (and buffer-path (not (equal buffer-path ".")))
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
          buffer-path)))
    "%b"))

(defun +doom-modeline--show-eldoc (input)
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




;;
;; Segments
;;

(defun *buffer-project ()
  "Displays `default-directory', for special buffers like the scratch buffer."
  (let ((face (if (active) 'doom-modeline-buffer-project)))
    (concat (all-the-icons-octicon
             "file-directory"
             :face face
             :v-adjust -0.05
             :height 1.25)
            (propertize (concat " " (abbreviate-file-name (doom-project-root)))
                        'face face))))

(defun *buffer-info ()
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (let ((all-the-icons-scale-factor 1.2)
        (modified-p (buffer-modified-p))
        faces)
    (if (active)   (push 'doom-modeline-buffer-path faces))
    (if modified-p (push 'doom-modeline-buffer-modified faces))
    (concat (if buffer-read-only
                (concat (all-the-icons-octicon
                         "lock"
                         :face 'doom-modeline-warning
                         :v-adjust -0.05)
                        " ")
              (when modified-p
                (concat
                 (all-the-icons-faicon "floppy-o"
                                       :face 'doom-modeline-buffer-modified
                                       :v-adjust -0.1)
                 " ")))
            (when (and buffer-file-name (not (file-exists-p buffer-file-name)))
              (concat (all-the-icons-octicon
                       "circle-slash"
                       :face 'doom-modeline-urgent
                       :v-adjust -0.05)
                      " "))
            (propertize (+doom-modeline--buffer-path)
                        'face (if faces `(:inherit ,faces))))))

(defun *buffer-encoding ()
  "The encoding and eol style of the buffer."
  (concat (let ((eol-type (coding-system-eol-type buffer-file-coding-system)))
            (cond ((eq eol-type 0) "LF  ")
                  ((eq eol-type 1) "CRLF  ")
                  ((eq eol-type 2) "CR  ")))
          (let* ((sys (coding-system-plist buffer-file-coding-system))
                 (sys-name (plist-get sys :name))
                 (sys-cat (plist-get sys :category)))
            (cond ((memq sys-cat '(coding-category-undecided coding-category-utf-8))
                   "UTF-8")
                  (t (upcase (symbol-name sys-name)))))
          "  "))

(defun *major-mode ()
  "The major mode, including process, environment and text-scale info."
  (propertize
   (concat (format-mode-line mode-name)
           (if (stringp mode-line-process) mode-line-process)
           (if +doom-modeline-env-version (concat " " +doom-modeline-env-version))
           (and (featurep 'face-remap)
                (/= text-scale-mode-amount 0)
                (format " (%+d)" text-scale-mode-amount)))
   'face (if (active) 'doom-modeline-buffer-major-mode)))

(defun *vc ()
  "Displays the current branch, colored based on its state."
  (when (and vc-mode buffer-file-name)
    (let ((backend (vc-backend buffer-file-name))
          (state   (vc-state buffer-file-name))
          (face    'mode-line-inactive)
          (active  (active))
          (all-the-icons-scale-factor 1.0)
          (all-the-icons-default-adjust -0.1))
      (concat (propertize " " 'face 'variable-pitch)
              (cond ((memq state '(edited added))
                     (if active (setq face 'doom-modeline-info))
                     (all-the-icons-octicon
                      "git-branch"
                      :face face
                      :height 1.2
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
                     (if active (setq face 'mode-line))
                     (all-the-icons-octicon
                      "git-branch"
                      :face face
                      :height 1.2
                      :v-adjust -0.05)))
              " "
              (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                          'face (if active face))
              "  "
              (propertize " " 'face 'variable-pitch)))))

(defvar-local doom--flycheck-err-cache nil "")
(defvar-local doom--flycheck-cache nil "")
(defun *flycheck ()
  "Persistent and cached flycheck indicators in the mode-line."
  (when (and (featurep 'flycheck) flycheck-mode)
    (if (or flycheck-current-errors
            (eq 'running flycheck-last-status-change))
        (or (and (or (eq doom--flycheck-err-cache doom--flycheck-cache)
                     (memq flycheck-last-status-change '(running not-checked)))
                 (if (eq flycheck-last-status-change 'running)
                     (concat " "
                             (all-the-icons-octicon
                              "ellipsis"
                              :face 'font-lock-doc-face
                              :height 1.1
                              :v-adjust 0)
                             " ")
                   doom--flycheck-cache))
            (and (setq doom--flycheck-err-cache flycheck-current-errors)
                 (setq doom--flycheck-cache
                       (let ((fw (+doom-modeline--flycheck-count 'warning))
                             (fe (+doom-modeline--flycheck-count 'error)))
                         (concat (if (or fe fw) "  ")
                                 (if fe (concat
                                         (all-the-icons-octicon "circle-slash" :face 'doom-modeline-urgent :height 1.0 :v-adjust 0)
                                         (propertize " " 'face 'variable-pitch)
                                         (propertize (format "%d" fe) 'face 'doom-modeline-urgent)
                                         " "
                                         ))
                                 (if fw (concat
                                         (all-the-icons-octicon "alert" :face 'doom-modeline-warning :height 0.9 :v-adjust 0)
                                         (propertize " " 'face 'variable-pitch)
                                         (propertize (format "%d" fw) 'face 'doom-modeline-warning)
                                         " "
                                         ))
                                 (if (or fe fw)
                                     "  "
                                   (when (active)
                                     (all-the-icons-octicon "check" :height 1.2 :v-adjust -0.06))))))))
      (concat
       "  "
       (all-the-icons-octicon "check"
                              :face (if (active) 'doom-modeline-info)
                              :height 1.2
                              :v-adjust -0.06)
       " "))))

(defun *selection-info ()
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and (active) (evil-visual-state-p))
    (concat
     " "
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
      'face 'doom-modeline-highlight))))

(defun *macro-recording ()
  "Display current macro being recorded."
  (when (and (active) defining-kbd-macro)
    (let ((sep (propertize " " 'face 'doom-modeline-panel)))
      (concat sep
              (propertize (char-to-string evil-this-macro)
                          'face 'doom-modeline-panel)
              sep
              (all-the-icons-octicon "triangle-right"
                                     :face 'doom-modeline-panel
                                     :v-adjust -0.05)
              sep))))

(make-variable-buffer-local 'anzu--state)
(defun *anzu ()
  "Show the match index and total number thereof. Requires `evil-anzu'."
  (when (and (featurep 'evil-anzu) (evil-ex-hl-active-p 'evil-ex-search))
    (propertize
     (format " %s/%d%s "
             anzu--current-position anzu--total-matched
             (if anzu--overflow-p "+" ""))
     'face (if (active) 'doom-modeline-panel))))

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
     'face (if (active) 'doom-modeline-panel))))

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
     'face (if (active) 'doom-modeline-panel))))

(defun *media-info ()
  (cond ((eq major-mode 'image-mode)
         (let ((size (image-size (image-get-display-property) :pixels)))
           (format "  %dx%d  " (car size) (cdr size))))))


;;
;; Mode lines
;;

(+doom-modeline-define 'main
  `(:eval
    (let* ((meta (concat (*macro-recording)
                         (*anzu)
                         (*evil-substitute)
                         (*iedit)))
           (lhs (list (+doom-modeline--make-xpm (face-background (if (active)
                                                                     'doom-modeline-bar
                                                                   'doom-modeline-inactive-bar))
                                                doom-modeline-height
                                                doom-modeline-bar-width)
                      (if (= (length meta) 0) " %I " meta)
                      " "
                      (*buffer-info)
                      "  %l:%c %p  "
                      (*selection-info)))
           (rhs (list (*buffer-encoding)
                      (*vc)
                      (*major-mode)
                      (*flycheck)))
           (mid (propertize
                 " " 'display `((space :align-to (- (+ right right-fringe right-margin)
                                                    ,(+ 1 (string-width (format-mode-line rhs)))))))))
      (list lhs mid rhs))))

(+doom-modeline-define 'eldoc
  `(:eval
    (list (list ,(when (display-graphic-p)
                   `(+doom-modeline--make-xpm (face-background 'doom-modeline-eldoc-bar)
                                              doom-modeline-height
                                              doom-modeline-bar-width))
                (and (bound-and-true-p str) str))
          (propertize " " 'display `((space :align-to (1- (+ right right-fringe right-margin))))))))

(+doom-modeline-define 'minimal
  `(:eval
    (let* ((lhs (list (+doom-modeline--make-xpm (face-background (if (active)
                                                                     'doom-modeline-bar
                                                                   'doom-modeline-inactive-bar))
                                                doom-modeline-height
                                                doom-modeline-bar-width)
                      " "
                      (*buffer-project)))
           (rhs (list (*major-mode)))
           (mid (propertize
                 " " 'display `((space :align-to (- (+ right right-fringe right-margin)
                                                    ,(+ 1 (string-width (format-mode-line rhs)))))))))
      (list lhs mid rhs))))

;;
(setq-default mode-line-format (+doom-modeline 'main))

