;;; core-ui.el --- interface & mode-line config

(defconst doom-fringe-size '3 "Default fringe width")

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default
 indicate-buffer-boundaries nil ; don't show where buffer starts/ends
 indicate-empty-lines nil       ; don't show empty lines
 fringes-outside-margins t      ; switches order of fringe and margin
 ;; Keep cursors and highlights in current window only
 cursor-in-non-selected-windows nil
 highlight-nonselected-windows nil
 ;; Disable bidirectional text support for slight performance bonus
 bidi-display-reordering nil
 ;; Remove continuation arrow on right fringe
 fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                              fringe-indicator-alist)

 blink-matching-paren nil ; don't blink--too distracting
 show-paren-delay 0.075
 uniquify-buffer-name-style nil
 visible-bell nil
 visible-cursor nil
 x-stretch-cursor t
 use-dialog-box nil             ; always avoid GUI
 redisplay-dont-pause t         ; don't pause display on input
 split-width-threshold nil      ; favor horizontal splits
 show-help-function nil         ; hide :help-echo text
 jit-lock-defer-time nil
 jit-lock-stealth-nice 0.1
 jit-lock-stealth-time 0.2
 jit-lock-stealth-verbose nil
 ;; Minibuffer resizing
 resize-mini-windows 'grow-only
 max-mini-window-height 0.3
 ;; Ask for confirmation on exit only if there are real buffers left
 confirm-kill-emacs
 (lambda (_) (if window-system (if (doom/get-real-buffers) (y-or-n-p "› Quit?") t) t)))

;; Initialize UI
(load-theme doom-current-theme t)
(tooltip-mode -1) ; show tooltips in echo area
(if (not window-system)
    (menu-bar-mode -1)
  (scroll-bar-mode -1)  ; no scrollbar
  (tool-bar-mode   -1)  ; no toolbar
  ;; full filename in frame title
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  ;; set fonts
  (set-frame-font doom-default-font t)
  (set-face-attribute 'default t :font doom-current-font)
  ;; standardize fringe width
  (fringe-mode doom-fringe-size)
  (push `(left-fringe  . ,doom-fringe-size) default-frame-alist)
  (push `(right-fringe . ,doom-fringe-size) default-frame-alist)
  ;; Default frame size on startup
  (push '(width . 120) default-frame-alist)
  (push '(height . 40) default-frame-alist)
  ;; no fringe in the minibuffer
  (add-hook! (emacs-startup minibuffer-setup)
    (set-window-fringes (minibuffer-window) 0 0 nil))
  ;; Show tilde in margin on empty lines
  (define-fringe-bitmap 'tilde [64 168 16] nil nil 'center)
  (set-fringe-bitmap-face 'tilde 'fringe)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
  ;; Fix certain unicode characters without upsetting line-height
  (doom-fix-unicode "DejaVu Sans Mono" '(?★) 13)
  (doom-fix-unicode "Hack" '(?λ) 13))

;; Hide mode-line in help/compile window
(add-hook 'help-mode-hook 'doom|hide-mode-line)
(add-hook 'compilation-mode-hook 'doom|hide-mode-line)

;; On by default in Emacs 25. I'll enable it manually, so disable it globally
(when (and (> emacs-major-version 24) (featurep 'eldoc))
  (global-eldoc-mode -1))

(use-package eldoc-eval
  :config
  (setq eldoc-in-minibuffer-show-fn 'doom/eldoc-show-in-mode-line)
  (eldoc-in-minibuffer-mode +1))

;; Highlight TODO/FIXME/NOTE tags
(add-hook! (prog-mode emacs-lisp-mode css-mode)
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\(?:(.*)\\)?:?\\)\\>"  1 'warning prepend)
         ("\\<\\(FIXME\\(?:(.*)\\)?:?\\)\\>" 1 'error prepend)
         ("\\<\\(NOTE\\(?:(.*)\\)?:?\\)\\>"  1 'success prepend))))


;;
;; Plugins
;;

(use-package hl-line
  :init (add-hook! (prog-mode markdown-mode) 'hl-line-mode)
  :config
  ;; Doesn't seem to play nice in emacs 25+
  (when (< emacs-major-version 25)
    (setq hl-line-sticky-flag nil
          global-hl-line-sticky-flag nil))

  (defvar-local doom--hl-line-mode nil)
  (defun doom|hl-line-on ()  (if doom--hl-line-mode (hl-line-mode +1)))
  (defun doom|hl-line-off () (if doom--hl-line-mode (hl-line-mode -1)))
  (add-hook! hl-line-mode (if hl-line-mode (setq doom--hl-line-mode t)))
  ;; Disable line highlight in visual mode
  (add-hook 'evil-visual-state-entry-hook 'doom|hl-line-off)
  (add-hook 'evil-visual-state-exit-hook  'doom|hl-line-on))

(use-package visual-fill-column :defer t
  :config
  (setq-default visual-fill-column-center-text nil
                visual-fill-column-width fill-column))

(use-package highlight-indentation
  :commands (highlight-indentation-mode
             highlight-indentation-current-column-mode)
  :init
  (add-hook! (nxml-mode yaml-mode json-mode scss-mode
              c-mode-common ruby-mode python-mode lua-mode)
    'highlight-indentation-mode)
  (after! editorconfig
    (advice-add 'highlight-indentation-guess-offset
                :override 'doom*hl-indent-guess-offset))
  ;; A long-winded method for ensuring whitespace is maintained (so that
  ;; highlight-indentation-mode can display them consistently)
  (add-hook! highlight-indentation-mode
    (if highlight-indentation-mode
        (progn
          (doom/add-whitespace)
          (add-hook 'after-save-hook 'doom/add-whitespace nil t)
          (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
      (remove-hook 'after-save-hook 'doom/add-whitespace t)
      (remove-hook 'before-save-hook 'delete-trailing-whitespace t))))

(use-package highlight-numbers :commands (highlight-numbers-mode))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :config (setq rainbow-delimiters-max-face-count 3)
  :init
  (add-hook! (emacs-lisp-mode lisp-mode js-mode css-mode c-mode-common)
    'rainbow-delimiters-mode))

;; NOTE hl-line-mode and rainbow-mode don't play well together
(use-package rainbow-mode
  :commands (rainbow-mode)
  :init (add-hook 'rainbow-mode-hook 'doom|hl-line-off))

(use-package nlinum
  :commands nlinum-mode
  :preface
  (setq linum-format "%3d ")
  (defvar nlinum-format "%4d ")
  (defvar doom--hl-nlinum-overlay nil)
  (defvar doom--hl-nlinum-line nil)
  :init
  (add-hook!
    (markdown-mode prog-mode scss-mode web-mode conf-mode)
    'nlinum-mode)
  (add-hook! 'nlinum-mode-hook
    (if nlinum-mode-hook
        (add-hook 'post-command-hook 'doom|nlinum-hl-line nil t)
      (remove-hook 'post-command-hook 'doom|nlinum-hl-line t)))
  :config
  ;; Calculate line number column width
  (add-hook! nlinum-mode
    (setq nlinum--width (length (save-excursion (goto-char (point-max))
                                                (format-mode-line "%l")))))
  ;; Disable nlinum when making frames, otherwise we get linum face error
  ;; messages that prevent frame creation.
  (add-hook 'before-make-frame-hook 'doom|nlinum-disable)
  (add-hook 'after-make-frame-functions 'doom|nlinum-enable))


;;
;; Mode-line
;;

(use-package spaceline
  :init
  (defvar-local doom--env-version nil)
  (defvar-local doom--env-command nil)
  (defvar powerline-height 26)
  (defvar powerline-default-separator nil)

  :config
  (defface mode-line-is-modified nil "Face for mode-line modified symbol")
  (defface mode-line-buffer-file nil "Face for mode-line buffer file name")
  (defface mode-line-buffer-path nil "Face for mode-line buffer file path")

  ;; Custom modeline segments
  (spaceline-define-segment *buffer-path
    (concat
     (when buffer-file-name
       (powerline-raw
        (f-dirname
         (let ((buffer-path (f-relative buffer-file-name (doom/project-root)))
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
        (if active 'mode-line-buffer-path)))
     (powerline-raw "%b" (if active 'mode-line-buffer-file))
     (when buffer-file-name
       (powerline-raw
        (concat (when buffer-file-name
                  (concat
                   (if (buffer-modified-p) " ✱")
                   (if (not (file-exists-p buffer-file-name)) "[!]")))
                (if buffer-read-only "[RO]"))
        'mode-line-is-modified)))
    :tight-right t
    :skip-alternate t)

  (spaceline-define-segment *buffer-position
    "A more vim-like buffer position."
    (concat "(%l,%c) "
            (let ((start (window-start))
                  (end (window-end))
                  (pend (point-max)))
              (if (and (eq start 1)
                       (eq end pend))
                  ":All"
                (let ((perc (/ end 0.01 pend)))
                  (cond ((= start 1) ":Top")
                        ((>= perc 100) ":Bot")
                        (t (format ":%d%%%%" perc))))))
            " "))

  (defface mode-line-vcs-info nil '((t (:inherit warning))))
  (defface mode-line-vcs-warning nil '((t (:inherit warning))))
  (spaceline-define-segment *vc
    "Version control info"
    (when vc-mode
      (propertize
       (concat "⎇ " (substring vc-mode (+ 2 (length (symbol-name (vc-backend buffer-file-name))))))
       'face (when active
               (let ((state (vc-state buffer-file-name)))
                 (cond ((memq state '(edited added))
                        'mode-line-vcs-info)
                       ((memq state '(removed needs-merge needs-update conflict removed unregistered))
                        'mode-line-vcs-warning)))))))

  ;; search indicators
  (defface mode-line-count-face nil "")
  (make-variable-buffer-local 'anzu--state)
  (spaceline-define-segment *anzu
    "Show the current match number and the total number of matches. Requires
anzu to be enabled."
    (when (and (featurep 'evil-anzu)
               (evil-ex-hl-active-p 'evil-ex-search))
      (powerline-raw
       (let ((here anzu--current-position)
             (total anzu--total-matched))
         (format " %s/%d%s "
                 here total
                 (if anzu--overflow-p "+" "")))
       (if active 'mode-line-count-face 'mode-line-inactive)))
    :tight t)

  (spaceline-define-segment *iedit
    "Show the number of iedit regions matches + what match you're on."
    (when (bound-and-true-p iedit-mode)
      (propertize
       (let ((this-oc (iedit-find-current-occurrence-overlay))
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
       'face (if active 'mode-line-count-face 'mode-line-inactive)))
    :tight t)

  (spaceline-define-segment *evil-substitute
    "Show number of :s matches in real time."
    (when (and (evil-ex-p) (evil-ex-hl-active-p 'evil-ex-substitute))
      (powerline-raw
       (let ((range (if evil-ex-range
                        (cons (car evil-ex-range) (cadr evil-ex-range))
                      (cons (line-beginning-position) (line-end-position))))
             (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
         (if pattern
             (format " %s matches "
                     (count-matches pattern (car range) (cdr range))
                     evil-ex-argument)
           " ... "))
       (if active 'mode-line-count-face 'mode-line-inactive)))
    :tight t)

  (spaceline-define-segment *macro-recording
    "Show when recording macro."
    (when (and active defining-kbd-macro)
      (powerline-raw
       (format " %s ▶ " (char-to-string evil-this-macro))
       highlight-face))
    :tight t)

  (spaceline-define-segment *buffer-encoding-abbrev
    "The line ending convention used in the buffer."
    (unless (string-match-p "\\(utf-8\\|undecided\\)"
                            (symbol-name buffer-file-coding-system))
      (format "%s" buffer-file-coding-system)))

  (spaceline-define-segment *major-mode
    "The major mode, including process, environment and text-scale info."
    (concat (format "%s" mode-name)
            (if (stringp mode-line-process) mode-line-process)
            (if doom--env-version (concat " " doom--env-version))
            (and (featurep 'face-remap)
                 (/= text-scale-mode-amount 0)
                 (format " (%+d)" text-scale-mode-amount))))

  (spaceline-define-segment *selection-info
    "Information about the current selection."
    (when (and active (evil-visual-state-p))
      (powerline-raw
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
       highlight-face))
    :tight t)

  ;; flycheck
  (defface doom-flycheck-error '((t (:inherit error)))
    "Face for flycheck error feedback in the modeline.")
  (defface doom-flycheck-warning '((t (:inherit warning)))
    "Face for flycheck warning feedback in the modeline.")

  (defvar-local doom--flycheck-err-cache nil "")
  (defvar-local doom--flycheck-cache nil "")
  (spaceline-define-segment *flycheck
    "Persistent and cached flycheck indicators in the mode-line."
    (when (and (bound-and-true-p flycheck-mode)
               (or flycheck-current-errors
                   (eq 'running flycheck-last-status-change)))
      (or (and (or (eq doom--flycheck-err-cache doom--flycheck-cache)
                   (memq flycheck-last-status-change '(running not-checked)))
               doom--flycheck-cache)
          (and (setq doom--flycheck-err-cache flycheck-current-errors)
               (setq doom--flycheck-cache
                     (let ((fe (doom/-flycheck-count 'error))
                           (fw (doom/-flycheck-count 'warning)))
                       (concat
                        (if fe (propertize (format " ⚠%s " fe)
                                           'face (if active
                                                     'doom-flycheck-error
                                                   'mode-line)))
                        (if fw (propertize (format " ⚠%s " fw)
                                           'face (if active
                                                     'doom-flycheck-warning
                                                   'mode-line)))))))))
    :tight t)

  (spaceline-define-segment *pad
    "Padding, to ensure the mode-line is `powerline-height' pixels tall"
    (pl/percent-xpm powerline-height 100 0 100 0 3 (if active "#00B3EF") nil)
    :tight t)

  (spaceline-compile
   'main
   '(*pad
     ((*macro-recording *anzu *iedit *evil-substitute *flycheck *selection-info)
      :skip-alternate t
      :tight t)
     *buffer-path)
   '(*vc
     *major-mode
     *env-version
     *buffer-encoding-abbrev
     (global :when active)
     *buffer-position))

  ;;
  (spaceline-define-segment *default-dir
    "Shows default-directory"
    (concat "[" (abbreviate-file-name default-directory) "]")
    :face other-face)

  (spaceline-compile
   'scratch
   '(*pad
     ((*macro-recording *anzu *iedit *evil-substitute *flycheck *selection-info)
      :skip-alternate t
      :tight t)
     *buffer-path
     *default-dir)
   '(*major-mode
     *env-version
     *buffer-encoding-abbrev
     (global :when active)
     *buffer-position))

  ;;
  (spaceline-define-segment *eldoc
    (and (bound-and-true-p str) str)
    :tight t
    :face 'mode-line)

  (spaceline-define-segment *eldoc-pad
    "Padding, to ensure the mode-line is `powerline-height' pixels tall"
    (pl/percent-xpm powerline-height 100 0 100 0 3 "#B3EF00" nil)
    :tight t
    :face 'mode-line)

  (spaceline-compile
   'eldoc '(*eldoc-pad *eldoc) '())

  ;; Initialize modeline
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(provide 'core-ui)
;;; core-ui.el ends here
