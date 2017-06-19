;;; core-ui.el -*- lexical-binding: t; -*-

(defvar doom-ui-fringe-size '4 "Default fringe width")

(setq-default
 bidi-display-reordering nil ; disable bidirectional text for tiny performance boost
 blink-matching-paren nil    ; don't blink--too distracting
 cursor-in-non-selected-windows nil  ; hide cursors in other windows
 frame-inhibit-implied-resize t
 ;; remove continuation arrow on right fringe
 fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                              fringe-indicator-alist)
 highlight-nonselected-windows nil
 image-animate-loop t
 indicate-buffer-boundaries nil
 indicate-empty-lines nil
 max-mini-window-height 0.3
 mode-line-default-help-echo nil ; disable mode-line mouseovers
 mouse-yank-at-point t           ; middle-click paste at point, not at click
 resize-mini-windows 'grow-only  ; Minibuffer resizing
 show-help-function nil          ; hide :help-echo text
 split-width-threshold nil       ; favor horizontal splits
 uniquify-buffer-name-style 'forward
 use-dialog-box nil              ; always avoid GUI
 visible-cursor nil
 x-stretch-cursor nil
 ;; defer jit font locking slightly to [try to] improve Emacs performance
 jit-lock-defer-time nil
 jit-lock-stealth-nice 0.1
 jit-lock-stealth-time 0.2
 jit-lock-stealth-verbose nil
 ;; `pos-tip' defaults
 pos-tip-internal-border-width 6
 pos-tip-border-width 1
 ;; no beeping or blinking please
 ring-bell-function #'ignore
 visible-bell nil)

(fset #'yes-or-no-p #'y-or-n-p) ; y/n instead of yes/no

(defun doom-quit-p (&optional prompt)
  "Return t if this session should be killed, but not before it prompts the user
for confirmation."
  (interactive)
  (if (ignore-errors (doom-real-buffers-list))
      (or (yes-or-no-p (format "››› %s" (or prompt "Quit Emacs?")))
          (ignore (message "Aborted")))
    t))
(setq confirm-kill-emacs nil)
(add-hook 'kill-emacs-query-functions #'doom-quit-p)

;; show typed keystrokes in minibuffer
(defun doom|enable-ui-keystrokes ()  (setq echo-keystrokes 0.02))
(defun doom|disable-ui-keystrokes () (setq echo-keystrokes 0))
(doom|enable-ui-keystrokes)
;; ...but hide them while isearch is active
(add-hook 'isearch-mode-hook     #'doom|disable-ui-keystrokes)
(add-hook 'isearch-mode-end-hook #'doom|enable-ui-keystrokes)

;; A minor mode for toggling the mode-line
(defvar-local doom--modeline-format nil
  "The modeline format to use when `doom-hide-modeline-mode' is active. Don't
set this directly. Let-bind it instead.")
(defvar-local doom--old-modeline-format nil
  "The old modeline format, so `doom-hide-modeline-mode' can revert when it's
disabled.")
(define-minor-mode doom-hide-modeline-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  (if doom-hide-modeline-mode
      (setq doom--old-modeline-format mode-line-format
            mode-line-format doom--modeline-format)
    (setq mode-line-format doom--old-modeline-format
          doom--old-modeline-format nil))
  (force-mode-line-update))
;; Ensure major-mode or theme changes don't overwrite these variables
(put 'doom--modeline-format 'permanent-local t)
(put 'doom--old-modeline-format 'permanent-local t)
(put 'doom-hide-modeline-mode 'permanent-local t)

(defun doom|hide-modeline-mode-reset ()
  "Sometimes, a major-mode is activated after `doom-hide-modeline-mode' is
activated, thus disabling it (because changing major modes invokes
`kill-all-local-variables' and specifically seems to kill `mode-line-format's
local value, whether or not it's permanent-local. Therefore, we cycle
`doom-hide-modeline-mode' to fix this."
  (when doom-hide-modeline-mode
    (doom-hide-modeline-mode -1)
    (doom-hide-modeline-mode +1)))
(add-hook 'after-change-major-mode-hook #'doom|hide-modeline-mode-reset)

;; no modeline in completion popups
(add-hook 'completion-list-mode-hook #'doom-hide-modeline-mode)

;; undo/redo changes to Emacs' window layout
(defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
(autoload 'winner-mode "winner" nil t)
(add-hook 'doom-init-hook #'winner-mode)

;; highlight matching delimiters
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(add-hook 'doom-init-hook #'show-paren-mode)

;;; More reliable inter-window border
;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq-default window-divider-default-places t
              window-divider-default-bottom-width 1
              window-divider-default-right-width 1)
(add-hook 'doom-init-hook #'window-divider-mode)

;; like diminish, but for major-modes. [pedantry intensifies]
(defvar doom-ui-mode-names
  '((sh-mode . "sh")
    (emacs-lisp-mode "Elisp"))
  "An alist mapping major modes to alternative names, which will be set when the
mode is detected.")

(defun doom|set-mode-name ()
  "Set the major mode's `mode-name', as dictated by `doom-ui-mode-names'."
  (let ((name (assq major-mode doom-ui-mode-names)))
    (if name (setq mode-name (cdr name)))))
(add-hook 'after-change-major-mode-hook #'doom|set-mode-name)


;;
;; Bootstrap
;;

;; smoother startup when mode-line is invisible
(setq mode-line-format nil)

;; Prompts the user for confirmation when deleting a non-empty frame
(define-key global-map [remap delete-frame] #'doom/delete-frame)

(global-eldoc-mode -1) ; auto-enabled in Emacs 25+; I'll do it myself
(blink-cursor-mode +1) ; a good indicator that Emacs isn't frozen

;; draw me like one of your French editors
(tooltip-mode -1) ; relegate tooltips to echo area only
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; buffer name in frame title
(setq-default frame-title-format '("DOOM Emacs"))

;; standardize fringe width
(push (cons 'left-fringe  doom-ui-fringe-size) default-frame-alist)
(push (cons 'right-fringe doom-ui-fringe-size) default-frame-alist)

;; no fringe in the minibuffer
(defun doom|no-fringes-in-minibuffer ()
  (set-window-fringes (minibuffer-window) 0 0 nil))
(add-hook! '(doom-init-hook minibuffer-setup-hook)
  #'doom|no-fringes-in-minibuffer)


;;
;; Plugins
;;

(def-package! all-the-icons
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
             all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon
             all-the-icons-install-fonts)
  :init
  (defun doom*disable-all-the-icons-in-tty (orig-fn &rest args)
    (when (display-graphic-p)
      (apply orig-fn args)))

  ;; all-the-icons doesn't work in the terminal, so we "disable" it.
  (advice-add #'all-the-icons-octicon    :around #'doom*disable-all-the-icons-in-tty)
  (advice-add #'all-the-icons-material   :around #'doom*disable-all-the-icons-in-tty)
  (advice-add #'all-the-icons-faicon     :around #'doom*disable-all-the-icons-in-tty)
  (advice-add #'all-the-icons-fileicon   :around #'doom*disable-all-the-icons-in-tty)
  (advice-add #'all-the-icons-wicon      :around #'doom*disable-all-the-icons-in-tty)
  (advice-add #'all-the-icons-alltheicon :around #'doom*disable-all-the-icons-in-tty))

(def-package! fringe-helper
  :commands (fringe-helper-define fringe-helper-convert)
  :init
  (unless (fboundp 'define-fringe-bitmap)
    (fset 'define-fringe-bitmap (lambda (&rest _)))))

(def-package! hideshow ; built-in
  :commands (hs-minor-mode hs-toggle-hiding hs-already-hidden-p)
  :config
  (setq hs-hide-comments-when-hiding-all nil))

;; Show uninterrupted indentation markers with some whitespace voodoo.
(def-package! highlight-indentation
  :commands (highlight-indentation-mode highlight-indentation-current-column-mode)
  :config
  (defun doom|inject-trailing-whitespace (start end)
    "The opposite of `delete-trailing-whitespace'. Injects whitespace into
buffer so that `highlight-indentation-mode' will display uninterrupted indent
markers. This whitespace is stripped out on save, as not to affect the resulting
file."
    (interactive (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list (point-min) (point-max))))
    (barf-if-buffer-read-only)
    (unless indent-tabs-mode
      (with-silent-modifications
        (save-excursion
          (goto-char start)
          (save-match-data
            (while (and (re-search-forward "^$" nil t) (<= (point) end))
              (let (line-start line-end next-start next-end)
                (save-excursion
                  ;; Check previous line indent
                  (forward-line -1)
                  (setq line-start (point)
                        line-end (save-excursion (back-to-indentation) (point)))
                  ;; Check next line indent
                  (forward-line 2)
                  (setq next-start (point)
                        next-end (save-excursion (back-to-indentation) (point)))
                  ;; Back to origin
                  (forward-line -1)
                  ;; Adjust indent
                  (let* ((line-indent (- line-end line-start))
                         (next-indent (- next-end next-start))
                         (indent (min line-indent next-indent)))
                    (insert (make-string (if (zerop indent) 0 (1+ indent)) ? )))))
              (forward-line 1))))))
    nil)

  (defun doom|init-highlight-indentation ()
    (unless (or indent-tabs-mode buffer-read-only)
      (if (or highlight-indentation-mode highlight-indentation-current-column-mode)
          (progn
            (doom|inject-trailing-whitespace)
            (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
            (add-hook 'after-save-hook #'doom|inject-trailing-whitespace nil t))
        (remove-hook 'before-save-hook #'delete-trailing-whitespace t)
        (remove-hook 'after-save-hook #'doom|inject-trailing-whitespace t)
        (with-silent-modifications
          (delete-trailing-whitespace)))))
  (add-hook! (highlight-indentation-mode highlight-indentation-current-column-mode)
    #'doom|init-highlight-indentation))

;; For modes that don't adequately highlight numbers
(def-package! highlight-numbers :commands highlight-numbers-mode)

;; Line highlighting
(def-package! hl-line ; built-in
  :init
  (add-hook! (linum-mode nlinum-mode) #'hl-line-mode)
  :config
  ;; I don't need hl-line showing in other windows. This also offers a small
  ;; speed boost when buffer is displayed in multiple windows.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  ;; Fix lingering hl-line overlays
  (add-hook! 'hl-line-mode-hook
    (remove-overlays (point-min) (point-max) 'face 'hl-line))

  (after! evil
    ;; Can get in the way of the selection region when in evil visual mode, so
    ;; disable it temporarily.
    (defun doom|turn-off-hl-line () (hl-line-mode -1))

    (add-hook! 'hl-line-mode-hook
      (cond (hl-line-mode
             (add-hook 'evil-visual-state-entry-hook #'doom|turn-off-hl-line nil t)
             (add-hook 'evil-visual-state-exit-hook #'hl-line-mode nil t))
            (t
             (remove-hook 'evil-visual-state-entry-hook #'doom|turn-off-hl-line t)
             (remove-hook 'evil-visual-state-exit-hook #'hl-line-mode t))))))

;; Line number column. A faster (or equivalent, in the worst case) line number
;; plugin than the built-in `linum'.
(def-package! nlinum
  :commands nlinum-mode
  :preface
  (defvar doom-ui-nlinum-lpad 4)
  (defvar doom-ui-nlinum-rpad 1)
  (defvar doom-ui-nlinum-spacer ?\u2002)
  :init
  (defun doom|init-nlinum-mode ()
    (unless (eq major-mode 'org-mode)
      (nlinum-mode +1)))
  (add-hook! (prog-mode text-mode) #'doom|init-nlinum-mode)
  :config
  (setq nlinum-highlight-current-line t)

  (defun doom-nlinum-format-fn (line _width)
    "A more customizable `nlinum-format-function'. See `doom-ui-nlinum-lpad',
`doom-ui-nlinum-rpad' and `doom-ui-nlinum-spacer'. Allows a fix for
`whitespace-mode' space-marks appearing inside the line number."
    (let ((str (number-to-string line)))
      (setq str (concat (make-string (max 0 (- doom-ui-nlinum-lpad (length str)))
                                     doom-ui-nlinum-spacer)
                        str
                        (make-string doom-ui-nlinum-rpad doom-ui-nlinum-spacer)))
      (put-text-property 0 (length str) 'face
                         (if (and nlinum-highlight-current-line
                                  (= line nlinum--current-line))
                             'nlinum-current-line
                           'linum)
                         str)
      str))
  (setq nlinum-format-function #'doom-nlinum-format-fn)

  ;; Optimization: calculate line number column width beforehand
  (defun doom|init-nlinum-width ()
    (setq nlinum--width
          (length (save-excursion (goto-char (point-max))
                                  (format-mode-line "%l")))))
  (add-hook 'nlinum-mode-hook #'doom|init-nlinum-width))

;; Fixes disappearing line numbers in nlinum and other quirks
(def-package! nlinum-hl
  :load-path "~/work/plugins/emacs-nlinum-hl/"
  :after nlinum
  :config
  ;; With `markdown-fontify-code-blocks-natively' enabled in `markdown-mode',
  ;; line numbers tend to vanish next to code blocks.
  (advice-add #'markdown-fontify-code-block-natively
              :after #'nlinum-hl-do-markdown-fontify-region)

  ;; When using `web-mode's code-folding an entire range of line numbers will
  ;; vanish in the affected area.
  (advice-add #'web-mode-fold-or-unfold :after #'nlinum-hl-do-generic-flush)

  ;; Changing fonts can leave nlinum line numbers in their original size; this
  ;; forces them to resize.
  (advice-add #'set-frame-font :after #'nlinum-hl-flush-all-windows))

;; Helps us distinguish stacked delimiter pairs. Especially in parentheses-drunk
;; languages like Lisp.
(def-package! rainbow-delimiters
  :commands rainbow-delimiters-mode
  :config (setq rainbow-delimiters-max-face-count 3)
  :init (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

;; indicators for empty lines past EOF
(def-package! vi-tilde-fringe
  :commands global-vi-tilde-fringe-mode
  :init (add-hook 'doom-init-hook #'global-vi-tilde-fringe-mode))

;; For a distractions-free-like UI, that dynamically resizes margets and can
;; center a buffer.
(def-package! visual-fill-column
  :commands visual-fill-column-mode
  :config
  (setq-default visual-fill-column-center-text nil
                visual-fill-column-width fill-column))


;;
;; Modeline
;;

(defmacro def-modeline-segment! (name &rest forms)
  "Defines a modeline segment and byte compiles it."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "doom-modeline-segment--%s" name))))
    `(progn
       (defun ,sym () ,@forms)
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(defsubst doom--prepare-modeline-segments (segments)
  (cl-loop for seg in segments
           if (stringp seg)
            collect seg
           else
            collect (list (intern (format "doom-modeline-segment--%s" (symbol-name seg))))))

(defmacro def-modeline! (name lhs &optional rhs)
  "Defines a modeline format and byte-compiles it. NAME is a symbol to identify
it (used by `doom-modeline' for retrieval). LHS and RHS are lists of symbols of
modeline segments defined with `def-modeline-segment!'.

Example:
  (def-modeline! minimal
    (bar matches \" \" buffer-info)
    (media-info major-mode))
  (doom-set-modeline 'minimal t)"
  (let ((sym (intern (format "doom-modeline-format--%s" name)))
        (lhs-forms (doom--prepare-modeline-segments lhs))
        (rhs-forms (doom--prepare-modeline-segments rhs)))
    `(progn
       (defun ,sym ()
         (let ((lhs (list ,@lhs-forms))
               (rhs (list ,@rhs-forms)))
           (let ((rhs-str (format-mode-line rhs)))
             (list lhs
                   (propertize
                    " " 'display
                    `((space :align-to (- (+ right right-fringe right-margin)
                                          ,(+ 1 (string-width rhs-str))))))
                   rhs-str))))
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(defun doom-modeline (key)
  "Returns a mode-line configuration associated with KEY (a symbol). Throws an
error if it doesn't exist."
  (let ((fn (intern (format "doom-modeline-format--%s" key))))
    (when (functionp fn)
      `(:eval (,fn)))))

(defun doom-set-modeline (key &optional default)
  "Set the modeline format. Does nothing if the modeline KEY doesn't exist. If
DEFAULT is non-nil, set the default mode-line for all buffers."
  (let ((modeline (doom-modeline key)))
    (when modeline
      (setf (if default
                (default-value 'mode-line-format)
              (buffer-local-value 'mode-line-format (current-buffer)))
            modeline))))

(provide 'core-ui)
;;; core-ui.el ends here
