;;; core-ui.el -*- lexical-binding: t; -*-

(defvar doom-fringe-size '4
  "Default fringe width.")

(defvar doom-theme nil
  "A symbol representing the color theme to load.")

(defvar doom-font nil
  "The default font to use. Expects a `font-spec'.")

(defvar doom-big-font nil
  "The default large font to use when `doom-big-font-mode' is enabled. Expects a
`font-spec'.")

(defvar doom-variable-pitch-font nil
  "The default font to use for variable-pitch text. Expects a `font-spec'.")

(defvar doom-unicode-font nil
  "Fallback font for unicode glyphs. Is ignored if :feature unicode is active.
Expects a `font-spec'.")

(defvar doom-major-mode-names
  '((sh-mode . "sh")
    (emacs-lisp-mode . "Elisp"))
  "An alist mapping major modes symbols to strings (or functions that will
return a string). This changes the 'long' name of a major-mode, allowing for
shorter major mode name in the mode-line. See `doom|set-mode-name'.")


;; Hook(s)
(defvar doom-init-ui-hook nil
  "List of hooks to run when core-ui is initialized.")

(setq-default
 ansi-color-for-comint-mode t
 bidi-display-reordering nil ; disable bidirectional text for tiny performance boost
 blink-matching-paren nil    ; don't blink--too distracting
 compilation-always-kill t        ; kill compilation process before starting another
 compilation-ask-about-save nil   ; save all buffers on `compile'
 compilation-scroll-output 'first-error
 confirm-nonexistent-file-or-buffer t
 cursor-in-non-selected-windows nil  ; hide cursors in other windows
 display-line-numbers-width 3
 enable-recursive-minibuffers nil
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
 ibuffer-use-other-window t
 resize-mini-windows 'grow-only  ; Minibuffer resizing
 show-help-function nil          ; hide :help-echo text
 split-width-threshold 160       ; favor horizontal splits
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


;;
;; Modeline library
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
  (when-let* ((modeline (doom-modeline key)))
    (setf (if default
              (default-value 'mode-line-format)
            (buffer-local-value 'mode-line-format (current-buffer)))
          modeline)))


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
  (dolist (fn '(all-the-icons-octicon all-the-icons-material
                all-the-icons-faicon all-the-icons-fileicon
                all-the-icons-wicon all-the-icons-alltheicon))
    (advice-add fn :around #'doom*disable-all-the-icons-in-tty)))

(def-package! fringe-helper
  :commands (fringe-helper-define fringe-helper-convert)
  :init
  (unless (fboundp 'define-fringe-bitmap)
    ;; doesn't exist in terminal Emacs; define it to prevent errors
    (defun define-fringe-bitmap (&rest _))))

(def-package! hideshow ; built-in
  :commands (hs-minor-mode hs-toggle-hiding hs-already-hidden-p)
  :config (setq hs-hide-comments-when-hiding-all nil))

(def-package! hide-mode-line
  :commands hide-mode-line-mode
  :init (add-hook 'completion-list-mode-hook #'hide-mode-line-mode))

(def-package! highlight-indentation
  :commands (highlight-indentation-mode highlight-indentation-current-column-mode))

;; For modes with sub-par number fontification
(def-package! highlight-numbers :commands highlight-numbers-mode)

;; Highlights the current line
(def-package! hl-line ; built-in
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
  :config
  ;; I don't need hl-line showing in other windows. This also offers a small
  ;; speed boost when buffer is displayed in multiple windows.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  ;; On Emacs 26+, when point is on the last line, hl-line highlights bleed into
  ;; the rest of the window after eob. This is the fix.
  (when (boundp 'display-line-numbers)
    (defun doom--line-range ()
      (cons (line-beginning-position)
            (cond ((save-excursion
                     (goto-char (line-end-position))
                     (and (eobp) (not (bolp))))
                   (1- (line-end-position)))
                  ((or (eobp) (save-excursion (forward-line) (eobp)))
                   (line-end-position))
                  (t
                   (line-beginning-position 2)))))
    (setq hl-line-range-function #'doom--line-range))

  (after! evil
    (defvar-local doom-buffer-hl-line-mode nil)

    ;; Disable `hl-line' in evil-visual mode (temporarily). `hl-line' can make
    ;; the selection region harder to see while in evil visual mode.
    (defun doom|disable-hl-line ()
      (when hl-line-mode
        (setq doom-buffer-hl-line-mode t)
        (hl-line-mode -1)))
    (defun doom|enable-hl-line-maybe ()
      (if doom-buffer-hl-line-mode (hl-line-mode +1)))

    (add-hook 'evil-visual-state-entry-hook #'doom|disable-hl-line)
    (add-hook 'evil-visual-state-exit-hook  #'doom|enable-hl-line-maybe)))

;; Helps us distinguish stacked delimiter pairs. Especially in parentheses-drunk
;; languages like Lisp.
(def-package! rainbow-delimiters
  :hook (lisp-mode . rainbow-delimiters-mode)
  :config (setq rainbow-delimiters-max-face-count 3))

;; For a distractions-free-like UI, that dynamically resizes margets and can
;; center a buffer.
(def-package! visual-fill-column
  :commands visual-fill-column-mode
  :config
  (setq-default
   visual-fill-column-center-text t
   visual-fill-column-width
   ;; take Emacs 26 line numbers into account
   (+ (if (boundp 'display-line-numbers) 6 0)
      fill-column)))


;;
;; Built-in packages
;;

;; show typed keystrokes in minibuffer
(defun doom|enable-ui-keystrokes ()  (setq echo-keystrokes 0.02))
(defun doom|disable-ui-keystrokes () (setq echo-keystrokes 0))
(doom|enable-ui-keystrokes)
;; ...but hide them while isearch is active
(add-hook 'isearch-mode-hook     #'doom|disable-ui-keystrokes)
(add-hook 'isearch-mode-end-hook #'doom|enable-ui-keystrokes)

;; undo/redo changes to Emacs' window layout
(defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
(autoload 'winner-mode "winner" nil t)
(add-hook 'doom-init-ui-hook #'winner-mode)

;; highlight matching delimiters
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(add-hook 'doom-init-ui-hook #'show-paren-mode)

;;; More reliable inter-window border
;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq-default window-divider-default-places t
              window-divider-default-bottom-width 1
              window-divider-default-right-width 1)
(add-hook 'doom-init-ui-hook #'window-divider-mode)

;; remove prompt if the file is opened in other clients
(defun server-remove-kill-buffer-hook ()
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)


;;
;; Custom hooks
;;

(defvar doom-load-theme-hook nil
  "Hook run when the theme (and font) is initialized (or reloaded
with `doom//reload-theme').")
(define-obsolete-variable-alias 'doom-init-theme-hook 'doom-load-theme-hook "2.1.0")

(defvar doom-before-switch-window-hook nil
  "Hook run before `switch-window' or `switch-frame' are called. See
`doom-after-switch-window-hook'.")

(defvar doom-after-switch-window-hook nil
  "Hook run after `switch-window' or `switch-frame' are called. See
`doom-before-switch-window-hook'.")

(defvar doom-before-switch-buffer-hook nil
  "Hook run before `switch-to-buffer' and `display-buffer' are called. See
`doom-after-switch-buffer-hook'.")

(defvar doom-after-switch-buffer-hook nil
  "Hook run before `switch-to-buffer' and `display-buffer' are called. See
`doom-before-switch-buffer-hook'.")

(defun doom*switch-frame-hooks (orig-fn frame &optional norecord)
  (if (eq frame (selected-frame))
      (funcall orig-fn frame norecord)
    (run-hook-with-args 'doom-before-switch-frame-hook)
    (prog1 (funcall orig-fn frame norecord)
      (run-hook-with-args 'doom-after-switch-frame-hook))))
(defun doom*switch-window-hooks (orig-fn window &optional norecord)
  (if (or (eq window (selected-window))
          (window-minibuffer-p)
          (window-minibuffer-p window))
      (funcall orig-fn window norecord)
    (run-hook-with-args 'doom-before-switch-window-hook)
    (prog1 (funcall orig-fn window norecord)
      (run-hook-with-args 'doom-after-switch-window-hook))))
(defun doom*switch-buffer-hooks (orig-fn &rest args)
  (run-hook-with-args 'doom-before-switch-buffer-hook)
  (prog1 (apply orig-fn args)
    (run-hook-with-args 'doom-after-switch-buffer-hook)))

(advice-add #'select-frame     :around #'doom*switch-frame-hooks)
(advice-add #'select-window    :around #'doom*switch-window-hooks)
(advice-add #'switch-to-buffer :around #'doom*switch-buffer-hooks)
(advice-add #'display-buffer   :around #'doom*switch-buffer-hooks)

(defun doom*load-theme-hooks (&rest _)
  (run-hook-with-args 'doom-load-theme-hook))
(advice-add #'load-theme       :after  #'doom*load-theme-hooks)


;;
;; Silence motion errors in minibuffer
;;

(defun doom*silence-motion-errors (orig-fn &rest args)
  (if (and (minibufferp)
           (<= (point) (minibuffer-prompt-end)))
      (progn
        (ignore-errors (apply orig-fn args))
        (goto-char (minibuffer-prompt-end)))
    (apply orig-fn args)))

(advice-add #'left-char :around #'doom*silence-motion-errors)
(advice-add #'right-char :around #'doom*silence-motion-errors)
(advice-add #'delete-backward-char :around #'doom*silence-motion-errors)
(advice-add #'backward-kill-sentence :around #'doom*silence-motion-errors)


;;
;; Line numbers
;;

(defvar doom-line-numbers-style t
  "The default styles to use for the line number display. Accepts one of the
following:

  nil         No line numbers
  t           Ordinary line numbers
  'relative   Relative line numbers

Use `doom/toggle-line-numbers' to cycle between these line number styles.")

(when (boundp 'display-line-numbers)
  (defvar doom-line-numbers-visual-style nil
    "If non-nil, relative line numbers will be countered by screen line, rather
than buffer line. Setting this to non-nil is the equivalent of using 'visual in
`display-line-numbers'.

It has no effect on nlinum."))

(defun doom|enable-line-numbers (&optional arg)
  "Enables the display of line numbers, using `display-line-numbers' (in Emacs
26+) or `nlinum-mode'.

See `doom-line-numbers-style' to control the style of line numbers to display."
  (cond ((boundp 'display-line-numbers)
         (setq display-line-numbers (unless (eq arg -1) doom-line-numbers-style)))
        ((eq doom-line-numbers-style 'relative)
         (if (eq arg -1)
             (nlinum-relative-off)
           (nlinum-relative-on)))
        ((not (null doom-line-numbers-style))
         (nlinum-mode (or arg +1)))))

(defun doom|disable-line-numbers ()
  "Disable the display of line numbers."
  (doom|enable-line-numbers -1))

;; Emacs 26+ has native line number support.
;; Line number column. A faster (or equivalent, in the worst case) line number
;; plugin than `linum-mode'.
(def-package! nlinum
  :unless (boundp 'display-line-numbers)
  :commands nlinum-mode
  :init
  (defvar doom-line-number-lpad 4
    "How much padding to place before line numbers.")
  (defvar doom-line-number-rpad 1
    "How much padding to place after line numbers.")
  (defvar doom-line-number-pad-char 32
    "Character to use for padding line numbers.

By default, this is a space character. If you use `whitespace-mode' with
`space-mark', the whitespace in line numbers will be affected (this can look
ugly). In this case, you can change this to ?\u2002, which is a unicode
character that looks like a space that `whitespace-mode' won't affect.")
  :config
  (setq nlinum-highlight-current-line t)

  ;; Fix lingering hl-line overlays (caused by nlinum)
  (add-hook! 'hl-line-mode-hook
    (remove-overlays (point-min) (point-max) 'face 'hl-line))

  (defun doom-nlinum-format-fn (line _width)
    "A more customizable `nlinum-format-function'. See `doom-line-number-lpad',
`doom-line-number-rpad' and `doom-line-number-pad-char'. Allows a fix for
`whitespace-mode' space-marks appearing inside the line number."
    (let ((str (number-to-string line)))
      (setq str (concat (make-string (max 0 (- doom-line-number-lpad (length str)))
                                     doom-line-number-pad-char)
                        str
                        (make-string doom-line-number-rpad doom-line-number-pad-char)))
      (put-text-property 0 (length str) 'face
                         (if (and nlinum-highlight-current-line
                                  (= line nlinum--current-line))
                             'nlinum-current-line
                           'linum)
                         str)
      str))
  (setq nlinum-format-function #'doom-nlinum-format-fn)

  (defun doom|init-nlinum-width ()
    "Calculate line number column width beforehand (optimization)."
    (setq nlinum--width
          (length (save-excursion (goto-char (point-max))
                                  (format-mode-line "%l")))))
  (add-hook 'nlinum-mode-hook #'doom|init-nlinum-width))

;; Fixes disappearing line numbers in nlinum and other quirks
(def-package! nlinum-hl
  :unless (boundp 'display-line-numbers)
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

(def-package! nlinum-relative
  :unless (boundp 'display-line-numbers)
  :commands (nlinum-relative-mode nlinum-relative-on nlinum-relative-off)
  :config
  (setq nlinum-format " %d ")
  (after! evil (nlinum-relative-setup-evil)))


;;
;; Theme & font
;;

(defun doom|init-fonts (&optional frame)
  "Initialize fonts."
  (when (fontp doom-font)
    (map-put default-frame-alist 'font (font-xlfd-name doom-font)))
  (or frame (setq frame (selected-frame)))
  (condition-case-unless-debug ex
      (progn
        (when (fontp doom-font)
          (set-face-attribute 'fixed-pitch frame :font doom-font))
        ;; Fallback to `doom-unicode-font' for Unicode characters
        (when (fontp doom-unicode-font)
          (set-fontset-font t 'unicode doom-unicode-font frame))
        ;; ...and for variable-pitch-mode:
        (when (fontp doom-variable-pitch-font)
          (set-face-attribute 'variable-pitch frame :font doom-variable-pitch-font)))
    ('error
     (if (string-prefix-p "Font not available: " (error-message-string ex))
         (lwarn 'doom-ui :warning
                "Could not find the '%s' font on your system, falling back to system font"
                (font-get (caddr ex) :family))
       (lwarn 'doom-ui :error
              "Unexpected error while initializing fonts: %s"
              (error-message-string ex))))))

(defun doom|init-theme ()
  "Set the theme and load the font, in that order."
  (when doom-theme
    (load-theme doom-theme t))
  (add-hook 'after-make-frame-functions #'doom|init-theme-in-frame))

;; Getting themes to remain consistent across GUI Emacs, terminal Emacs and
;; daemon Emacs is hairy. `doom|init-theme' sorts out the initial GUI frame.
;; Attaching `doom|init-theme-in-frame' to `after-make-frame-functions' sorts
;; out daemon and emacsclient frames.
;;
;; There will still be issues with simultaneous gui and terminal (emacsclient)
;; frames, however. There's always `doom//reload-theme' if you need it!
(defun doom|init-theme-in-frame (frame)
  "Reloads the theme in new daemon or tty frames."
  (when (or (daemonp) (not (display-graphic-p)))
    (with-selected-frame frame
      (doom|init-theme))))

(add-hook! 'doom-init-ui-hook #'(doom|init-theme doom|init-fonts))


;;
;; Bootstrap
;;

;; auto-enabled in Emacs 25+; I'll do it myself
(global-eldoc-mode -1)
;; simple name in frame title
(setq frame-title-format '("%b – Doom Emacs"))
;; make `next-buffer', `other-buffer', etc. ignore unreal buffers
(map-put default-frame-alist 'buffer-predicate #'doom-buffer-frame-predicate)
;; draw me like one of your French editors
(tooltip-mode -1) ; relegate tooltips to echo area only
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; standardize default fringe width
(if (fboundp 'fringe-mode) (fringe-mode doom-fringe-size))
;; prompts the user for confirmation when deleting a non-empty frame
(define-key global-map [remap delete-frame] #'doom/delete-frame)

;; a good indicator that Emacs isn't frozen
(add-hook 'doom-init-ui-hook #'blink-cursor-mode)
;; line numbers in most modes
(add-hook! (prog-mode text-mode conf-mode) #'doom|enable-line-numbers)


;; Customized confirmation prompt for quitting Emacs
(defun doom-quit-p (&optional prompt)
  "Return t if this session should be killed. Prompts the user for
confirmation."
  (if (ignore-errors (doom-real-buffer-list))
      (or (yes-or-no-p (format "››› %s" (or prompt "Quit Emacs?")))
          (ignore (message "Aborted")))
    t))
(setq confirm-kill-emacs #'doom-quit-p)

(defun doom|ansi-color-apply ()
  "TODO"
  (ansi-color-apply-on-region compilation-filter-start (point)))

(defun doom|no-fringes-in-minibuffer ()
  "Disable fringes in the minibuffer window."
  (set-window-fringes (minibuffer-window) 0 0 nil))
(add-hook! '(doom-init-ui-hook minibuffer-setup-hook)
  #'doom|no-fringes-in-minibuffer)

(defun doom|set-mode-name ()
  "Set the major mode's `mode-name', as dictated by `doom-major-mode-names'."
  (when-let* ((name (cdr (assq major-mode doom-major-mode-names))))
    (setq mode-name
          (cond ((functionp name) (funcall name))
                ((stringp name) name)
                (t (error "'%s' isn't a valid name for %s" name major-mode))))))

(defun doom|protect-visible-buffers ()
  "Don't kill the current buffer if it is visible in another window (bury it
instead)."
  (not (delq (selected-window) (get-buffer-window-list nil nil t))))

(defun doom|protect-fallback-buffer ()
  "Don't kill the scratch buffer."
  (not (eq (current-buffer) (doom-fallback-buffer))))

(defun doom*switch-to-fallback-buffer-maybe (orig-fn)
  "Advice for `kill-this-buffer'. If in a dedicated window, delete it. If there
are no real buffers left OR if all remaining buffers are visible in other
windows, switch to `doom-fallback-buffer'. Otherwise, delegate to original
`kill-this-buffer'."
  (let ((buf (current-buffer)))
    (cond ((window-dedicated-p)
           (delete-window))
          ((eq buf (doom-fallback-buffer))
           (message "Can't kill the fallback buffer."))
          ((doom-real-buffer-p buf)
           (if (and buffer-file-name
                    (buffer-modified-p buf)
                    (not (y-or-n-p "Buffer %s is modified; kill anyway?")))
               (message "Aborted")
             (set-buffer-modified-p nil)
             (when (or ;; if there aren't more real buffers than visible buffers,
                       ;; then there are no real, non-visible buffers left.
                       (not (cl-set-difference (doom-real-buffer-list)
                                               (doom-visible-buffers)))
                       ;; if we end up back where we start (or previous-buffer
                       ;; returns nil), we have nowhere left to go
                       (memq (previous-buffer) (list buf 'nil)))
               (switch-to-buffer (doom-fallback-buffer)))
             (kill-buffer buf)))
          (t
           (funcall orig-fn)))))


(defun doom|init-ui ()
  "Initialize Doom's user interface by applying all its advice and hooks."
  ;; Switch to `doom-fallback-buffer' if on last real buffer
  (advice-add #'kill-this-buffer :around #'doom*switch-to-fallback-buffer-maybe)
  ;; Don't kill the fallback buffer
  (add-hook 'kill-buffer-query-functions #'doom|protect-fallback-buffer)
  ;; Don't kill buffers that are visible in another window, only bury them
  (add-hook 'kill-buffer-query-functions #'doom|protect-visible-buffers)
  ;; Renames major-modes [pedantry intensifies]
  (add-hook 'after-change-major-mode-hook #'doom|set-mode-name)
  ;; Ensure ansi codes in compilation buffers are replaced
  (add-hook 'compilation-filter-hook #'doom|ansi-color-apply)
  ;;
  (run-hooks 'doom-init-ui-hook))

(add-hook 'doom-init-hook #'doom|init-ui)

(provide 'core-ui)
;;; core-ui.el ends here
