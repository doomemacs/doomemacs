;;; core-ui.el -*- lexical-binding: t; -*-

(defvar doom-fringe-size '4
  "Default fringe width.")

(defvar doom-theme nil
  "A symbol representing the color theme to load.")

(defvar doom-font nil
  "The default font to use. Expects a FONT-SPEC (`font-spec').")

(defvar doom-big-font nil
  "The default font to use. Expects a FONT-SPEC (`font-spec').")

(defvar doom-variable-pitch-font nil
  "The default font to use for variable-pitch text. Expects a FONT-SPEC (`font-spec').")

(defvar doom-unicode-font nil
  "Fallback font for unicode glyphs. Is ignored if :feature unicode is active.")

(defvar doom-major-mode-names
  '((sh-mode . "sh")
    (emacs-lisp-mode . "Elisp"))
  "An alist mapping major modes symbols to strings (or functions that will
return a string). This changes the 'long' name of a major-mode, allowing for
shorter major mode name in the mode-line. See `doom|set-mode-name'.")


;; Hook(s)
(defvar doom-init-ui-hook nil
  "List of hooks to run when the theme and font is initialized (or reloaded with
`doom/reload-theme').")


;; Settings
(def-setting! :theme (theme)
  `(unless doom-theme
     (setq doom-theme ,theme)))

(def-setting! :font (family &rest spec)
  `(unless doom-font
     (setq doom-font (font-spec :family ,family ,@spec))))

(def-setting! :variable-pitch-font (family &rest spec)
  `(unless doom-variable-pitch-font
     (setq doom-variable-pitch-font (font-spec :family ,family ,@spec))))

(def-setting! :big-font (family &rest spec)
  `(unless doom-big-font
     (setq doom-big-font (font-spec :family ,family ,@spec))))

(def-setting! :unicode-font (family &rest spec)
  `(unless doom-unicode-font
     (setq doom-unicode-font (font-spec :family ,family ,@spec))))


(setq-default
 bidi-display-reordering nil ; disable bidirectional text for tiny performance boost
 blink-matching-paren nil    ; don't blink--too distracting
 cursor-in-non-selected-windows nil  ; hide cursors in other windows
 display-line-numbers-width 3
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

(defun doom-quit-p (&optional prompt)
  "Return t if this session should be killed. Prompts the user for
confirmation."
  (if (ignore-errors (doom-real-buffer-list))
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
(add-hook 'doom-init-ui-hook #'show-paren-mode)

;;; More reliable inter-window border
;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq-default window-divider-default-places t
              window-divider-default-bottom-width 0
              window-divider-default-right-width 1)
(add-hook 'doom-init-ui-hook #'window-divider-mode)

;; like diminish, but for major-modes. [pedantry intensifies]
(defun doom|set-mode-name ()
  "Set the major mode's `mode-name', as dictated by `doom-major-mode-names'."
  (when-let (name (cdr (assq major-mode doom-major-mode-names)))
    (setq mode-name
          (cond ((functionp name)
                 (funcall name))
                ((stringp name)
                 name)
                (t
                 (error "'%s' isn't a valid name for %s" name major-mode))))))
(add-hook 'after-change-major-mode-hook #'doom|set-mode-name)


;;
;; Themes & fonts
;;

;; Getting themes to remain consistent across GUI Emacs, terminal Emacs and
;; daemon Emacs is hairy.
;;
;; + Running `+doom|init-ui' directly sorts out the initial GUI frame.
;; + Attaching it to `after-make-frame-functions' sorts out daemon Emacs.
;; + Waiting for 0.1s in `doom|reload-ui-in-daemon' fixes daemon Emacs started
;;   with `server-start' in an interactive session of Emacs AND in tty Emacs.
(defun doom|init-ui (&optional frame)
  "Set the theme and load the font, in that order."
  (when doom-theme
    (load-theme doom-theme t))
  (when (display-graphic-p)
    (with-demoted-errors "FONT ERROR: %s"
      (when (fontp doom-font)
        (set-frame-font doom-font nil (if frame (list frame) t)))
      ;; Fallback to `doom-unicode-font' for Unicode characters
      (when (fontp doom-unicode-font)
        (set-fontset-font t 'unicode doom-unicode-font frame))
      ;; ...and for variable-pitch-mode:
      (when (fontp doom-variable-pitch-font)
        (set-face-attribute 'variable-pitch frame :font doom-variable-pitch-font))))
  (run-hooks 'doom-init-ui-hook))

(defun doom|reload-ui-in-daemon (frame)
  "Reload the theme (and font) in an daemon frame."
  (when (or (daemonp) (not (display-graphic-p)))
    (with-selected-frame frame
      (run-with-timer 0.1 nil #'doom|init-ui))))

;; register UI init hooks
(add-hook 'doom-post-init-hook #'doom|init-ui)
(add-hook! 'after-make-frame-functions #'(doom|init-ui doom|reload-ui-in-daemon))


;;
;; Bootstrap
;;

;; prompts the user for confirmation when deleting a non-empty frame
(define-key global-map [remap delete-frame] #'doom/delete-frame)
;; buffer name in frame title
(setq-default frame-title-format '("DOOM Emacs"))
;; auto-enabled in Emacs 25+; I'll do it myself
(global-eldoc-mode -1)
;; a good indicator that Emacs isn't frozen
(add-hook 'doom-post-init-hook #'blink-cursor-mode)
;; standardize default fringe width
(if (fboundp 'fringe-mode) (fringe-mode doom-fringe-size))
;; draw me like one of your French editors
(tooltip-mode -1) ; relegate tooltips to echo area only
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(defun doom|no-fringes-in-minibuffer ()
  "Disable fringes in the minibuffer window."
  (set-window-fringes (minibuffer-window) 0 0 nil))
(add-hook! '(doom-post-init-hook minibuffer-setup-hook)
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
    (defun define-fringe-bitmap (&rest _))))

(def-package! hideshow ; built-in
  :commands (hs-minor-mode hs-toggle-hiding hs-already-hidden-p)
  :config
  (setq hs-hide-comments-when-hiding-all nil))

(def-package! highlight-indentation
  :commands (highlight-indentation-mode highlight-indentation-current-column-mode))

;; For modes with sub-par number fontification
(def-package! highlight-numbers :commands highlight-numbers-mode)

;; Highlights the current line
(def-package! hl-line ; built-in
  :init (add-hook! (prog-mode text-mode conf-mode) #'hl-line-mode)
  :config
  ;; I don't need hl-line showing in other windows. This also offers a small
  ;; speed boost when buffer is displayed in multiple windows.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  (after! evil
    ;; Disable `hl-line' in evil-visual mode (temporarily). `hl-line' can make
    ;; the selection region harder to see while in evil visual mode.
    (defun doom|disable-hl-line () (hl-line-mode -1))

    (add-hook 'evil-visual-state-entry-hook #'doom|disable-hl-line)
    (add-hook 'evil-visual-state-exit-hook #'hl-line-mode)))

;; Helps us distinguish stacked delimiter pairs. Especially in parentheses-drunk
;; languages like Lisp.
(def-package! rainbow-delimiters
  :commands rainbow-delimiters-mode
  :config (setq rainbow-delimiters-max-face-count 3)
  :init (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

;; indicators for empty lines past EOF
(def-package! vi-tilde-fringe
  :commands (global-vi-tilde-fringe-mode vi-tilde-fringe-mode)
  :init
  (add-hook 'doom-init-ui-hook #'global-vi-tilde-fringe-mode)
  (defun doom|disable-vi-tilde-fringe () (vi-tilde-fringe-mode -1)))

;; For a distractions-free-like UI, that dynamically resizes margets and can
;; center a buffer.
(def-package! visual-fill-column
  :commands visual-fill-column-mode
  :config
  (setq-default visual-fill-column-center-text nil
                visual-fill-column-width fill-column))


;;
;; Line numbers
;;

(defvar doom-line-numbers-style t
  "The style to use for the line number display.

Accepts the same arguments as `display-line-numbers', which are:

nil         No line numbers
t           Ordinary line numbers
'relative   Relative line numbers")

(defun doom|enable-line-numbers (&optional arg)
  "Enables the display of line numbers, using `display-line-numbers' (in Emacs
26+) or `nlinum-mode'.

See `doom-line-numbers-style' to control the style of line numbers to display."
  (cond ((boundp 'display-line-numbers)
         (setq display-line-numbers
               (pcase arg
                 (+1 doom-line-numbers-style)
                 (-1 nil)
                 (_ doom-line-numbers-style))))
        ((eq doom-line-numbers-style 'relative)
         (if (= arg -1)
             (nlinum-relative-off)
           (nlinum-relative-on)))
        ((not (null doom-line-numbers-style))
         (nlinum-mode (or arg +1)))))

(defun doom|disable-line-numbers ()
  "Disable the display of line numbers."
  (doom|enable-line-numbers -1))

(add-hook! (prog-mode text-mode conf-mode) #'doom|enable-line-numbers)

;; Emacs 26+ has native line number support.
(unless (boundp 'display-line-numbers)
  ;; Line number column. A faster (or equivalent, in the worst case) line number
  ;; plugin than `linum-mode'.
  (def-package! nlinum
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
    :commands nlinum-relative-mode
    :config
    (after! evil
      (nlinum-relative-setup-evil))))


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
  (when-let (modeline (doom-modeline key))
    (setf (if default
              (default-value 'mode-line-format)
            (buffer-local-value 'mode-line-format (current-buffer)))
          modeline)))

(provide 'core-ui)
;;; core-ui.el ends here
