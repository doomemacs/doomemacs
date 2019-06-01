;;; core-ui.el -*- lexical-binding: t; -*-

;;
;;; Variables

(defvar doom-theme nil
  "A symbol representing the Emacs theme to load at startup.

This is changed by `load-theme'.")

(defvar doom-font nil
  "The default font to use.

Expects either a `font-spec', font object, an XFT font string or an XLFD font
string.

This affects the `default' and `fixed-pitch' faces.

Examples:
  (setq doom-font (font-spec :family \"Fira Mono\" :size 12))
  (setq doom-font \"Terminus (TTF):pixelsize=12:antialias=off\")")

(defvar doom-variable-pitch-font nil
  "The font to use for variable-pitch text.

Expects either a `font-spec', font object, a XFT font string or XLFD string. See
`doom-font' for examples.

It is recommended you don't set specify a font-size, as to inherit `doom-font's
size.")

(defvar doom-serif-font nil
  "The default font to use for the `fixed-pitch-serif' face.

Expects either a `font-spec', font object, a XFT font string or XLFD string. See
`doom-font' for examples.

It is recommended you don't set specify a font-size, as to inherit `doom-font's
size.")

(defvar doom-unicode-font
  (if IS-MAC
      (font-spec :family "Apple Color Emoji")
    (font-spec :family "Symbola"))
  "Fallback font for unicode glyphs.

It defaults to Apple Color Emoji on MacOS and Symbola on Linux. Expects either a
`font-spec', font object, a XFT font string or XLFD string. See `doom-font' for
examples.

It is recommended you don't set specify a font-size, as to inherit `doom-font's
size.")

(defvar doom--prefer-theme-elc nil
  "If non-nil, `load-theme' will prefer the compiled theme (unlike its default
behavior). Do not set this directly, this is let-bound in `doom|init-theme'.")


;;
;;; Custom hooks

(defvar doom-init-ui-hook nil
  "List of hooks to run when the UI has been initialized.")

(defvar doom-load-theme-hook nil
  "Hook run after the theme is loaded with `load-theme' or reloaded with
`doom/reload-theme'.")

(defvar doom-switch-buffer-hook nil
  "A list of hooks run after changing the current buffer.")

(defvar doom-switch-window-hook nil
  "A list of hooks run after changing the focused windows.")

(defvar doom-switch-frame-hook nil
  "A list of hooks run after changing the focused frame.")

(defvar doom-inhibit-switch-buffer-hooks nil
  "Letvar for inhibiting `doom-switch-buffer-hook'. Do not set this directly.")
(defvar doom-inhibit-switch-window-hooks nil
  "Letvar for inhibiting `doom-switch-window-hook'. Do not set this directly.")
(defvar doom-inhibit-switch-frame-hooks nil
  "Letvar for inhibiting `doom-switch-frame-hook'. Do not set this directly.")

(defvar doom--last-window nil)
(defvar doom--last-frame nil)

(defun doom|run-switch-window-hooks ()
  (let ((gc-cons-threshold doom-gc-cons-upper-limit))
    (unless (or doom-inhibit-switch-window-hooks
                (eq doom--last-window (selected-window))
                (minibufferp))
      (let ((doom-inhibit-switch-window-hooks t))
        (run-hooks 'doom-switch-window-hook)
        (setq doom--last-window (selected-window))))))

(defun doom|run-switch-frame-hooks (&rest _)
  (unless (or doom-inhibit-switch-frame-hooks
              (eq doom--last-frame (selected-frame))
              (frame-parameter nil 'parent-frame))
    (let ((doom-inhibit-switch-frame-hooks t))
      (run-hooks 'doom-switch-frame-hook)
      (setq doom--last-frame (selected-frame)))))

(defun doom*run-switch-buffer-hooks (orig-fn buffer-or-name &rest args)
  (let ((gc-cons-threshold doom-gc-cons-upper-limit))
    (if (or doom-inhibit-switch-buffer-hooks
            (eq (current-buffer) (get-buffer buffer-or-name))
            (and (eq orig-fn #'switch-to-buffer) (car args)))
        (apply orig-fn buffer-or-name args)
      (let ((doom-inhibit-switch-buffer-hooks t))
        (when-let* ((buffer (apply orig-fn buffer-or-name args)))
          (with-current-buffer (if (windowp buffer)
                                   (window-buffer buffer)
                                 buffer)
            (run-hooks 'doom-switch-buffer-hook))
          buffer)))))

(defun doom*run-switch-to-next-prev-buffer-hooks (orig-fn &rest args)
  (let ((gc-cons-threshold doom-gc-cons-upper-limit))
    (if doom-inhibit-switch-buffer-hooks
        (apply orig-fn args)
      (let ((doom-inhibit-switch-buffer-hooks t))
        (when-let* ((buffer (apply orig-fn args)))
          (with-current-buffer buffer
            (run-hooks 'doom-switch-buffer-hook))
          buffer)))))

(defun doom*run-load-theme-hooks (theme &optional _no-confirm no-enable)
  "Set up `doom-load-theme-hook' to run after `load-theme' is called."
  (unless no-enable
    (setq doom-theme theme)
    (run-hooks 'doom-load-theme-hook)))

(defun doom|protect-fallback-buffer ()
  "Don't kill the scratch buffer. Meant for `kill-buffer-query-functions'."
  (not (eq (current-buffer) (doom-fallback-buffer))))

(defun doom|highlight-non-default-indentation ()
  "Highlight whitespace that doesn't match your `indent-tabs-mode' setting.

e.g. If you indent with spaces by default, tabs will be highlighted. If you
indent with tabs, spaces at BOL are highlighted.

Does nothing if `whitespace-mode' is already active or the current buffer is
read-only or not file-visiting."
  (unless (or (bound-and-true-p global-whitespace-mode)
              (bound-and-true-p whitespace-mode)
              (eq major-mode 'fundamental-mode)
              buffer-read-only
              (null buffer-file-name))
    (require 'whitespace)
    (set (make-local-variable 'whitespace-style)
         (if (bound-and-true-p whitespace-newline-mode)
             (cl-union (if indent-tabs-mode '(indentation) '(tabs tab-mark))
                       whitespace-style)
           `(face ,@(if indent-tabs-mode '(indentation) '(tabs tab-mark)))))
    (whitespace-mode +1)))


;;
;;; General configuration

(setq-default
 ansi-color-for-comint-mode t
 bidi-display-reordering nil ; disable bidirectional text for tiny performance boost
 blink-matching-paren nil    ; don't blink--too distracting
 compilation-always-kill t        ; kill compilation process before starting another
 compilation-ask-about-save nil   ; save all buffers on `compile'
 compilation-scroll-output 'first-error
 confirm-nonexistent-file-or-buffer t
 confirm-kill-emacs #'doom-quit-p   ; custom confirmation when killing Emacs
 cursor-in-non-selected-windows nil ; hide cursors in other windows
 custom-theme-directory (expand-file-name "themes/" doom-private-dir)
 display-line-numbers-width 3
 echo-keystrokes 0.02
 enable-recursive-minibuffers nil
 frame-inhibit-implied-resize t
 frame-title-format '("%b – Doom Emacs") ; simple name in frame title
 ;; remove continuation arrow on right fringe
 fringe-indicator-alist
 (delq (assq 'continuation fringe-indicator-alist)
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
 uniquify-buffer-name-style nil  ; custom modeline will show file paths anyway
 use-dialog-box nil              ; always avoid GUI
 visible-cursor nil
 x-stretch-cursor nil
 ;; Favor vertical splits
 split-width-threshold 160
 split-height-threshold nil
 ;; `pos-tip' defaults
 pos-tip-internal-border-width 6
 pos-tip-border-width 1
 ;; no beeping or blinking please
 ring-bell-function #'ignore
 visible-bell nil
 ;; don't resize emacs in steps, it looks weird
 window-resize-pixelwise t
 frame-resize-pixelwise t)
;; y/n instead of yes/no
(fset #'yes-or-no-p #'y-or-n-p)
;; Truly silence startup message
(fset #'display-startup-echo-area-message #'ignore)
;; relegate tooltips to echo area only
(if (bound-and-true-p tooltip-mode) (tooltip-mode -1))
;; enabled by default; no thanks, too distracting
(blink-cursor-mode -1)
;; Handle ansi codes in compilation buffer
(add-hook 'compilation-filter-hook #'doom|apply-ansi-color-to-compilation-buffer)
;; Make `next-buffer', `other-buffer', etc. ignore unreal buffers.
(add-to-list 'default-frame-alist '(buffer-predicate . doom-buffer-frame-predicate))
;; Prevent the glimpse of un-styled Emacs by setting these early.
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
;; prompts the user for confirmation when deleting a non-empty frame
(global-set-key [remap delete-frame] #'doom/delete-frame)

;; Use `show-trailing-whitespace' instead of `whitespace-mode' because it's
;; faster (implemented in C). But try to only enable it in editing buffers.
(setq-default show-trailing-whitespace nil)
(setq-hook! '(prog-mode-hook text-mode-hook conf-mode-hook) show-trailing-whitespace t)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq-default window-divider-default-places t
              window-divider-default-bottom-width 1
              window-divider-default-right-width 1)
(add-hook 'doom-init-ui-hook #'window-divider-mode)


;;
;;; Built-in packages

(def-package! ediff
  :defer t
  :init
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)
  :config
  (defvar doom--ediff-saved-wconf nil)
  ;; Restore window config after quitting ediff
  (defun doom|ediff-save-wconf ()
    (setq doom--ediff-saved-wconf (current-window-configuration)))
  (add-hook 'ediff-before-setup-hook #'doom|ediff-save-wconf)

  (defun doom|ediff-restore-wconf ()
    (when (window-configuration-p doom--ediff-saved-wconf)
      (set-window-configuration doom--ediff-saved-wconf)))
  (add-hook 'ediff-quit-hook #'doom|ediff-restore-wconf 'append)
  (add-hook 'ediff-suspend-hook #'doom|ediff-restore-wconf 'append))


(def-package! hl-line
  ;; Highlights the current line
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
  :config
  ;; I don't need hl-line showing in other windows. This also offers a small
  ;; speed boost when buffer is displayed in multiple windows.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  ;; Disable `hl-line' in evil-visual mode (temporarily). `hl-line' can make the
  ;; selection region harder to see while in evil visual mode.
  (after! evil
    (defvar doom-buffer-hl-line-mode nil)

    (defun doom|disable-hl-line ()
      (when hl-line-mode
        (setq-local doom-buffer-hl-line-mode t)
        (hl-line-mode -1)))
    (add-hook 'evil-visual-state-entry-hook #'doom|disable-hl-line)

    (defun doom|enable-hl-line-maybe ()
      (when doom-buffer-hl-line-mode
        (hl-line-mode +1)))
    (add-hook 'evil-visual-state-exit-hook  #'doom|enable-hl-line-maybe)))


(def-package! winner
  ;; undo/redo changes to Emacs' window layout
  :after-call (after-find-file doom-switch-window-hook)
  :preface (defvar winner-dont-bind-my-keys t)
  :config (winner-mode +1)) ; I'll bind keys myself


(def-package! paren
  ;; highlight matching delimiters
  :after-call (after-find-file doom-switch-buffer-hook)
  :init
  (defun doom|disable-show-paren-mode ()
    "Turn off `show-paren-mode' buffer-locally."
    (set (make-local-variable 'show-paren-mode) nil))
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t)
  (show-paren-mode +1))


;;;###package whitespace
(setq whitespace-line-column nil
      whitespace-style
      '(face indentation tabs tab-mark spaces space-mark newline newline-mark
        trailing lines-tail)
      whitespace-display-mappings
      '((tab-mark ?\t [?› ?\t])
        (newline-mark ?\n [?¬ ?\n])
        (space-mark ?\  [?·] [?.])))

;; Disable these because whitespace should be customized programmatically
;; (through `whitespace-style'), and not through these commands.
(put 'whitespace-toggle-options 'disabled t)
(put 'global-whitespace-toggle-options 'disabled t)


;;
;;; Third party packages

(def-package! all-the-icons
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
             all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon)
  :init
  (defun doom*disable-all-the-icons-in-tty (orig-fn &rest args)
    (if (display-graphic-p)
        (apply orig-fn args)
      ""))
  :config
  ;; all-the-icons doesn't work in the terminal, so we "disable" it.
  (dolist (fn '(all-the-icons-octicon all-the-icons-material
                all-the-icons-faicon all-the-icons-fileicon
                all-the-icons-wicon all-the-icons-alltheicon))
    (advice-add fn :around #'doom*disable-all-the-icons-in-tty)))

;;;###package hide-mode-line-mode
(add-hook 'completion-list-mode-hook #'hide-mode-line-mode)
(add-hook 'Man-mode-hook #'hide-mode-line-mode)

;; Better fontification of number literals in code
(def-package! highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;;;###package rainbow-delimiters
;; Helps us distinguish stacked delimiter pairs, especially in parentheses-drunk
;; languages like Lisp.
(setq rainbow-delimiters-max-face-count 3)


;;
;;; Line numbers

;; line numbers in most modes
(add-hook! (prog-mode text-mode conf-mode) #'display-line-numbers-mode)

(defun doom|enable-line-numbers ()  (display-line-numbers-mode +1))
(defun doom|disable-line-numbers () (display-line-numbers-mode -1))

;; `nlinum' is used for Emacs 25 users, as Emacs 26+ has native line numbers.
(def-package! nlinum
  ;; Line number column. A faster (or equivalent, in the worst case) line number
  ;; plugin than `linum-mode'.
  :unless EMACS26+
  :defer t
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

(def-package! nlinum-hl
  ;; Fixes disappearing line numbers in nlinum and other quirks
  :unless EMACS26+
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
  (add-hook 'after-setting-font-hook #'nlinum-hl-flush-all-windows))

(def-package! nlinum-relative
  :unless EMACS26+
  :defer t
  :config
  (setq nlinum-format " %d ")
  (add-hook 'evil-mode-hook #'nlinum-relative-setup-evil))


;;
;;; Theme & font

(defun doom|init-fonts ()
  "Loads fonts.

Fonts are specified by `doom-font', `doom-variable-pitch-font',
`doom-serif-font' and `doom-unicode-font'."
  (condition-case e
      (progn
        (cond (doom-font
               (add-to-list
                'default-frame-alist
                (cons 'font
                      (cond ((stringp doom-font) doom-font)
                            ((fontp doom-font) (font-xlfd-name doom-font))
                            ((signal 'wrong-type-argument (list '(fontp stringp) doom-font)))))))
              ((display-graphic-p)
               (setq doom-font (face-attribute 'default :font))))
        (when doom-serif-font
          (set-face-attribute 'fixed-pitch-serif nil :font doom-serif-font))
        (when doom-variable-pitch-font
          (set-face-attribute 'variable-pitch nil :font doom-variable-pitch-font)))
    ((debug error)
     (if (string-prefix-p "Font not available: " (error-message-string e))
         (lwarn 'doom-ui :warning
                "Could not find the '%s' font on your system, falling back to system font"
                (font-get (caddr e) :family))
       (signal 'doom-error e)))))

(defun doom|init-emoji-fonts (frame)
  "Set up unicode fonts (if `doom-unicode-font' is set).

By default, this uses Apple Color Emoji on MacOS and Symbola on Linux."
  (when doom-unicode-font
    (set-fontset-font t 'unicode doom-unicode-font frame 'prepend)))

(defun doom|init-theme (&optional frame)
  "Load the theme specified by `doom-theme' in FRAME."
  (when (and doom-theme (not (memq doom-theme custom-enabled-themes)))
    (with-selected-frame (or frame (selected-frame))
      (let ((doom--prefer-theme-elc t))
        (load-theme doom-theme t)))))


;;
;;; Bootstrap

(defun doom|init-ui ()
  "Initialize Doom's user interface by applying all its advice and hooks."
  (run-hook-wrapped 'doom-init-ui-hook #'doom-try-run-hook)

  (add-to-list 'kill-buffer-query-functions #'doom|protect-fallback-buffer nil 'eq)
  (add-hook 'after-change-major-mode-hook #'doom|highlight-non-default-indentation)

  ;; Initialize custom switch-{buffer,window,frame} hooks:
  ;; + `doom-switch-buffer-hook'
  ;; + `doom-switch-window-hook'
  ;; + `doom-switch-frame-hook'
  (add-hook 'buffer-list-update-hook #'doom|run-switch-window-hooks)
  (add-hook 'focus-in-hook #'doom|run-switch-frame-hooks)
  (advice-add! '(switch-to-next-buffer switch-to-prev-buffer)
               :around #'doom*run-switch-to-next-prev-buffer-hooks)
  (advice-add! '(switch-to-buffer display-buffer)
               :around #'doom*run-switch-buffer-hooks))

;; Apply `doom-theme'
(if (daemonp)
    (add-hook 'after-make-frame-functions #'doom|init-theme)
  (add-hook 'doom-init-ui-hook #'doom|init-theme))
;; Apply `doom-font' et co
(add-hook 'doom-after-init-modules-hook #'doom|init-fonts)
;; Ensure unicode fonts are set on each frame
(add-hook 'after-make-frame-functions #'doom|init-emoji-fonts)
;; Setup `doom-load-theme-hook' and ensure `doom-theme' is always set to the
;; currently loaded theme
(advice-add #'load-theme :after #'doom*run-load-theme-hooks)

(add-hook 'window-setup-hook #'doom|init-ui)


;;
;;; Fixes/hacks

;; doesn't exist in terminal Emacs; we define it to prevent errors
(unless (fboundp 'define-fringe-bitmap)
  (defun define-fringe-bitmap (&rest _)))

(defun doom*prefer-compiled-theme (orig-fn &rest args)
  "Make `load-theme' prioritize the byte-compiled theme for a moderate boost in
startup (or theme switch) time, so long as `doom--prefer-theme-elc' is non-nil."
  (if (or (null after-init-time)
          doom--prefer-theme-elc)
      (cl-letf* ((old-locate-file (symbol-function 'locate-file))
                 ((symbol-function 'locate-file)
                  (lambda (filename path &optional _suffixes predicate)
                    (funcall old-locate-file filename path '("c" "") predicate))))
        (apply orig-fn args))
    (apply orig-fn args)))
(advice-add #'load-theme :around #'doom*prefer-compiled-theme)

(after! whitespace
  (defun doom*disable-whitespace-mode-in-childframes (orig-fn)
    "`whitespace-mode' inundates child frames with whitspace markers, so disable
it to fix all that visual noise."
    (unless (frame-parameter nil 'parent-frame)
      (funcall orig-fn)))
  (add-function :around whitespace-enable-predicate #'doom*disable-whitespace-mode-in-childframes)

  (defun doom|disable-whitespace-mode-in-childframes (frame)
    "`whitespace-mode' inundates child frames with whitspace markers, so disable
it to fix all that visual noise."
    (when (frame-parameter frame 'parent-frame)
      (with-selected-frame frame
        (setq-local whitespace-style nil)
        frame)))
  (add-hook 'after-make-frame-functions #'doom|disable-whitespace-mode-in-childframes))

;; Don't allow cursor to enter the prompt
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Don't display messages in the minibuffer when using the minibuffer
(defmacro doom-silence-motion-key (command key)
  (let ((key-command (intern (format "doom/silent-%s" command))))
    `(progn
       (defun ,key-command ()
         (interactive)
         (ignore-errors (call-interactively ',command)))
       (define-key minibuffer-local-map (kbd ,key) #',key-command))))
(doom-silence-motion-key backward-delete-char "<backspace>")
(doom-silence-motion-key delete-char "<delete>")

;; Switch to `doom-fallback-buffer' if on last real buffer
(advice-add #'kill-current-buffer :around #'doom*switch-to-fallback-buffer-maybe)

(provide 'core-ui)
;;; core-ui.el ends here
