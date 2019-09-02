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

(defun doom-run-switch-window-hooks-h ()
  (let ((gc-cons-threshold most-positive-fixnum))
    (unless (or doom-inhibit-switch-window-hooks
                (eq doom--last-window (selected-window))
                (minibufferp))
      (let ((doom-inhibit-switch-window-hooks t))
        (run-hooks 'doom-switch-window-hook)
        (setq doom--last-window (selected-window))))))

(defun doom-run-switch-frame-hooks-h (&rest _)
  (unless (or doom-inhibit-switch-frame-hooks
              (eq doom--last-frame (selected-frame))
              (frame-parameter nil 'parent-frame))
    (let ((doom-inhibit-switch-frame-hooks t))
      (run-hooks 'doom-switch-frame-hook)
      (setq doom--last-frame (selected-frame)))))

(defun doom-run-switch-buffer-hooks-a (orig-fn buffer-or-name &rest args)
  (let ((gc-cons-threshold most-positive-fixnum))
    (if (or doom-inhibit-switch-buffer-hooks
            (eq (current-buffer) (get-buffer buffer-or-name))
            (and (eq orig-fn #'switch-to-buffer) (car args)))
        (apply orig-fn buffer-or-name args)
      (let ((doom-inhibit-switch-buffer-hooks t))
        (when-let (buffer (apply orig-fn buffer-or-name args))
          (with-current-buffer (if (windowp buffer)
                                   (window-buffer buffer)
                                 buffer)
            (run-hooks 'doom-switch-buffer-hook))
          buffer)))))

(defun doom-run-switch-to-next-prev-buffer-hooks-a (orig-fn &rest args)
  (let ((gc-cons-threshold most-positive-fixnum))
    (if doom-inhibit-switch-buffer-hooks
        (apply orig-fn args)
      (let ((doom-inhibit-switch-buffer-hooks t))
        (when-let (buffer (apply orig-fn args))
          (with-current-buffer buffer
            (run-hooks 'doom-switch-buffer-hook))
          buffer)))))

(defun doom-protect-fallback-buffer-h ()
  "Don't kill the scratch buffer. Meant for `kill-buffer-query-functions'."
  (not (eq (current-buffer) (doom-fallback-buffer))))

(defun doom-highlight-non-default-indentation-h ()
  "Highlight whitespace that doesn't match your `indent-tabs-mode' setting.

e.g. If you indent with spaces by default, tabs will be highlighted. If you
indent with tabs, spaces at BOL are highlighted.

Does nothing if `whitespace-mode' is already active or the current buffer is
read-only or not file-visiting."
  (unless (or (eq major-mode 'fundamental-mode)
              buffer-read-only
              (null buffer-file-name))
    (require 'whitespace)
    (set (make-local-variable 'whitespace-style)
         (let ((style (if indent-tabs-mode '(indentation) '(tabs tab-mark))))
           (if whitespace-mode
               (cl-union style whitespace-active-style)
             style)))
    (cl-pushnew 'face whitespace-style)
    (whitespace-mode +1)))


;;
;;; General UX

;; Simpler confirmation prompt when killing Emacs
(setq confirm-kill-emacs #'doom-quit-p)

(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

;; Enable mouse in terminal Emacs
(add-hook 'tty-setup-hook #'xterm-mouse-mode)


;;
;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(setq-hook! '(eshell-mode-hook term-mode-hook) hscroll-margin 0)

(when IS-MAC
  ;; sane trackpad/mouse scroll settings
  (setq mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smooth-scroll nil))


;;
;;; Cursor

;; Don't blink the cursor, it's too distracting.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

(setq visible-cursor nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)


;;
;;; Buffers

;; Make `next-buffer', `other-buffer', etc. ignore unreal buffers.
(push '(buffer-predicate . doom-buffer-frame-predicate) default-frame-alist)

(setq confirm-nonexistent-file-or-buffer t)

(defadvice! doom--switch-to-fallback-buffer-maybe-a (orig-fn)
  "Switch to `doom-fallback-buffer' if on last real buffer.

Advice for `kill-current-buffer'. If in a dedicated window, delete it. If there
are no real buffers left OR if all remaining buffers are visible in other
windows, switch to `doom-fallback-buffer'. Otherwise, delegate to original
`kill-current-buffer'."
  :around #'kill-current-buffer
  (let ((buf (current-buffer)))
    (cond ((window-dedicated-p)
           (delete-window))
          ((eq buf (doom-fallback-buffer))
           (message "Can't kill the fallback buffer."))
          ((doom-real-buffer-p buf)
           (if (and buffer-file-name
                    (buffer-modified-p buf)
                    (not (y-or-n-p
                          (format "Buffer %s is modified; kill anyway?" buf))))
               (message "Aborted")
             (set-buffer-modified-p nil)
             (let (buffer-list-update-hook)
               (when (or ;; if there aren't more real buffers than visible buffers,
                      ;; then there are no real, non-visible buffers left.
                      (not (cl-set-difference (doom-real-buffer-list)
                                              (doom-visible-buffers)))
                      ;; if we end up back where we start (or previous-buffer
                      ;; returns nil), we have nowhere left to go
                      (memq (switch-to-prev-buffer nil t) (list buf 'nil)))
                 (switch-to-buffer (doom-fallback-buffer)))
               (unless (delq (selected-window) (get-buffer-window-list buf nil t))
                 (kill-buffer buf)))
             (run-hooks 'buffer-list-update-hook)))
          ((funcall orig-fn)))))


;;
;;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; remove continuation arrow on right fringe
(delq! 'continuation fringe-indicator-alist 'assq)


;;
;;; Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Doom Emacs")
      icon-title-format frame-title-format)

;; Don't resize emacs in steps, it looks weird.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

(unless EMACS27+  ; We already do this in early-init.el
  ;; Disable tool and scrollbars; Doom encourages keyboard-centric workflows, so
  ;; these are just clutter (the scrollbar also impacts Emacs' performance).
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist))

;; Sets `ns-appearance' and `ns-transparent-titlebar' on GUI frames (and fixes
;; mismatching text color in the frame title)
(when IS-MAC
  ;; Curse Lion and its sudden but inevitable fullscreen mode!
  ;; NOTE Meaningless to railwaycat's emacs-mac build
  (setq ns-use-native-fullscreen nil
        ;; Visit files opened outside of Emacs in existing frame, rather than a
        ;; new one
        ns-pop-up-frames nil)

  ;; Sets ns-transparent-titlebar and ns-appearance frame parameters as is
  ;; appropriate for the loaded theme.
  (and (or (daemonp)
           (display-graphic-p))
       (require 'ns-auto-titlebar nil t)
       (ns-auto-titlebar-mode +1))

  (add-hook! 'after-make-frame-functions
    (defun doom-init-menu-bar-in-gui-frames-h (frame)
      "On MacOS, the menu bar isn't part of the frame. Disabling it makes MacOS
treat Emacs as a non-application window."
      (when (display-graphic-p frame)
        (set-frame-parameter frame 'menu-bar-lines 1)))))

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'doom-init-ui-hook #'window-divider-mode)

;; Prompt the user for confirmation when deleting a non-empty frame
(global-set-key [remap delete-frame] #'doom/delete-frame)

;; always avoid GUI
(setq use-dialog-box nil)
;; Don't display floating tooltips; display their contents in the echo-area.
(if (bound-and-true-p tooltip-mode) (tooltip-mode -1))
;; native linux tooltips are ugly
(when IS-LINUX
  (setq x-gtk-use-system-tooltips nil))

 ;; Favor vertical splits over horizontal ones
(setq split-width-threshold 160
      split-height-threshold nil)


;;
;;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; _while_ we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer, like vim does. Any feedback after
;; typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15)

;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
;; They're generally unhelpful and only add confusing visual clutter.
(setq mode-line-default-help-echo nil
      show-help-function nil)

;; y/n is easier to type than yes/no
(fset #'yes-or-no-p #'y-or-n-p)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;;
;;; Built-in packages

;;;###package ansi-color
(setq ansi-color-for-comint-mode t)


(use-package! compile
  :defer t
  :config
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output 'first-error)
  ;; Handle ansi codes in compilation buffer
  (add-hook 'compilation-filter-hook #'doom-apply-ansi-color-to-compilation-buffer-h))


(use-package! ediff
  :defer t
  :init
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)
  :config
  (defvar doom--ediff-saved-wconf nil)
  ;; Restore window config after quitting ediff
  (add-hook! 'ediff-before-setup-hook
    (defun doom-ediff-save-wconf-h ()
      (setq doom--ediff-saved-wconf (current-window-configuration))))
  (add-hook! '(ediff-quit-hook ediff-suspend-hook) :append
    (defun doom-ediff-restore-wconf-h ()
      (when (window-configuration-p doom--ediff-saved-wconf)
        (set-window-configuration doom--ediff-saved-wconf)))))


(use-package! hl-line
  ;; Highlights the current line
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
  :config
  ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
  ;; performance boost. I also don't need to see it in other buffers.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  ;; Disable `hl-line' in evil-visual mode (temporarily). `hl-line' can make the
  ;; selection region harder to see while in evil visual mode.
  (after! evil
    (defvar doom-buffer-hl-line-mode nil)
    (add-hook! 'evil-visual-state-entry-hook
      (defun doom-disable-hl-line-h ()
        (when hl-line-mode
          (setq-local doom-buffer-hl-line-mode t)
          (hl-line-mode -1))))
    (add-hook! 'evil-visual-state-exit-hook
      (defun doom-enable-hl-line-maybe-h ()
        (when doom-buffer-hl-line-mode
          (hl-line-mode +1))))))


(use-package! winner
  ;; undo/redo changes to Emacs' window layout
  :after-call after-find-file doom-switch-window-hook
  :preface (defvar winner-dont-bind-my-keys t)
  :config (winner-mode +1)) ; I'll bind keys myself


(use-package! paren
  ;; highlight matching delimiters
  :after-call after-find-file doom-switch-buffer-hook
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


;;
;;; Third party packages

(use-package! all-the-icons
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon)
  :init
  (defadvice! doom--disable-all-the-icons-in-tty-a (orig-fn &rest args)
    "Return a blank string in tty Emacs, which doesn't support multiple fonts."
    :around '(all-the-icons-octicon all-the-icons-material
              all-the-icons-faicon all-the-icons-fileicon
              all-the-icons-wicon all-the-icons-alltheicon)
    (if (display-multi-font-p)
        (apply orig-fn args)
      "")))

;;;###package hide-mode-line-mode
(add-hook! '(completion-list-mode-hook Man-mode-hook)
           #'hide-mode-line-mode)

;; Better fontification of number literals in code
(use-package! highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;;;###package image
(setq image-animate-loop t)

;;;###package rainbow-delimiters
;; Helps us distinguish stacked delimiter pairs, especially in parentheses-drunk
;; languages like Lisp.
(setq rainbow-delimiters-max-face-count 3)

;;;###package pos-tip
(setq pos-tip-internal-border-width 6
      pos-tip-border-width 1)


;;
;;; Line numbers

(setq-default display-line-numbers-width 3)

;; line numbers in most modes
(add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
           #'display-line-numbers-mode)

(defun doom-enable-line-numbers-h ()  (display-line-numbers-mode +1))
(defun doom-disable-line-numbers-h () (display-line-numbers-mode -1))

;; DEPRECATED `nlinum' is used for Emacs 25 users; 26+ has native line numbers.
(use-package! nlinum
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

  (add-hook! 'nlinum-mode-hook
    (defun doom-init-nlinum-width-h ()
      "Calculate line number column width beforehand (optimization)."
      (setq nlinum--width
            (length (save-excursion (goto-char (point-max))
                                    (format-mode-line "%l")))))))

(use-package! nlinum-hl
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

(use-package! nlinum-relative
  :unless EMACS26+
  :defer t
  :config
  (setq nlinum-format " %d ")
  (add-hook 'evil-mode-hook #'nlinum-relative-setup-evil))


;;
;;; Theme & font

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)

(defvar doom--prefer-theme-elc nil
  "If non-nil, `load-theme' will prefer the compiled theme (unlike its default
behavior). Do not set this directly, this is let-bound in `doom-init-theme-h'.")

(defun doom-init-fonts-h ()
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
          (set-face-attribute 'fixed-pitch-serif t :font doom-serif-font))
        (when doom-variable-pitch-font
          (set-face-attribute 'variable-pitch t :font doom-variable-pitch-font))
        (when (and doom-unicode-font (fboundp 'set-fontset-font))
          (set-fontset-font t 'unicode doom-unicode-font nil 'prepend)))
    ((debug error)
     (if (string-prefix-p "Font not available: " (error-message-string e))
         (lwarn 'doom-ui :warning
                "Could not find the '%s' font on your system, falling back to system font"
                (font-get (caddr e) :family))
       (signal 'doom-error e)))))

(defun doom-init-theme-h (&optional frame)
  "Load the theme specified by `doom-theme' in FRAME."
  (when (and doom-theme (not (memq doom-theme custom-enabled-themes)))
    (with-selected-frame (or frame (selected-frame))
      (let ((doom--prefer-theme-elc t))
        (load-theme doom-theme t)))))

(defadvice! doom--run-load-theme-hooks-a (theme &optional _no-confirm no-enable)
  "Set up `doom-load-theme-hook' to run after `load-theme' is called."
  :after-while #'load-theme
  (unless no-enable
    (setq doom-theme theme)
    (run-hooks 'doom-load-theme-hook)))

(defadvice! doom--prefer-compiled-theme-a (orig-fn &rest args)
  "Make `load-theme' prioritize the byte-compiled theme for a moderate boost in
startup (or theme switch) time, so long as `doom--prefer-theme-elc' is non-nil."
  :around #'load-theme
  (if (or (null after-init-time)
          doom--prefer-theme-elc)
      (cl-letf* ((old-locate-file (symbol-function 'locate-file))
                 ((symbol-function 'locate-file)
                  (lambda (filename path &optional _suffixes predicate)
                    (funcall old-locate-file filename path '("c" "") predicate))))
        (apply orig-fn args))
    (apply orig-fn args)))


;;
;;; Bootstrap

(defun doom-init-ui-h ()
  "Initialize Doom's user interface by applying all its advice and hooks."
  (run-hook-wrapped 'doom-init-ui-hook #'doom-try-run-hook)

  (add-to-list 'kill-buffer-query-functions #'doom-protect-fallback-buffer-h nil 'eq)
  (add-hook 'after-change-major-mode-hook #'doom-highlight-non-default-indentation-h 'append)

  ;; Initialize custom switch-{buffer,window,frame} hooks:
  ;; + `doom-switch-buffer-hook'
  ;; + `doom-switch-window-hook'
  ;; + `doom-switch-frame-hook'
  (add-hook 'buffer-list-update-hook #'doom-run-switch-window-hooks-h)
  (add-hook 'focus-in-hook #'doom-run-switch-frame-hooks-h)
  (dolist (fn '(switch-to-next-buffer switch-to-prev-buffer))
    (advice-add fn :around #'doom-run-switch-to-next-prev-buffer-hooks-a))
  (dolist (fn '(switch-to-buffer display-buffer))
    (advice-add fn :around #'doom-run-switch-buffer-hooks-a)))

;; Apply `doom-theme'
(add-hook 'doom-init-ui-hook #'doom-init-theme-h)
;; Apply `doom-font' et co
(add-hook 'doom-after-init-modules-hook #'doom-init-fonts-h)

(add-hook 'window-setup-hook #'doom-init-ui-h)


;;
;;; Fixes/hacks

;; doesn't exist in terminal Emacs; we define it to prevent errors
(unless (fboundp 'define-fringe-bitmap)
  (defun define-fringe-bitmap (&rest _)))

(after! whitespace
  (defun doom-disable-whitespace-mode-in-childframes-a (orig-fn)
    "`whitespace-mode' inundates child frames with whitspace markers, so disable
it to fix all that visual noise."
    (unless (frame-parameter nil 'parent-frame)
      (funcall orig-fn)))
  (add-function :around whitespace-enable-predicate #'doom-disable-whitespace-mode-in-childframes-a))

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

(provide 'core-ui)
;;; core-ui.el ends here
