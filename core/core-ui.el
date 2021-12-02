;;; core-ui.el -*- lexical-binding: t; -*-

;;
;;; Variables

(defvar doom-theme nil
  "A symbol representing the Emacs theme to load at startup.

Set to `nil' to load no theme at all. This variable is changed by
`load-theme'.")

(defvar doom-font nil
  "The default font to use.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string.

This affects the `default' and `fixed-pitch' faces.

Examples:
  (setq doom-font (font-spec :family \"Fira Mono\" :size 12))
  (setq doom-font \"Terminus (TTF):pixelsize=12:antialias=off\")")

(defvar doom-variable-pitch-font nil
  "The default font to use for variable-pitch text.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`doom-font' for examples.

An omitted font size means to inherit `doom-font''s size.")

(defvar doom-serif-font nil
  "The default font to use for the `fixed-pitch-serif' face.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`doom-font' for examples.

An omitted font size means to inherit `doom-font''s size.")

(defvar doom-unicode-font nil
  "Fallback font for Unicode glyphs.
Must be a `font-spec', a font object, an XFT font string, or an XLFD string. See
`doom-font' for examples.

The defaults on macOS and Linux are Apple Color Emoji and Symbola, respectively.

WARNING: if you specify a size for this font it will hard-lock any usage of this
font to that size. It's rarely a good idea to do so!")

(defvar doom-emoji-fallback-font-families
  '("Apple Color Emoji"
    "Segoe UI Emoji"
    "Noto Color Emoji"
    "Noto Emoji")
  "A list of fallback font families to use for emojis.")

(defvar doom-symbol-fallback-font-families
  '("Segoe UI Symbol"
    "Apple Symbols")
  "A list of fallback font families for general symbol glyphs.")


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

(defun doom-run-switch-buffer-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks 'doom-switch-buffer-hook)))

(defvar doom--last-frame nil)
(defun doom-run-switch-window-or-frame-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (unless (equal (old-selected-frame) (selected-frame))
      (run-hooks 'doom-switch-frame-hook))
    (unless (or (minibufferp)
                (equal (old-selected-window) (minibuffer-window)))
      (run-hooks 'doom-switch-window-hook))))

(defun doom-protect-fallback-buffer-h ()
  "Don't kill the scratch buffer. Meant for `kill-buffer-query-functions'."
  (not (eq (current-buffer) (doom-fallback-buffer))))

(defun doom-highlight-non-default-indentation-h ()
  "Highlight whitespace at odds with `indent-tabs-mode'.
That is, highlight tabs if `indent-tabs-mode' is `nil', and highlight spaces at
the beginnings of lines if `indent-tabs-mode' is `t'. The purpose is to make
incorrect indentation in the current buffer obvious to you.

Does nothing if `whitespace-mode' or `global-whitespace-mode' is already active
or if the current buffer is read-only or not file-visiting."
  (unless (or (eq major-mode 'fundamental-mode)
              (bound-and-true-p global-whitespace-mode)
              (null buffer-file-name))
    (require 'whitespace)
    (set (make-local-variable 'whitespace-style)
         (cl-union (if indent-tabs-mode
                       '(indentation)
                     '(tabs tab-mark))
                   (when whitespace-mode
                     (remq 'face whitespace-active-style))))
    (cl-pushnew 'face whitespace-style) ; must be first
    (whitespace-mode +1)))


;;
;;; General UX

;; A simple confirmation prompt when killing Emacs. But only prompt when there
;; are real buffers open.
(setq confirm-kill-emacs #'doom-quit-p)
;; Prompt for confirmation when deleting a non-empty frame; a last line of
;; defense against accidental loss of work.
(global-set-key [remap delete-frame] #'doom/delete-frame-with-prompt)

;; Don't prompt for confirmation when we create a new file or buffer (assume the
;; user knows what they're doing).
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

;; Larger column width for function name in profiler reports
(after! profiler
  (setf (caar profiler-report-cpu-line-format) 80
        (caar profiler-report-memory-line-format) 80))


;;
;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(setq-hook! '(eshell-mode-hook term-mode-hook) hscroll-margin 0)


;;
;;; Cursor

;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)


;;
;;; Buffers

;; Make `next-buffer', `other-buffer', etc. ignore unreal buffers.
(push '(buffer-predicate . doom-buffer-frame-predicate) default-frame-alist)

(defadvice! doom--switch-to-fallback-buffer-maybe-a (&rest _)
  "Switch to `doom-fallback-buffer' if on last real buffer.

Advice for `kill-current-buffer'. If in a dedicated window, delete it. If there
are no real buffers left OR if all remaining buffers are visible in other
windows, switch to `doom-fallback-buffer'. Otherwise, delegate to original
`kill-current-buffer'."
  :before-until #'kill-current-buffer
  (let ((buf (current-buffer)))
    (cond ((window-dedicated-p)
           (delete-window)
           t)
          ((eq buf (doom-fallback-buffer))
           (message "Can't kill the fallback buffer.")
           t)
          ((doom-real-buffer-p buf)
           (let ((visible-p (delq (selected-window) (get-buffer-window-list buf nil t))))
             (unless visible-p
               (when (and (buffer-modified-p buf)
                          (not (y-or-n-p
                                (format "Buffer %s is modified; kill anyway?"
                                        buf))))
                 (user-error "Aborted")))
             (let ((inhibit-redisplay t)
                   buffer-list-update-hook)
               (when (or ;; if there aren't more real buffers than visible buffers,
                      ;; then there are no real, non-visible buffers left.
                      (not (cl-set-difference (doom-real-buffer-list)
                                              (doom-visible-buffers)))
                      ;; if we end up back where we start (or previous-buffer
                      ;; returns nil), we have nowhere left to go
                      (memq (switch-to-prev-buffer nil t) (list buf 'nil)))
                 (switch-to-buffer (doom-fallback-buffer)))
               (unless visible-p
                 (with-current-buffer buf
                   (restore-buffer-modified-p nil))
                 (kill-buffer buf)))
             (run-hooks 'buffer-list-update-hook)
             t)))))


;;
;;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)


;;
;;; Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Doom Emacs")
      icon-title-format frame-title-format)

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; Disable tool, menu, and scrollbars. Doom is designed to be keyboard-centric,
;; so these are just clutter (the scrollbar also impacts performance). Whats
;; more, the menu bar exposes functionality that Doom doesn't endorse.
;;
;; I am intentionally not calling `menu-bar-mode', `tool-bar-mode', and
;; `scroll-bar-mode' because they do extra and unnecessary work that can be more
;; concisely and efficiently expressed with these six lines:
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'doom-init-ui-hook #'window-divider-mode)

;; GUIs are inconsistent across systems and themes (and will rarely match our
;; active Emacs theme). They impose inconsistent shortcut key paradigms too.
;; It's best to avoid them altogether and have Emacs handle the prompting.
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(when IS-LINUX
  (setq x-gtk-use-system-tooltips nil))

 ;; Favor vertical splits over horizontal ones. Monitors are trending toward
 ;; wide, rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)


;;
;;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only)

;; Typing yes/no is obnoxious when y/n will do
(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;;
;;; Built-in packages

;;;###package ansi-color
(setq ansi-color-for-comint-mode t)


(after! comint
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 2048)) ; double the default


(after! compile
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output 'first-error)
  ;; Handle ansi codes in compilation buffer
  (add-hook 'compilation-filter-hook #'doom-apply-ansi-color-to-compilation-buffer-h)
  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of Emacs.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))


(after! ediff
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)

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
  :hook (doom-first-buffer . global-hl-line-mode)
  :init
  (defvar global-hl-line-modes
    '(prog-mode text-mode conf-mode special-mode
      org-agenda-mode)
    "What modes to enable `hl-line-mode' in.")
  :config
  ;; HACK I reimplement `global-hl-line-mode' so we can white/blacklist modes in
  ;;      `global-hl-line-modes' _and_ so we can use `global-hl-line-mode',
  ;;      which users expect to control hl-line in Emacs.
  (define-globalized-minor-mode global-hl-line-mode hl-line-mode
    (lambda ()
      (and (cond (hl-line-mode nil)
                 ((null global-hl-line-modes) nil)
                 ((eq global-hl-line-modes t))
                 ((eq (car global-hl-line-modes) 'not)
                  (not (derived-mode-p global-hl-line-modes)))
                 ((apply #'derived-mode-p global-hl-line-modes)))
           (hl-line-mode +1))))

  ;; Temporarily disable `hl-line' when selection is active, since it doesn't
  ;; serve much purpose when the selection is so much more visible.
  (defvar doom--hl-line-mode nil)

  (add-hook! 'hl-line-mode-hook
    (defun doom-truly-disable-hl-line-h ()
      (unless hl-line-mode
        (setq-local doom--hl-line-mode nil))))

  (add-hook! '(evil-visual-state-entry-hook activate-mark-hook)
    (defun doom-disable-hl-line-h ()
      (when hl-line-mode
        (hl-line-mode -1)
        (setq-local doom--hl-line-mode t))))

  (add-hook! '(evil-visual-state-exit-hook deactivate-mark-hook)
    (defun doom-enable-hl-line-maybe-h ()
      (when doom--hl-line-mode
        (hl-line-mode +1)))))


(use-package! winner
  ;; undo/redo changes to Emacs' window layout
  :preface (defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
  :hook (doom-first-buffer . winner-mode)
  :config
  (appendq! winner-boring-buffers
            '("*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
              "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
              "*esh command on file*")))


(use-package! paren
  ;; highlight matching delimiters
  :hook (doom-first-buffer . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))


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
  :preface
  (add-hook! 'after-setting-font-hook
    (defun doom-init-all-the-icons-fonts-h ()
      (when (fboundp 'set-fontset-font)
        (dolist (font (list "Weather Icons"
                            "github-octicons"
                            "FontAwesome"
                            "all-the-icons"
                            "file-icons"
                            "Material Icons"))
          (set-fontset-font t 'unicode font nil 'append)))))
  :config
  (cond ((daemonp)
         (defadvice! doom--disable-all-the-icons-in-tty-a (fn &rest args)
           "Return a blank string in tty Emacs, which doesn't support multiple fonts."
           :around '(all-the-icons-octicon all-the-icons-material
                     all-the-icons-faicon all-the-icons-fileicon
                     all-the-icons-wicon all-the-icons-alltheicon)
           (if (or (not after-init-time) (display-multi-font-p))
               (apply fn args)
             "")))
        ((not (display-graphic-p))
         (defadvice! doom--disable-all-the-icons-in-tty-a (&rest _)
           "Return a blank string for tty users."
           :override '(all-the-icons-octicon all-the-icons-material
                       all-the-icons-faicon all-the-icons-fileicon
                       all-the-icons-wicon all-the-icons-alltheicon)
           ""))))

;; Hide the mode line in completion popups and MAN pages because they serve
;; little purpose there, and is better hidden.
;;;###package hide-mode-line-mode
(add-hook! '(completion-list-mode-hook Man-mode-hook)
           #'hide-mode-line-mode)

;; Many major modes do no highlighting of number literals, so we do it for them
(use-package! highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;;;###package image
(setq image-animate-loop t)

;;;###package rainbow-delimiters
;; Helps us distinguish stacked delimiter pairs, especially in parentheses-drunk
;; languages like Lisp. I reduce it from it's default of 9 to reduce the
;; complexity of the font-lock keyword and hopefully buy us a few ms of
;; performance.
(setq rainbow-delimiters-max-face-count 4)


;;
;;; Line numbers

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; Enable line numbers in most text-editing modes. We avoid
;; `global-display-line-numbers-mode' because there are many special and
;; temporary modes where we don't need/want them.
(add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
           #'display-line-numbers-mode)

;; Fix #2742: cursor is off by 4 characters in `artist-mode'
;; REVIEW Reported upstream https://debbugs.gnu.org/cgi/bugreport.cgi?bug=43811
;; DEPRECATED Fixed in Emacs 28; remove when we drop 27 support
(unless EMACS28+
  (add-hook 'artist-mode-hook #'doom-disable-line-numbers-h))


;;
;;; Theme & font

;; User themes should live in $DOOMDIR/themes, not ~/.emacs.d
(setq custom-theme-directory (concat doom-private-dir "themes/"))

;; Always prioritize the user's themes above the built-in/packaged ones.
(setq custom-theme-load-path
      (cons 'custom-theme-directory
            (delq 'custom-theme-directory custom-theme-load-path)))

(defun doom-init-fonts-h (&optional reload)
  "Loads `doom-font'."
  (when (fboundp 'set-fontset-font)
    (let ((fn (doom-rpartial #'member (font-family-list))))
      (when-let (font (cl-find-if fn doom-symbol-fallback-font-families))
        (set-fontset-font t 'symbol font))
      (when-let (font (cl-find-if fn doom-emoji-fallback-font-families))
        (set-fontset-font t 'unicode font))
      (when doom-unicode-font
        (set-fontset-font t 'unicode doom-unicode-font))))
  (apply #'custom-set-faces
         (let ((attrs '(:weight unspecified :slant unspecified :width unspecified)))
           (append (when doom-font
                     `((fixed-pitch ((t (:font ,doom-font ,@attrs))))))
                   (when doom-serif-font
                     `((fixed-pitch-serif ((t (:font ,doom-serif-font ,@attrs))))))
                   (when doom-variable-pitch-font
                     `((variable-pitch ((t (:font ,doom-variable-pitch-font ,@attrs)))))))))
  ;; Never save these settings to `custom-file'
  (dolist (sym '(fixed-pitch fixed-pitch-serif variable-pitch))
    (put sym 'saved-face nil))
  (cond
   (doom-font
    (when (or reload (daemonp))
      (set-frame-font doom-font t t))
    ;; I avoid `set-frame-font' at startup because it is expensive; doing extra,
    ;; unnecessary work we can avoid by setting the frame parameter directly.
    (setf (alist-get 'font default-frame-alist)
          (cond ((stringp doom-font) doom-font)
                ((fontp doom-font) (font-xlfd-name doom-font))
                ((signal 'wrong-type-argument
                         (list '(fontp stringp) doom-font))))))
   ((display-graphic-p)
    (setq font-use-system-font t)))
  ;; Give users a chance to inject their own font logic.
  (run-hooks 'after-setting-font-hook))

(defun doom-init-theme-h (&rest _)
  "Load the theme specified by `doom-theme' in FRAME."
  (when (and doom-theme (not (custom-theme-enabled-p doom-theme)))
    (load-theme doom-theme t)))

(defadvice! doom--load-theme-a (fn theme &optional no-confirm no-enable)
  "Record `doom-theme', disable old themes, and trigger `doom-load-theme-hook'."
  :around #'load-theme
  ;; Run `load-theme' from an estranged buffer, where we can ensure that
  ;; buffer-local face remaps (by `mixed-pitch-mode', for instance) won't
  ;; interfere with recalculating faces in new themes.
  (with-temp-buffer
    (let ((last-themes (copy-sequence custom-enabled-themes)))
      ;; Disable previous themes so there are no conflicts. If you truly want
      ;; multiple themes enabled, then use `enable-theme' instead.
      (mapc #'disable-theme custom-enabled-themes)
      (prog1 (funcall fn theme no-confirm no-enable)
        (when (and (not no-enable) (custom-theme-enabled-p theme))
          (setq doom-theme theme)
          (put 'doom-theme 'previous-themes (or last-themes 'none))
          (doom-run-hooks 'doom-load-theme-hook))))))


;;
;;; Bootstrap

(defun doom-init-ui-h (&optional _)
  "Initialize Doom's user interface by applying all its advice and hooks."
  (doom-run-hooks 'doom-init-ui-hook)

  (add-hook 'kill-buffer-query-functions #'doom-protect-fallback-buffer-h)
  (add-hook 'after-change-major-mode-hook #'doom-highlight-non-default-indentation-h 'append)

  ;; Initialize custom switch-{buffer,window,frame} hooks:
  ;;
  ;; - `doom-switch-buffer-hook'
  ;; - `doom-switch-window-hook'
  ;; - `doom-switch-frame-hook'
  ;;
  ;; These should be done as late as possible, as not to prematurely trigger
  ;; hooks during startup.
  (add-hook 'window-buffer-change-functions #'doom-run-switch-buffer-hooks-h)
  (add-hook 'window-selection-change-functions #'doom-run-switch-window-or-frame-hooks-h)

  ;; Only execute this function once.
  (remove-hook 'window-buffer-change-functions #'doom-init-ui-h))

;; Apply fonts and theme
(let ((hook (if (daemonp)
                'server-after-make-frame-hook
              'after-init-hook)))
  (add-hook hook #'doom-init-fonts-h -100)
  (add-hook hook #'doom-init-theme-h -90))

;; Initialize UI as late as possible. `window-buffer-change-functions' runs
;; once, when the scratch/dashboard buffer is first displayed.
(add-hook 'window-buffer-change-functions #'doom-init-ui-h -100)


;;
;;; Fixes/hacks

;; Doom doesn't support `customize' and it never will. It's a clumsy interface
;; that sets variables at a time where it can be easily and unpredictably
;; overwritten. Configure things from your $DOOMDIR instead.
(dolist (sym '(customize-option customize-browse customize-group customize-face
               customize-rogue customize-saved customize-apropos
               customize-changed customize-unsaved customize-variable
               customize-set-value customize-customized customize-set-variable
               customize-apropos-faces customize-save-variable
               customize-apropos-groups customize-apropos-options
               customize-changed-options customize-save-customized))
  (put sym 'disabled "Doom doesn't support `customize', configure Emacs from $DOOMDIR/config.el instead"))
(put 'customize-themes 'disabled "Set `doom-theme' or use `load-theme' in $DOOMDIR/config.el instead")

;; Doesn't exist in terminal Emacs, but some Emacs packages (internal and
;; external) use it anyway, leading to a void-function error, so define a no-op
;; substitute to suppress them.
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))

(after! whitespace
  (defun doom-is-childframes-p ()
    "`whitespace-mode' inundates child frames with whitespace markers, so
disable it to fix all that visual noise."
    (null (frame-parameter nil 'parent-frame)))
  (add-function :before-while whitespace-enable-predicate #'doom-is-childframes-p))

(provide 'core-ui)
;;; core-ui.el ends here
