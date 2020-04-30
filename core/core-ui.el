;;; core-ui.el -*- lexical-binding: t; -*-

;;
;;; Variables

(defvar doom-init-theme-p nil
  "If non-nil, a theme as been loaded.")

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

(defvar doom-unicode-extra-fonts nil
  "Fonts to inject into the unicode charset before `doom-unicode-font'.")


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
  (unless (or doom-inhibit-switch-window-hooks
              (eq doom--last-window (selected-window))
              (minibufferp))
    (let ((gc-cons-threshold most-positive-fixnum)
          (doom-inhibit-switch-window-hooks t)
          (inhibit-redisplay t))
      (run-hooks 'doom-switch-window-hook)
      (setq doom--last-window (selected-window)))))

(defun doom-run-switch-frame-hooks-h (&rest _)
  (unless (or doom-inhibit-switch-frame-hooks
              (eq doom--last-frame (selected-frame))
              (frame-parameter nil 'parent-frame))
    (let ((gc-cons-threshold most-positive-fixnum)
          (doom-inhibit-switch-frame-hooks t))
      (run-hooks 'doom-switch-frame-hook)
      (setq doom--last-frame (selected-frame)))))

(defun doom-run-switch-buffer-hooks-a (orig-fn buffer-or-name &rest args)
  (if (or doom-inhibit-switch-buffer-hooks
          (and buffer-or-name
               (eq (current-buffer)
                   (get-buffer buffer-or-name)))
          (and (eq orig-fn #'switch-to-buffer) (car args)))
      (apply orig-fn buffer-or-name args)
    (let ((gc-cons-threshold most-positive-fixnum)
          (doom-inhibit-switch-buffer-hooks t)
          (inhibit-redisplay t))
      (when-let (buffer (apply orig-fn buffer-or-name args))
        (with-current-buffer (if (windowp buffer)
                                 (window-buffer buffer)
                               buffer)
          (run-hooks 'doom-switch-buffer-hook))
        buffer))))

(defun doom-run-switch-to-next-prev-buffer-hooks-a (orig-fn &rest args)
  (if doom-inhibit-switch-buffer-hooks
      (apply orig-fn args)
    (let ((gc-cons-threshold most-positive-fixnum)
          (doom-inhibit-switch-buffer-hooks t)
          (inhibit-redisplay t))
      (when-let (buffer (apply orig-fn args))
        (with-current-buffer buffer
          (run-hooks 'doom-switch-buffer-hook))
        buffer))))

(defun doom-protect-fallback-buffer-h ()
  "Don't kill the scratch buffer. Meant for `kill-buffer-query-functions'."
  (not (eq (current-buffer) (doom-fallback-buffer))))

(defun doom-highlight-non-default-indentation-h ()
  "Highlight whitespace that doesn't match your `indent-tabs-mode' setting.

e.g. If you indent with spaces by default, tabs will be highlighted. If you
indent with tabs, spaces at BOL are highlighted.

Does nothing if `whitespace-mode' or 'global-whitespace-mode' is already 
active or if the current buffer is read-only or not file-visiting."
  (unless (or (eq major-mode 'fundamental-mode)
              buffer-read-only
              (bound-and-true-p global-whitespace-mode)
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

;; Don't prompt for confirmation when we create a new file or buffer (assume the
;; user knows what they're doing).
(setq confirm-nonexistent-file-or-buffer nil)

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

;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Some terminals offer two different cursors: a “visible” static cursor and a
;; “very visible” blinking one. By default, Emacs uses the very visible cursor
;; and switches to it when you start or resume Emacs. If `visible-cursor' is nil
;; when Emacs starts or resumes, it uses the normal cursor.
(setq visible-cursor nil)

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
             (run-hooks 'buffer-list-update-hook))
           t))))


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

;; Don't resize windows & frames in steps; it's prohibitive to prevent the user
;; from resizing it to exact dimensions, and looks weird.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

(unless (assq 'menu-bar-lines default-frame-alist)
  ;; We do this in early-init.el too, but in case the user is on Emacs 26 we do
  ;; it here too: disable tool and scrollbars, as Doom encourages
  ;; keyboard-centric workflows, so these are just clutter (the scrollbar also
  ;; impacts performance).
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars)))

(when! IS-MAC
  ;; Curse Lion and its sudden but inevitable fullscreen mode!
  ;; NOTE Meaningless to railwaycat's emacs-mac build
  (setq ns-use-native-fullscreen nil)

  ;; Visit files opened outside of Emacs in existing frame, not a new one
  (setq ns-pop-up-frames nil)

  ;; Sets `ns-transparent-titlebar' and `ns-appearance' frame parameters so
  ;; window borders will match the enabled theme.
  (and (or (daemonp)
           (display-graphic-p))
       (require 'ns-auto-titlebar nil t)
       (ns-auto-titlebar-mode +1))

  ;; HACK On MacOS, disabling the menu bar makes MacOS treat Emacs as a
  ;;      non-application window -- which means it doesn't automatically capture
  ;;      focus when it is started, among other things. We enable menu-bar-lines
  ;;      there, but we still want it disabled in terminal frames because there
  ;;      it activates an ugly menu bar.
  (add-hook! '(window-setup-hook after-make-frame-functions)
    (defun doom-init-menu-bar-in-gui-frames-h (&optional frame)
      "Re-enable menu-bar-lines in GUI frames."
      (when-let (frame (or frame (selected-frame)))
        (when (display-graphic-p frame)
          (set-frame-parameter frame 'menu-bar-lines 1))))))

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'doom-init-ui-hook #'window-divider-mode)

;; Prompt for confirmation when deleting a non-empty frame; a last line of
;; defense against accidental loss of work.
(global-set-key [remap delete-frame] #'doom/delete-frame-with-prompt)

;; always avoid GUI
(setq use-dialog-box nil)
;; Don't display floating tooltips; display their contents in the echo-area,
;; because native tooltips are ugly.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
;; ...especially on linux
(when IS-LINUX
  (setq x-gtk-use-system-tooltips nil))

 ;; Favor vertical splits over horizontal ones. Screens are usually wide.
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
(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15)

;; Typing yes/no is obnoxious when y/n will do
(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;;
;;; Built-in packages

;;;###package ansi-color
(setq ansi-color-for-comint-mode t)


(after! compile
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output 'first-error)
  ;; Handle ansi codes in compilation buffer
  (add-hook 'compilation-filter-hook #'doom-apply-ansi-color-to-compilation-buffer-h))


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
  :hook ((prog-mode text-mode conf-mode special-mode) . hl-line-mode)
  :config
  ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
  ;; performance boost. I also don't need to see it in other buffers.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  ;; Temporarily disable `hl-line' when selection is active, since it doesn't
  ;; serve much purpose when the selection is so much more visible.
  (defvar doom--hl-line-mode nil)

  (add-hook! '(evil-visual-state-entry-hook activate-mark-hook)
    (defun doom-disable-hl-line-h ()
      (when hl-line-mode
        (setq-local doom--hl-line-mode t)
        (hl-line-mode -1))))

  (add-hook! '(evil-visual-state-exit-hook deactivate-mark-hook)
    (defun doom-enable-hl-line-maybe-h ()
      (when doom--hl-line-mode
        (hl-line-mode +1)))))


(use-package! winner
  ;; undo/redo changes to Emacs' window layout
  :after-call after-find-file doom-switch-window-hook
  :preface (defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
  :config (winner-mode +1)
  (appendq! winner-boring-buffers
            '("*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
              "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
              "*esh command on file*")))


(use-package! paren
  ;; highlight matching delimiters
  :after-call after-find-file doom-switch-buffer-hook
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
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
  :preface
  (setq doom-unicode-extra-fonts
        (list "Weather Icons"
              "github-octicons"
              "FontAwesome"
              "all-the-icons"
              "file-icons"
              "Material Icons"))
  :config
  (cond ((daemonp)
         (defadvice! doom--disable-all-the-icons-in-tty-a (orig-fn &rest args)
           "Return a blank string in tty Emacs, which doesn't support multiple fonts."
           :around '(all-the-icons-octicon all-the-icons-material
                     all-the-icons-faicon all-the-icons-fileicon
                     all-the-icons-wicon all-the-icons-alltheicon)
           (if (or (not after-init-time) (display-multi-font-p))
               (apply orig-fn args)
             "")))
        ((not (display-graphic-p))
         (defadvice! doom--disable-all-the-icons-in-tty-a (&rest _)
           "Return a blank string for tty users."
           :override '(all-the-icons-octicon all-the-icons-material
                       all-the-icons-faicon all-the-icons-fileicon
                       all-the-icons-wicon all-the-icons-alltheicon)
           ""))))

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
;; languages like Lisp.
(setq rainbow-delimiters-max-face-count 3)


;;
;;; Line numbers

;; Explicitly define a width to reduce computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions makes it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; Enable line numbers in most text-editing modes. We avoid
;; `global-display-line-numbers-mode' because there are many special and
;; temporary modes where we don't need/want them.
(add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
           #'display-line-numbers-mode)


;;
;;; Theme & font

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)

;; DEPRECATED In Emacs 27
(defvar doom--prefer-theme-elc nil
  "If non-nil, `load-theme' will prefer the compiled theme (unlike its default
behavior). Do not set this directly, this is let-bound in `doom-init-theme-h'.")

(defun doom-init-fonts-h ()
  "Loads `doom-font'."
  (cond
   (doom-font
    (cl-pushnew
     ;; Avoiding `set-frame-font' because it does a lot of extra, expensive
     ;; work we can avoid by setting the font frame parameter instead.
     (cons 'font
           (cond ((stringp doom-font) doom-font)
                 ((fontp doom-font) (font-xlfd-name doom-font))
                 ((signal 'wrong-type-argument (list '(fontp stringp)
                                                     doom-font)))))
     default-frame-alist
     :key #'car :test #'eq))
   ((display-graphic-p)
    ;; We try our best to record your system font, so `doom-big-font-mode'
    ;; can still use it to compute a larger font size with.
    (setq font-use-system-font t
          doom-font (face-attribute 'default :font)))))

(defun doom-init-extra-fonts-h (&optional frame)
  "Loads `doom-variable-pitch-font',`doom-serif-font' and `doom-unicode-font'."
  (condition-case e
      (with-selected-frame (or frame (selected-frame))
        (when doom-font
          (set-face-attribute 'fixed-pitch nil :font doom-font))
        (when doom-serif-font
          (set-face-attribute 'fixed-pitch-serif nil :font doom-serif-font))
        (when doom-variable-pitch-font
          (set-face-attribute 'variable-pitch nil :font doom-variable-pitch-font))
        (when (fboundp 'set-fontset-font)
          (dolist (font (append doom-unicode-extra-fonts (doom-enlist doom-unicode-font)))
            (set-fontset-font t 'unicode font nil 'prepend))))
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
      (let ((doom--prefer-theme-elc t)) ; DEPRECATED in Emacs 27
        (load-theme doom-theme t)))))

(defadvice! doom--load-theme-a (orig-fn theme &optional no-confirm no-enable)
  "Run `doom-load-theme-hook' on `load-theme' and fix its issues.

1. Disable previously enabled themes.
2. Don't let face-remapping screw up loading the new theme
   (*cough*`mixed-pitch-mode').
3. Record the current theme in `doom-theme'."
  :around #'load-theme
  ;; HACK Run `load-theme' from an estranged buffer, where we can be assured
  ;;      that buffer-local face remaps (by `mixed-pitch-mode', for instance)
  ;;      won't interfere with changing themes.
  (with-temp-buffer
    (when-let (result (funcall orig-fn theme no-confirm no-enable))
      (unless no-enable
        (setq doom-theme theme
              doom-init-theme-p t)
        ;; `load-theme' doesn't disable previously enabled themes, which seems
        ;; like what you'd want. You could always use `enable-theme' to activate
        ;; multiple themes instead.
        (mapc #'disable-theme (remq theme custom-enabled-themes))
        (run-hooks 'doom-load-theme-hook))
      result)))

(when! (not EMACS27+)
  ;; DEPRECATED Not needed in Emacs 27
  (defadvice! doom--prefer-compiled-theme-a (orig-fn &rest args)
    "Have `load-theme' prioritize the byte-compiled theme.
This offers a moderate boost in startup (or theme switch) time, so long as
`doom--prefer-theme-elc' is non-nil."
    :around #'load-theme
    (if (or (null after-init-time)
            doom--prefer-theme-elc)
        (letf! (defun locate-file (filename path &optional _suffixes predicate)
                 (funcall locate-file filename path '("c" "") predicate))
          (apply orig-fn args))
      (apply orig-fn args))))


;;
;;; Bootstrap

(defun doom-init-ui-h ()
  "Initialize Doom's user interface by applying all its advice and hooks."
  (run-hook-wrapped 'doom-init-ui-hook #'doom-try-run-hook)

  (add-hook 'kill-buffer-query-functions #'doom-protect-fallback-buffer-h)
  (add-hook 'after-change-major-mode-hook #'doom-highlight-non-default-indentation-h 'append)

  ;; Initialize custom switch-{buffer,window,frame} hooks:
  ;;
  ;; + `doom-switch-buffer-hook'
  ;; + `doom-switch-window-hook'
  ;; + `doom-switch-frame-hook'
  ;;
  ;; These should be done as late as possible, as not to prematurely trigger
  ;; hooks during startup.
  (add-hook 'buffer-list-update-hook #'doom-run-switch-window-hooks-h)
  (add-hook 'focus-in-hook #'doom-run-switch-frame-hooks-h)
  (dolist (fn '(switch-to-next-buffer switch-to-prev-buffer))
    (advice-add fn :around #'doom-run-switch-to-next-prev-buffer-hooks-a))
  (dolist (fn '(switch-to-buffer display-buffer))
    (advice-add fn :around #'doom-run-switch-buffer-hooks-a)))

;; Apply `doom-font' et co
(add-hook 'doom-after-init-modules-hook #'doom-init-fonts-h)
(add-hook 'doom-load-theme-hook #'doom-init-extra-fonts-h)

;; Apply `doom-theme'
(add-hook (if (daemonp)
              'after-make-frame-functions
            'doom-init-ui-hook)
          #'doom-init-theme-h)

(add-hook 'window-setup-hook #'doom-init-ui-h)


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

;; Doesn't exist in terminal Emacs, so we define it to prevent void-function
;; errors emitted from packages that blindly try to use it.
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))

(after! whitespace
  (defun doom-disable-whitespace-mode-in-childframes-a (orig-fn)
    "`whitespace-mode' inundates child frames with whitespace markers, so
disable it to fix all that visual noise."
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
