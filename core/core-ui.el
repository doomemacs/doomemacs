;;; core-ui.el

(defvar doom-ui-fringe-size '3
  "Default fringe width")

(defvar doom-ui-theme 'doom-one
  "The color theme currently in use.")

(defvar doom-ui-font
  (font-spec :family "Fira Mono" :size 12)
  "The font currently in use.")

(defvar doom-ui-variable-pitch-font
  (font-spec :family "Fira Sans" :size 12)
  "The font currently in use.")


;;
;; Configuration
;;

(setq-default
 mode-line-default-help-echo nil ; don't say anything on mode-line mouseover
 indicate-buffer-boundaries nil  ; don't show where buffer starts/ends
 indicate-empty-lines nil        ; don't show empty lines
 fringes-outside-margins t       ; switches order of fringe and margin
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
 show-paren-highlight-openparen t
 show-paren-when-point-inside-paren t
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
 image-animate-loop t
 ;; Ask for confirmation on exit only if there are real buffers left
 confirm-kill-emacs
 (lambda (_)
   (if (ignore-errors (doom/get-real-buffers))
       (y-or-n-p "››› Quit?")
     t)))

;; A subtle bell: flash the mode-line
;; TODO More flexible colors (only suits dark themes)
(defvar doom--modeline-bg nil)

(setq ring-bell-function
      (lambda ()
        (unless doom--modeline-bg
          (setq doom--modeline-bg (face-attribute 'mode-line :background)))
        (set-face-attribute 'mode-line nil :background "#54252C")
        (run-with-timer
         0.1 nil
         (lambda () (set-face-attribute 'mode-line nil :background doom--modeline-bg)))))


;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; mode-line is unimportant in help/compile windows
(add-hook 'help-mode-hook 'doom-hide-mode-line-mode)
(add-hook 'compilation-mode-hook 'doom-hide-mode-line-mode)
(add-hook 'messages-buffer-mode-hook 'doom-hide-mode-line-mode)
(with-current-buffer "*Messages*" (doom-hide-mode-line-mode +1))

;; Eldoc is enabled globally on Emacs 25. No thank you, I'll do it myself.
(when (bound-and-true-p global-eldoc-mode)
  (global-eldoc-mode -1))

;; TODO/FIXME/NOTE highlighting in comments
(add-hook! (prog-mode emacs-lisp-mode css-mode)
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\(?:(.*)\\)?:?\\)\\>"  1 'warning prepend)
         ("\\<\\(FIXME\\(?:(.*)\\)?:?\\)\\>" 1 'error prepend)
         ("\\<\\(NOTE\\(?:(.*)\\)?:?\\)\\>"  1 'success prepend))))

;; `window-divider-mode' gives us finer control over the border between windows.
;; The native border "consumes" a pixel of the fringe on righter-most splits (in
;; Yamamoto's emacs-mac at least), window-divider does not.
;; NOTE Only available on Emacs 25.1+
(when (boundp 'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (window-divider-mode +1))


;;
;; Plugins
;;

(use-package doom-themes
  :config
  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-file-icons 'simple
        doom-neotree-line-spacing 3)
  (load-theme doom-ui-theme t)
  ;; brighter source buffers
  (add-hook 'find-file-hook 'doom-buffer-mode)
  ;; Custom neotree theme
  (when window-system
    (require 'doom-neotree)))

(use-package beacon
  :config
  (beacon-mode +1)
  (setq beacon-color (face-attribute 'highlight :background nil t)
        beacon-blink-when-buffer-changes t
        beacon-blink-when-point-moves-vertically 10))

(use-package hl-line
  :init (add-hook 'prog-mode-hook 'hl-line-mode)
  :config
  ;; stickiness doesn't play nice with emacs 25+
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  ;; Remember whether hl-line was initially on or off in the current buffer
  (defvar-local doom--hl-line-mode nil)
  (defun doom|hl-line-on ()  (if doom--hl-line-mode (hl-line-mode +1)))
  (defun doom|hl-line-off () (if doom--hl-line-mode (hl-line-mode -1)))
  (add-hook! hl-line-mode (if hl-line-mode (setq doom--hl-line-mode t))))

(use-package highlight-indentation
  :commands (highlight-indentation-mode
             highlight-indentation-current-column-mode)
  :init
  (after! editorconfig
    (advice-add 'highlight-indentation-guess-offset
                :override 'doom*hl-indent-guess-offset))
  ;; Since empty lines are stripped on save, the indentation highlights will
  ;; have unseemly breaks in them. These hooks will indent empty lines so that
  ;; the highlights are consistent, without affecting the saved output.
  (add-hook! highlight-indentation-mode
    (if highlight-indentation-mode
        (progn
          (doom/add-whitespace)
          (add-hook 'after-save-hook 'doom/add-whitespace nil t))
      (remove-hook 'after-save-hook 'doom/add-whitespace t)
      (delete-trailing-whitespace))))

(use-package highlight-numbers :commands (highlight-numbers-mode))

(use-package nlinum
  :commands nlinum-mode
  :preface
  (setq linum-format "%3d ")
  (defvar nlinum-format "%4d ")
  (defvar doom--hl-nlinum-overlay nil)
  (defvar doom--hl-nlinum-line nil)
  :init
  (add-hook!
    (markdown-mode prog-mode scss-mode web-mode conf-mode groovy-mode
     nxml-mode snippet-mode php-mode)
    'nlinum-mode)
  ;; FIXME This only works if hl-line is active! Why?
  (add-hook! nlinum-mode
    (if nlinum-mode-hook
        (add-hook 'post-command-hook 'doom|nlinum-hl-line nil t)
      (remove-hook 'post-command-hook 'doom|nlinum-hl-line t)))
  :config
  ;; Calculate line number column width beforehand
  (add-hook! nlinum-mode
    (setq nlinum--width (length (save-excursion (goto-char (point-max))
                                                (format-mode-line "%l")))))

  ;; Disable nlinum when making frames, otherwise we get linum face error
  ;; messages that prevent frame creation.
  (add-hook 'before-make-frame-hook 'doom|nlinum-disable)
  (add-hook 'after-make-frame-functions 'doom|nlinum-enable))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :config (setq rainbow-delimiters-max-face-count 3)
  :init
  (add-hook! (emacs-lisp-mode lisp-mode js-mode css-mode c-mode-common)
    'rainbow-delimiters-mode))

;; NOTE hl-line-mode and rainbow-mode don't play well together
(use-package rainbow-mode
  :commands rainbow-mode
  :init (after! hl-line (add-hook 'rainbow-mode-hook 'doom|hl-line-off)))

(use-package stripe-buffer
  :commands stripe-buffer-mode
  :init (add-hook 'dired-mode-hook 'stripe-buffer-mode))

(use-package visual-fill-column :defer t
  :config
  (setq-default visual-fill-column-center-text nil
                visual-fill-column-width fill-column
                split-window-preferred-function 'visual-line-mode-split-window-sensibly))


;;
;; Bootstrap
;;

(tooltip-mode -1) ; relegate tooltips to echo area only
(menu-bar-mode -1)
(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  ;; full filename in frame title
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  ;; Set theme and font
  (with-demoted-errors "FONT ERROR: %s"
    (set-frame-font doom-ui-font t)
    ;; Fallback to `doom-unicode-font' for Unicode characters
    (set-fontset-font t 'unicode doom-unicode-font)
    ;; Set font for variable-pitch mode
    (set-face-attribute 'variable-pitch nil :font doom-ui-variable-pitch-font))
  ;; standardize fringe width
  (fringe-mode doom-ui-fringe-size)
  (push `(left-fringe  . ,doom-ui-fringe-size) default-frame-alist)
  (push `(right-fringe . ,doom-ui-fringe-size) default-frame-alist)
  ;; no fringe in the minibuffer
  (add-hook! (emacs-startup minibuffer-setup)
    (set-window-fringes (minibuffer-window) 0 0 nil))
  ;; Show tilde in margin on empty lines
  (define-fringe-bitmap 'tilde [64 168 16] nil nil 'center)
  (set-fringe-bitmap-face 'tilde 'fringe)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde))

(provide 'core-ui)
;;; core-ui.el ends here
