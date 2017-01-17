;; core-ui.el --- draw me like one of your French editors

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

(defvar doom-ui-unicode-font
  (font-spec :family "DejaVu Sans Mono" :size 12)
  "Fallback font for unicode glyphs.")

(setq bidi-display-reordering nil ; disable bidirectional text for tiny performance boost
      blink-matching-paren nil ; don't blink--too distracting
      cursor-in-non-selected-windows nil
      echo-keystrokes 0.02
      frame-inhibit-implied-resize t
      ;; remove continuation arrow on right fringe
      fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                                   fringe-indicator-alist)
      highlight-nonselected-window nil
      image-animate-loop t
      indicate-buffer-boundaries nil
      indicate-empty-lines nil
      jit-lock-defer-time nil
      jit-lock-stealth-nice 0.1
      jit-lock-stealth-time 0.2
      jit-lock-stealth-verbose nil
      max-mini-window-height 0.3
      mode-line-default-help-echo nil  ; disable mode-line mouseovers
      redisplay-dont-pause t         ; don't pause display on input
      resize-mini-windows 'grow-only ; Minibuffer resizing
      show-help-function nil         ; hide :help-echo text
      show-paren-delay 0.075
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      split-width-threshold nil      ; favor horizontal splits
      uniquify-buffer-name-style nil
      use-dialog-box nil             ; always avoid GUI
      visible-bell nil
      visible-cursor nil
      x-stretch-cursor t)


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
    (set-frame-font doom-ui-font t t)
    ;; Fallback to `doom-unicode-font' for Unicode characters
    (when doom-ui-unicode-font
      (set-fontset-font t 'unicode doom-ui-unicode-font))
    ;; Set font for variable-pitch mode
    (when doom-ui-variable-pitch-font
      (set-face-attribute 'variable-pitch nil :font doom-ui-variable-pitch-font)))
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

(fset 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no
(global-eldoc-mode -1)        ; auto-enabled in Emacs 25+; I'd rather do this myself

;;; TODO Smart quit-confirmation prompt
;; Only prompt if there are real buffers left (see ;; `doom-real-buffer-p')
;;(setq confirm-kill-emacs
;;      (lambda (_)
;;        (if (ignore-errors (doom-get-real-buffers))
;;            (y-or-n-p "››› Quit?")
;;          t)))

;;; Flash the mode-line on error
;; TODO More flexible colors (only suits dark themes)
;; FIXME With a rapid key-repeat setting the mode-line bg can get stuck
(defvar doom--visual-bell-bg nil)
(setq ring-bell-function 'doom-visual-bell)
(defun doom-visual-bell ()
  (unless doom--visual-bell-bg
    (setq doom--visual-bell-bg (face-attribute 'mode-line :background)))
  (set-face-attribute 'mode-line nil :background "#54252C")
  (run-with-timer
   0.1 nil
   (lambda () (set-face-attribute 'mode-line nil :background doom--visual-bell-bg))))

;;; TODO/FIXME/NOTE highlighting in comments
(add-hook! (prog-mode emacs-lisp-mode css-mode)
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\(?:(.*)\\)?:?\\)\\>"  1 'warning prepend)
         ("\\<\\(FIXME\\(?:(.*)\\)?:?\\)\\>" 1 'error prepend)
         ("\\<\\(NOTE\\(?:(.*)\\)?:?\\)\\>"  1 'success prepend))))

;;; More reliable inter-window border
;; The native border "consumes" a pixel of the fringe on righter-most splits (in
;; Yamamoto's emacs-mac at least), `window-divider' does not.
;; NOTE available since Emacs 25.1
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode +1)


;;; Mode-line hiding minor mode
(defvar doom-hide-mode-line-format nil
  "Format to use when `doom-hide-mode-line-mode' replaces the modeline")

(defvar-local doom--old-mode-line nil)

(define-minor-mode doom-hide-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  (if doom-hide-mode-line-mode
      (setq doom--old-mode-line mode-line-format
            mode-line-format doom-hide-mode-line-format)
    (setq mode-line-format doom--old-mode-line
          doom--mode-line doom-hide-mode-line-format))
  (force-mode-line-update))

;; Ensure major-mode or theme changes don't overwrite these variables
(put 'doom--old-mode-line 'permanent-local t)
(put 'doom-hide-mode-line-mode 'permanent-local t)

;; mode-line is unimportant in some windows
(with-current-buffer "*Messages*" (doom-hide-mode-line-mode +1))
(add-hook! (help-mode compilation-mode messages-buffer-mode completion-list-mode)
  'doom-hide-mode-line-mode)


;;
;; Plugins
;;

;; Causes a flash around the cursor when it moves across a "large" distance.
;; Usually between windows, or across files. This makes it easier to keep track
;; where your cursor is, which I find helpful on my 30" 2560x1600 display.
(package! beacon
  :config (beacon-mode +1)
  (setq beacon-color (let ((bg (face-attribute 'highlight :background nil t)))
                       (if (eq bg 'unspecified) (face-attribute 'highlight :foreground nil t) bg))
        beacon-blink-when-buffer-changes t
        beacon-blink-when-point-moves-vertically 10))

;; I modified the built-in `hideshow' package to be prettier, autoload when
;; needed, and to behave as much like folding in vim does as possible. A better
;; option might be `origami', but certain bugs in it are preventing the switch
;; for now.
(package! hideshow :ensure nil
  :commands (hs-minor-mode hs-toggle-hiding hs-already-hidden-p)
  :init
  (defun doom*load-hs-minor-mode ()
    (hs-minor-mode 1)
    (advice-remove 'evil-toggle-fold 'doom-load-hs-minor-mode))
  (advice-add 'evil-toggle-fold :before 'doom*load-hs-minor-mode)

  :config
  ;; Prettify code folding in emacs
  (defface doom-folded-face '((t (:background "#ff8")))
    "Face to hightlight `hideshow' overlays."
    :group 'hideshow)
  (setq hs-set-up-overlay
        (lambda (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (overlay-put
             ov 'display (propertize "  [...]  " 'face 'doom-folded-face))))))

;; Show unintrusive indentation markers, and do some whitespace voodoo to
;; prevent the lack-of-indent-guides-on-blank-lines problem.
(package! highlight-indent-guides
  :commands highlight-indent-guides-mode
  :config
  (setq highlight-indent-guides-method 'character)

  (defun doom|highlight-indent-guides-adjust-whitespace (&optional start end)
    "Consider this the opposite of `delete-trailing-whitespace'. Injects
whitespace into buffer so that `highlight-indent-guides-mode' will display
consistent, unbroken indent markers. This whitespace is stripped out on save, as
not to affect the resulting file."
    (interactive (progn (barf-if-buffer-read-only)
                        (if (use-region-p)
                            (list (region-beginning) (region-end))
                          (list nil nil))))
    (unless indent-tabs-mode
      (save-match-data
        (save-excursion
          (let ((end-marker (copy-marker (or end (point-max))))
                (start (or start (point-min))))
            (goto-char start)
            (while (and (re-search-forward "^$" end-marker t) (not (>= (point) end-marker)))
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
              (forward-line 1)))))
      (set-buffer-modified-p nil))
    nil)

  (add-hook! highlight-indent-guides-mode
    (if highlight-indent-guides-mode
        (progn
          (doom|highlight-indent-guides-adjust-whitespace)
          (add-hook 'after-save-hook 'doom|highlight-indent-guides-adjust-whitespace nil t))
      (remove-hook 'after-save-hook 'doom|highlight-indent-guides-adjust-whitespace t)
      (delete-trailing-whitespace)))

  ;; If all else fails, this package tries to guess the indentation, but does it
  ;; naively, so get default indentation from editorconfig instead.
  (defun doom*highlight-indentation-guess-offset (&rest _)
    (when (featurep 'editorconfig)
      (setq-local highlight-indentation-offset
                  (string-to-int (gethash 'indent_size (editorconfig-get-properties))))))
  (advice-add 'highlight-indentation-guess-offset :before 'doom*highlight-indentation-guess-offset))

;; Some modes don't adequately highlight numbers, therefore...
(package! highlight-numbers :commands highlight-numbers-mode)

;; Line highlighting (built-in)
(package! hl-line :ensure nil
  :config
  ;; stickiness doesn't play nice with emacs 25+
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  ;; Remember whether hl-line was initially on or off in the current buffer
  (defvar-local doom--hl-line-mode nil)
  (defun doom|hl-line-on ()  (if doom--hl-line-mode (hl-line-mode +1)))
  (defun doom|hl-line-off () (if doom--hl-line-mode (hl-line-mode -1)))
  (add-hook! hl-line-mode (if hl-line-mode (setq doom--hl-line-mode t))))

;; A faster (or equal, in the worst case) line number plugin than `linum'. I've
;; modified it to highlight the current line, and fixed some glaring problems
;; with nlinum and frames.
(package! nlinum
  :commands nlinum-mode
  :preface (defvar nlinum-format "%4d ")
  :init
  (add-hook!
    (markdown-mode prog-mode scss-mode web-mode conf-mode groovy-mode
     nxml-mode snippet-mode php-mode)
    'nlinum-mode)

  :config
  (defun doom/nlinum-toggle ()
    "Toggle `nlinum-mode'."
    (interactive)
    (nlinum-mode (if (bound-and-true-p nlinum-mode) -1 +1)))

  ;; Optimization: calculate line number column width beforehand
  (add-hook! nlinum-mode
    (setq nlinum--width (length (save-excursion (goto-char (point-max))
                                                (format-mode-line "%l")))))

  ;; Disable nlinum when making frames, otherwise we get linum face error
  ;; messages that prevent frame creation.
  (defun doom|nlinum-off () (nlinum-mode -1))
  (add-hook 'before-make-frame-hook 'doom|nlinum-off)
  (add-hook 'after-make-frame-functions 'doom|nlinum-off))

;; Makes distinguishing stacked delimiters apart much, much easier. Especially
;; in parentheses-drunk languages like Lisp.
(package! rainbow-delimiters
  :commands rainbow-delimiters-mode
  :config (setq rainbow-delimiters-max-face-count 3)
  :init
  (add-hook! (emacs-lisp-mode lisp-mode js-mode css-mode c-mode-common)
    'rainbow-delimiters-mode))

;; Give color codes or names a background in that color. Nifty for css. Note
;; that hl-line and rainbow-mode don't play nicely together.
(package! rainbow-mode
  :commands rainbow-mode
  :init (after! hl-line (add-hook 'rainbow-mode-hook 'doom|hl-line-off)))

;; This makes distractions-free mode possible, but modifying window margins on
;; the fly and centering the buffer.
(package! visual-fill-column
  :config
  (add-hook! visual-fill-column-mode
    (setq-local split-window-preferred-function 'visual-line-mode-split-window-sensibly)))

(provide 'core-ui)
;;; core-ui.el ends here
