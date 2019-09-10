;;; editor/evil/config.el -*- lexical-binding: t; -*-

;; I'm a vimmer at heart. Its modal philosophy suits me better, and this module
;; strives to make Emacs a much better vim than vim was.

(defvar +evil-want-o/O-to-continue-comments t
  "If non-nil, the o/O keys will continue comment lines if the point is on a
line with a linewise comment.")

(defvar +evil-preprocessor-regexp "^\\s-*#[a-zA-Z0-9_]"
  "The regexp used by `+evil/next-preproc-directive' and
`+evil/previous-preproc-directive' on ]# and [#, to jump between preprocessor
directives. By default, this only recognizes C directives.")

;; Set these defaults before `evil'; use `defvar' so they can be changed prior
;; to loading.
(defvar evil-want-C-i-jump (or (daemonp) (display-graphic-p)))
(defvar evil-want-C-u-scroll t)
(defvar evil-want-C-w-scroll t)
(defvar evil-want-Y-yank-to-eol t)
(defvar evil-want-abbrev-expand-on-insert-exit nil)

(use-package! evil
  :hook (doom-init-modules . evil-mode)
  :demand t
  :preface
  (setq evil-want-visual-char-semi-exclusive t
        evil-magic t
        evil-echo-state t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-insert-skip-empty-lines t
        evil-mode-line-format 'nil
        evil-respect-visual-line-mode t
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; cursor appearance
        evil-default-cursor '+evil-default-cursor-fn
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        ;; must be set before evil/evil-collection is loaded
        evil-want-keybinding (not (featurep! +everywhere)))

  :config
  (evil-select-search-module 'evil-search-module 'evil-search)

  (put 'evil-define-key* 'lisp-indent-function 'defun)

  ;; stop copying each visual state move to the clipboard:
  ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
  ;; grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (advice-add #'evil-visual-update-x-selection :override #'ignore)

  ;; Start help-with-tutorial in emacs state
  (advice-add #'help-with-tutorial :after (lambda (&rest _) (evil-emacs-state +1)))

  ;; Done in a hook to ensure the popup rules load as late as possible
  (add-hook! 'doom-init-modules-hook
    (defun +evil--init-popup-rules-h ()
      (set-popup-rules!
        '(("^\\*evil-registers" :size 0.3)
          ("^\\*Command Line"   :size 8)))))

  ;; Change the cursor color in emacs state. We do it this roundabout way
  ;; instead of changing `evil-default-cursor' (or `evil-emacs-state-cursor') so
  ;; it won't interfere with users who have changed these variables.
  (defvar +evil--default-cursor-color "#ffffff")
  (defvar +evil--emacs-cursor-color "#ff9999")

  (add-hook! 'doom-load-theme-hook
    (defun +evil-update-cursor-color-h ()
      (setq +evil--default-cursor-color (face-background 'cursor)
            +evil--emacs-cursor-color (face-foreground 'warning))))

  (defun +evil-default-cursor-fn ()
    (evil-set-cursor-color +evil--default-cursor-color))
  (defun +evil-emacs-cursor-fn ()
    (evil-set-cursor-color +evil--emacs-cursor-color))

  (setq-hook! 'after-change-major-mode-hook evil-shift-width tab-width)


  ;; --- keybind fixes ----------------------
  (after! wgrep
    ;; A wrapper that invokes `wgrep-mark-deletion' across lines you use
    ;; `evil-delete' in wgrep buffers.
    (define-key wgrep-mode-map [remap evil-delete] #'+evil-delete))

  (add-hook! 'doom-escape-hook
    (defun +evil-disable-ex-highlights-h ()
      "Disable ex search buffer highlights."
      (when (evil-ex-hl-active-p 'evil-ex-search)
        (evil-ex-nohighlight)
        t)))


  ;; --- evil hacks -------------------------
  (unless noninteractive
    (setq save-silently t)
    (add-hook! 'after-save-hook
      (defun +evil-display-vimlike-save-message-h ()
        "Shorter, vim-esque save messages."
        (message "\"%s\" %dL, %dC written"
                 (if buffer-file-name
                     (file-relative-name (file-truename buffer-file-name) (doom-project-root))
                   (buffer-name))
                 (count-lines (point-min) (point-max))
                 (buffer-size)))))
  ;; Make ESC (from normal mode) the universal escaper. See `doom-escape-hook'.
  (advice-add #'evil-force-normal-state :after #'+evil-escape-a)
  ;; Don't move cursor when indenting
  (advice-add #'evil-indent :around #'+evil--static-reindent-a)
  ;; monkey patch `evil-ex-replace-special-filenames' to improve support for
  ;; file modifiers like %:p:h. This adds support for most of vim's modifiers,
  ;; and one custom one: %:P (expand to the project root).
  (advice-add #'evil-ex-replace-special-filenames :override #'+evil-resolve-vim-path-a)

  ;; make `try-expand-dabbrev' (from `hippie-expand') work in minibuffer
  (add-hook 'minibuffer-inactive-mode-hook #'+evil--fix-dabbrev-in-minibuffer-h)

  ;; Focus and recenter new splits
  (advice-add #'evil-window-split  :override #'+evil-window-split-a)
  (advice-add #'evil-window-vsplit :override #'+evil-window-vsplit-a)

  ;; In evil, registers 2-9 are buffer-local. In vim, they're global, so...
  (advice-add #'evil-global-marker-p :around #'+evil--make-numbered-markers-global-a)

  ;; Make o/O continue comments (see `+evil-want-o/O-to-continue-comments')
  (advice-add #'evil-open-above :around #'+evil--insert-newline-above-and-respect-comments-a)
  (advice-add #'evil-open-below :around #'+evil--insert-newline-below-and-respect-comments-a)

  ;; Recenter screen after most searches
  (dolist (fn '(evil-visualstar/begin-search-forward
                evil-visualstar/begin-search-backward
                evil-ex-search-word-backward
                evil-ex-search-word-backward
                evil-ex-search-forward
                evil-ex-search-backward))
    (advice-add fn :after #'doom-recenter-a))

  ;; --- custom interactive codes -----------
  ;; These arg types will highlight matches in the current buffer
  (evil-ex-define-argument-type regexp-match
    :runner (lambda (flag &optional arg) (+evil-ex-regexp-match flag arg 'inverted)))
  (evil-ex-define-argument-type regexp-global-match
    :runner +evil-ex-regexp-match)

  (defun +evil--regexp-match-args (arg)
    (when (evil-ex-p)
      (cl-destructuring-bind (&optional arg flags)
          (evil-delimited-arguments arg 2)
        (list arg (string-to-list flags)))))

  ;; Other commands can make use of this
  (evil-define-interactive-code "<//>"
    :ex-arg regexp-match
    (+evil--regexp-match-args evil-ex-argument))

  (evil-define-interactive-code "<//!>"
    :ex-arg regexp-global-match
    (+evil--regexp-match-args evil-ex-argument))

  ;; Forward declare these so that ex completion works, even if the autoloaded
  ;; functions aren't loaded yet.
  (evil-add-command-properties '+evil:align :ex-arg 'regexp-match)
  (evil-add-command-properties '+evil:align-right :ex-arg 'regexp-match)
  (evil-add-command-properties '+multiple-cursors:evil-mc :ex-arg 'regexp-global-match)

  ;; `evil-collection'
  (when (and (featurep! +everywhere)
             (not doom-reloading-p))
    (load! "+everywhere"))

  ;; Lazy load evil ex commands
  (delq! 'evil-ex features)
  (add-transient-hook! 'evil-ex (provide 'evil-ex))
  (after! evil-ex (load! "+commands")))


;;
;; Packages

(use-package! evil-commentary
  :commands (evil-commentary
             evil-commentary-yank
             evil-commentary-yank-line
             evil-commentary-line)
  :config (evil-commentary-mode 1))


(use-package! evil-easymotion
  :commands evilem-create evilem-default-keybindings
  :config
  ;; Use evil-search backend, instead of isearch
  (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
                      :bind ((evil-ex-search-highlight-all nil)))

  (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
                      :bind ((evil-ex-search-highlight-all nil))))


(use-package! evil-embrace
  :commands embrace-add-pair embrace-add-pair-regexp
  :hook (LaTeX-mode . embrace-LaTeX-mode-hook)
  :hook (org-mode . embrace-org-mode-hook)
  :hook ((ruby-mode enh-ruby-mode) . embrace-ruby-mode-hook)
  :hook (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)
  :hook ((lisp-mode emacs-lisp-mode clojure-mode racket-mode)
         . +evil-embrace-lisp-mode-hook-h)
  :hook ((org-mode LaTeX-mode) . +evil-embrace-latex-mode-hook-h)
  :hook ((c++-mode rust-mode rustic-mode csharp-mode java-mode swift-mode typescript-mode)
         . +evil-embrace-angle-bracket-modes-hook-h)
  :init
  (after! evil-surround
    (evil-embrace-enable-evil-surround-integration))
  :config
  (setq evil-embrace-show-help-p nil)

  (defun +evil-embrace-latex-mode-hook-h ()
    (embrace-add-pair-regexp ?l "\\[a-z]+{" "}" #'+evil--embrace-latex))

  (defun +evil-embrace-lisp-mode-hook-h ()
    (push (cons ?f (make-embrace-pair-struct
                    :key ?f
                    :read-function #'+evil--embrace-elisp-fn
                    :left-regexp "([^ ]+ "
                    :right-regexp ")"))
          embrace--pairs-list))

  (defun +evil-embrace-angle-bracket-modes-hook-h ()
    (set (make-local-variable 'evil-embrace-evil-surround-keys)
         (delq ?< evil-embrace-evil-surround-keys))
    (push (cons ?< (make-embrace-pair-struct
                    :key ?<
                    :read-function #'+evil--embrace-angle-brackets
                    :left-regexp "\\[a-z]+<"
                    :right-regexp ">"))
          embrace--pairs-list))

  ;; Add escaped-sequence support to embrace
  (setf (alist-get ?\\ (default-value 'embrace--pairs-list))
        (make-embrace-pair-struct
         :key ?\\
         :read-function #'+evil--embrace-escaped
         :left-regexp "\\[[{(]"
         :right-regexp "\\[]})]")))


(use-package! evil-escape
  :commands evil-escape
  :after-call pre-command-hook
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.25)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  ;; no `evil-escape' in minibuffer
  (add-hook 'evil-escape-inhibit-functions #'minibufferp)
  ;; so that evil-escape-mode-hook runs, and can be toggled by evil-mc
  (evil-escape-mode +1))


(use-package! evil-exchange
  :commands evil-exchange
  :config
  (add-hook! 'doom-escape-hook
    (defun +evil--escape-exchange-h ()
      (when evil-exchange--overlays
        (evil-exchange-cancel)
        t))))


(use-package! evil-snipe
  :commands (evil-snipe-mode
             evil-snipe-override-mode
             evil-snipe-local-mode
             evil-snipe-override-local-mode)
  :after-call pre-command-hook
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  (pushnew! evil-snipe-disabled-modes 'Info-mode 'calc-mode)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))


(use-package! evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))


(use-package! evil-traces
  :after evil-ex
  :config
  (pushnew! evil-traces-argument-type-alist
            '(+evil:align . evil-traces-global)
            '(+evil:align-right . evil-traces-global))
  (evil-traces-mode))


;; Allows you to use the selection for * and #
(use-package! evil-visualstar
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))


;;
;;; Text object plugins

(use-package! exato
  :commands evil-outer-xml-attr evil-inner-xml-attr)
