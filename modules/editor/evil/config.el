;;; editor/evil/config.el -*- lexical-binding: t; -*-

;; I'm a vimmer at heart. Its modal philosophy suits me better, and this module
;; strives to make Emacs a much better vim than vim was.

(defvar +evil-want-o/O-to-continue-comments t
  "If non-nil, the o/O keys will continue comment lines if the point is on a
line with a linewise comment.")

;; Set these defaults before `evil'; use `defvar' so they can be changed prior
;; to loading.
(defvar evil-want-C-i-jump (or (daemonp) (display-graphic-p)))
(defvar evil-want-C-u-scroll t)
(defvar evil-want-C-w-scroll t)
(defvar evil-want-Y-yank-to-eol t)

(def-package! evil
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
        evil-default-cursor '+evil-default-cursor
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  '(box +evil-emacs-cursor)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        ;; must be set before evil/evil-collection is loaded
        evil-want-keybinding (not (featurep! +everywhere)))

  :config
  (evil-select-search-module 'evil-search-module 'evil-search)

  (put 'evil-define-key* 'lisp-indent-function 'defun)

  ;; Start help-with-tutorial in emacs state
  (advice-add #'help-with-tutorial :after (lambda (&rest _) (evil-emacs-state +1)))

  ;; Done in a hook to ensure the popup rules load as late as possible
  (defun +evil|init-popup-rules ()
    (set-popup-rules!
      '(("^\\*evil-registers" :size 0.3)
        ("^\\*Command Line"   :size 8))))
  (add-hook 'doom-init-modules-hook #'+evil|init-popup-rules)

  ;; Change the cursor color in emacs state. We do it this roundabout way
  ;; instead of changing `evil-default-cursor' (or `evil-emacs-state-cursor') so
  ;; it won't interfere with users who have changed these variables.
  (defvar +evil--default-cursor-color "#ffffff")
  (defvar +evil--emacs-cursor-color "#ff9999")

  (defun +evil|update-cursor-color ()
    (setq +evil--default-cursor-color (face-background 'cursor)
          +evil--emacs-cursor-color (face-foreground 'warning)))
  (add-hook 'doom-load-theme-hook #'+evil|update-cursor-color)

  (defun +evil-default-cursor ()
    (evil-set-cursor-color +evil--default-cursor-color))
  (defun +evil-emacs-cursor ()
    (evil-set-cursor-color +evil--emacs-cursor-color))

  (defun +evil|update-shift-width ()
    (setq evil-shift-width tab-width))
  (add-hook 'after-change-major-mode-hook #'+evil|update-shift-width t)


  ;; --- keybind fixes ----------------------
  (after! wgrep
    ;; A wrapper that invokes `wgrep-mark-deletion' across lines you use
    ;; `evil-delete' in wgrep buffers.
    (define-key wgrep-mode-map [remap evil-delete] #'+evil-delete))

  (defun +evil|disable-highlights ()
    "Disable ex search buffer highlights."
    (when (evil-ex-hl-active-p 'evil-ex-search)
      (evil-ex-nohighlight)
      t))
  (add-hook 'doom-escape-hook #'+evil|disable-highlights)


  ;; --- evil hacks -------------------------
  (defun +evil|display-vimlike-save-message ()
    "Shorter, vim-esque save messages."
    (message "\"%s\" %dL, %dC written"
             (if buffer-file-name
                 (file-relative-name (file-truename buffer-file-name) (doom-project-root))
               (buffer-name))
             (count-lines (point-min) (point-max))
             (buffer-size)))
  (unless noninteractive
    (setq save-silently t)
    (add-hook 'after-save-hook #'+evil|display-vimlike-save-message))
  ;; Make ESC (from normal mode) the universal escaper. See `doom-escape-hook'.
  (advice-add #'evil-force-normal-state :after #'+evil*escape)
  ;; Don't move cursor when indenting
  (advice-add #'evil-indent :around #'+evil*static-reindent)
  ;; monkey patch `evil-ex-replace-special-filenames' to improve support for
  ;; file modifiers like %:p:h. This adds support for most of vim's modifiers,
  ;; and one custom one: %:P (expand to the project root).
  (advice-add #'evil-ex-replace-special-filenames :override #'+evil*resolve-vim-path)

  ;; make `try-expand-dabbrev' (from `hippie-expand') work in minibuffer
  (add-hook 'minibuffer-inactive-mode-hook #'+evil*fix-dabbrev-in-minibuffer)

  ;; Focus and recenter new splits
  (advice-add #'evil-window-split  :override #'+evil*window-split)
  (advice-add #'evil-window-vsplit :override #'+evil*window-vsplit)

  ;; In evil, registers 2-9 are buffer-local. In vim, they're global, so...
  (advice-add #'evil-global-marker-p :around #'+evil*make-numbered-markers-global)

  ;; Make o/O continue comments (see `+evil-want-o/O-to-continue-comments')
  (advice-add #'evil-open-above :around #'+evil*insert-newline-above-and-respect-comments)
  (advice-add #'evil-open-below :around #'+evil*insert-newline-below-and-respect-comments)

  ;; Recenter screen after most searches
  (advice-add! '(evil-visualstar/begin-search-forward
                 evil-visualstar/begin-search-backward
                 evil-ex-search-word-backward
                 evil-ex-search-word-backward
                 evil-ex-search-forward
                 evil-ex-search-backward)
               :after #'doom*recenter)

  ;; --- custom interactive codes -----------
  ;; These arg types will highlight matches in the current buffer
  (evil-ex-define-argument-type buffer-match :runner +evil-ex-buffer-match)
  (evil-ex-define-argument-type global-match :runner +evil-ex-global-match)
  ;; Other commands can make use of this
  (evil-define-interactive-code "<//>"
    :ex-arg buffer-match (list (if (evil-ex-p) evil-ex-argument)))
  (evil-define-interactive-code "<//g>"
    :ex-arg global-match (list (if (evil-ex-p) evil-ex-argument)))

  ;; By default :g[lobal] doesn't highlight matches in the current buffer. I've
  ;; got to write my own argument type and interactive code to get it to do so.
  (evil-ex-define-argument-type global-delim-match :runner +evil-ex-global-delim-match)
  (dolist (sym '(evil-ex-global evil-ex-global-inverted))
    (evil-set-command-property sym :ex-arg 'global-delim-match))

  ;; Forward declare these so that ex completion works, even if the autoloaded
  ;; functions aren't loaded yet.
  (evil-set-command-properties
   '+evil:align :move-point t :ex-arg 'buffer-match :ex-bang t :keep-visual t :suppress-operator t)

  ;; `evil-collection'
  (when (and (featurep! +everywhere)
             (not doom-reloading-p))
    (load! "+everywhere"))

  ;; Custom evil ex commands
  (load! "+commands"))


;;
;; Packages

(def-package! evil-commentary
  :commands (evil-commentary
             evil-commentary-yank
             evil-commentary-yank-line
             evil-commentary-line)
  :config (evil-commentary-mode 1))


(def-package! evil-easymotion
  :commands (evilem-create evilem-default-keybindings)
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


(def-package! evil-embrace
  :commands (embrace-add-pair embrace-add-pair-regexp)
  :hook (LaTeX-mode . embrace-LaTeX-mode-hook)
  :hook (org-mode . embrace-org-mode-hook)
  :hook ((ruby-mode enh-ruby-mode) . embrace-ruby-mode-hook)
  :hook (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)
  :hook ((emacs-lisp-mode lisp-mode) . +evil|embrace-lisp-mode-hook)
  :hook ((org-mode LaTeX-mode) . +evil|embrace-latex-mode-hook)
  :init
  (after! evil-surround
    (evil-embrace-enable-evil-surround-integration))
  :config
  (setq evil-embrace-show-help-p nil)

  (defun +evil|embrace-latex-mode-hook ()
    (embrace-add-pair-regexp ?l "\\[a-z]+{" "}" #'+evil--embrace-latex))

  (defun +evil|embrace-lisp-mode-hook ()
    (push (cons ?f (make-embrace-pair-struct
                    :key ?f
                    :read-function #'+evil--embrace-elisp-fn
                    :left-regexp "([^ ]+ "
                    :right-regexp ")"))
          embrace--pairs-list))

  ;; Add escaped-sequence support to embrace
  (setf (alist-get ?\\ (default-value 'embrace--pairs-list))
        (make-embrace-pair-struct
         :key ?\\
         :read-function #'+evil--embrace-escaped
         :left-regexp "\\[[{(]"
         :right-regexp "\\[]})]")))


(def-package! evil-escape
  :commands (evil-escape)
  :after-call (evil-normal-state-exit-hook)
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode term-mode vterm-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.25)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  ;; no `evil-escape' in minibuffer
  (add-hook 'evil-escape-inhibit-functions #'minibufferp)
  ;; so that evil-escape-mode-hook runs, and can be toggled by evil-mc
  (evil-escape-mode +1))


(def-package! evil-exchange
  :commands evil-exchange
  :config
  (defun +evil|escape-exchange ()
    (when evil-exchange--overlays
      (evil-exchange-cancel)
      t))
  (add-hook 'doom-escape-hook #'+evil|escape-exchange))


(def-package! evil-matchit
  :commands (evilmi-jump-items global-evil-matchit-mode
             evilmi-outer-text-object evilmi-inner-text-object)
  :config (global-evil-matchit-mode 1)
  :init
  (global-set-key [remap evil-jump-item] #'evilmi-jump-items)
  (define-key evil-inner-text-objects-map "%" #'evilmi-inner-text-object)
  (define-key evil-outer-text-objects-map "%" #'evilmi-outer-text-object)
  :config
  ;; Fixes #519 where d% wouldn't leave a dangling end-parenthesis
  (evil-set-command-properties 'evilmi-jump-items :type 'inclusive :jump t)

  (defun +evil|simple-matchit ()
    "A hook to force evil-matchit to favor simple bracket jumping. Helpful when
the new algorithm is confusing, like in python or ruby."
    (setq-local evilmi-always-simple-jump t))
  (add-hook 'python-mode-hook #'+evil|simple-matchit))


(def-package! evil-snipe
  :commands (evil-snipe-mode evil-snipe-override-mode
             evil-snipe-local-mode evil-snipe-override-local-mode)
  :after-call pre-command-hook
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  (add-to-list 'evil-snipe-disabled-modes 'Info-mode nil #'eq)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))


(def-package! evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))


;; Allows you to use the selection for * and #
(def-package! evil-visualstar
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))


;;
;; Text object plugins

(def-package! exato
  :commands (evil-outer-xml-attr evil-inner-xml-attr))
