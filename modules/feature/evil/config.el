;;; feature/evil/config.el -*- lexical-binding: t; -*-

;; I'm a vimmer at heart. Its modal philosophy suits me better, and this module
;; strives to make Emacs a much better vim than vim was.

;; Set these defaults before `evil'; use `defvar' so they can be changed prior
;; to loading.
(defvar evil-want-C-u-scroll t)
(defvar evil-want-C-w-scroll t)
(defvar evil-want-Y-yank-to-eol t)

(def-package! evil
  :init
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
        ;; don't activate mark on shift-click
        shift-select-mode nil
        ;; cursor appearance
        evil-default-cursor '+evil-default-cursor
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  '(box +evil-emacs-cursor)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        ;; must be set before evil/evil-collection is loaded
        evil-want-integration (not (featurep! +everywhere)))

  :config
  (add-hook 'doom-post-init-hook #'evil-mode)
  (evil-select-search-module 'evil-search-module 'evil-search)

  (put 'evil-define-key* 'lisp-indent-function 'defun)

  (set-popup-rules!
    '(("^\\*evil-registers" :size 0.3)
      ("^\\*Command Line"   :size 8)))

  ;; Change the cursor color in emacs mode
  (defvar +evil--default-cursor-color "#ffffff")
  (defun +evil-default-cursor () (set-cursor-color +evil--default-cursor-color))
  (defun +evil-emacs-cursor ()   (set-cursor-color (face-foreground 'warning)))

  (defun +evil|update-cursor-color ()
    (setq +evil--default-cursor-color (face-background 'cursor)))
  (add-hook 'doom-load-theme-hook #'+evil|update-cursor-color)

  (defun +evil|update-shift-width ()
    (setq evil-shift-width tab-width))
  (add-hook 'after-change-major-mode-hook #'+evil|update-shift-width t)


  ;; --- keybind fixes ----------------------
  (after! wgrep
    ;; A wrapper that invokes `wgrep-mark-deletion' across lines you use
    ;; `evil-delete' in wgrep buffers.
    (define-key wgrep-mode-map [remap evil-delete] #'+evil-delete))

  ;; replace native folding commands
  (define-key! 'global
    [remap evil-toggle-fold]   #'+evil:fold-toggle
    [remap evil-close-fold]    #'+evil:fold-close
    [remap evil-open-fold]     #'+evil:fold-open
    [remap evil-open-fold-rec] #'+evil:fold-open
    [remap evil-close-folds]   #'+evil:fold-close-all
    [remap evil-open-folds]    #'+evil:fold-open-all)

  (defun +evil|disable-highlights ()
    "Disable ex search buffer highlights."
    (when (evil-ex-hl-active-p 'evil-ex-search)
      (evil-ex-nohighlight)
      t))
  (add-hook 'doom-escape-hook #'+evil|disable-highlights)


  ;; --- evil hacks -------------------------
  (defun +evil|save-buffer ()
    "Shorter, vim-esque save messages."
    (message "\"%s\" %dL, %dC written"
             (if buffer-file-name
                 (file-relative-name (file-truename buffer-file-name) (doom-project-root))
               (buffer-name))
             (count-lines (point-min) (point-max))
             (buffer-size)))
  (unless noninteractive
    (setq save-silently t)
    (add-hook 'after-save-hook #'+evil|save-buffer))
  ;; Make ESC (from normal mode) the universal escaper. See `doom-escape-hook'.
  (advice-add #'evil-force-normal-state :after #'+evil*escape)
  ;; Don't move cursor when indenting
  (advice-add #'evil-indent :around #'+evil*static-reindent)
  ;; monkey patch `evil-ex-replace-special-filenames' to improve support for
  ;; file modifiers like %:p:h. This adds support for most of vim's modifiers,
  ;; and one custom one: %:P (expand to the project root).
  (advice-add #'evil-ex-replace-special-filenames :override #'+evil*resolve-vim-path)

  ;; make `try-expand-dabbrev' from `hippie-expand' work in minibuffer. See
  ;; `he-dabbrev-beg', so we need to redefine syntax for '/'
  (defun +evil*fix-dabbrev-in-minibuffer ()
    (set-syntax-table (let* ((table (make-syntax-table)))
                        (modify-syntax-entry ?/ "." table)
                        table)))
  (add-hook 'minibuffer-inactive-mode-hook #'+evil*fix-dabbrev-in-minibuffer)

  ;; Focus and recenter new splits
  (advice-add #'evil-window-split  :override #'+evil*window-split)
  (advice-add #'evil-window-vsplit :override #'+evil*window-vsplit)

  ;; Ensure jump points are created
  (defun +evil*set-jump (&rest _)
    (evil-set-jump))
  (advice-add #'counsel-git-grep-action :before #'+evil*set-jump)

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
   '+evil:align :move-point t :ex-arg 'buffer-match :ex-bang t :evil-mc t :keep-visual t :suppress-operator t)
  (evil-set-command-properties
   '+evil:mc :move-point nil :ex-arg 'global-match :ex-bang t :evil-mc t)

  ;; `evil-collection'
  (when (featurep! +everywhere)
    (load! "+everywhere")))


;;
;; Plugins
;;

(def-package! evil-commentary
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
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
  :after evil-surround
  :commands (embrace-add-pair embrace-add-pair-regexp)
  :hook (LaTeX-mode . embrace-LaTeX-mode-hook)
  :hook (org-mode . embrace-org-mode-hook)
  :init
  ;; Add extra pairs
  (add-hook! emacs-lisp-mode
    (embrace-add-pair ?\` "`" "'"))
  (add-hook! (emacs-lisp-mode lisp-mode)
    (embrace-add-pair-regexp ?f "([^ ]+ " ")" #'+evil--embrace-elisp-fn))
  (add-hook! (org-mode LaTeX-mode)
    (embrace-add-pair-regexp ?l "\\[a-z]+{" "}" #'+evil--embrace-latex))
  :config
  (setq evil-embrace-show-help-p nil)
  (evil-embrace-enable-evil-surround-integration)

  (defun +evil--embrace-get-pair (char)
    (if-let* ((pair (cdr-safe (assoc (string-to-char char) evil-surround-pairs-alist))))
        pair
      (if-let* ((pair (assoc-default char embrace--pairs-list)))
          (if-let* ((real-pair (and (functionp (embrace-pair-struct-read-function pair))
                                    (funcall (embrace-pair-struct-read-function pair)))))
              real-pair
            (cons (embrace-pair-struct-left pair) (embrace-pair-struct-right pair)))
        (cons char char))))

  (defun +evil--embrace-escaped ()
    "Backslash-escaped surround character support for embrace."
    (let ((char (read-char "\\")))
      (if (eq char 27)
          (cons "" "")
        (let ((pair (+evil--embrace-get-pair (string char)))
              (text (if (sp-point-in-string) "\\\\%s" "\\%s")))
          (cons (format text (car pair))
                (format text (cdr pair)))))))

  (defun +evil--embrace-latex ()
    "LaTeX command support for embrace."
    (cons (format "\\%s{" (read-string "\\")) "}"))

  (defun +evil--embrace-elisp-fn ()
    "Elisp function support for embrace."
    (cons (format "(%s " (or (read-string "(") "")) ")"))

  ;; Add escaped-sequence support to embrace
  (setf (alist-get ?\\ (default-value 'embrace--pairs-list))
        (make-embrace-pair-struct
         :key ?\\
         :read-function #'+evil--embrace-escaped
         :left-regexp "\\[[{(]"
         :right-regexp "\\[]})]")))


(def-package! evil-escape
  :commands (evil-escape evil-escape-mode evil-escape-pre-command-hook)
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.25)
  (add-hook 'pre-command-hook #'evil-escape-pre-command-hook)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  ;; no `evil-escape' in minibuffer
  (add-hook 'evil-escape-inhibit-functions #'minibufferp))


(def-package! evil-exchange
  :commands evil-exchange
  :config
  (defun +evil|escape-exchange ()
    (when evil-exchange--overlays
      (evil-exchange-cancel)
      t))
  (add-hook 'doom-escape-hook #'+evil|escape-exchange))


(def-package! evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))


(def-package! evil-matchit
  :commands (evilmi-jump-items global-evil-matchit-mode
             evilmi-outer-text-object evilmi-inner-text-object)
  :config (global-evil-matchit-mode 1)
  :init
  (define-key! 'global [remap evil-jump-item] #'evilmi-jump-items)
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


(def-package! evil-multiedit
  :commands (evil-multiedit-match-all
             evil-multiedit-match-and-next
             evil-multiedit-match-and-prev
             evil-multiedit-match-symbol-and-next
             evil-multiedit-match-symbol-and-prev
             evil-multiedit-toggle-or-restrict-region
             evil-multiedit-next
             evil-multiedit-prev
             evil-multiedit-abort
             evil-multiedit-ex-match))


(def-package! evil-mc
  :commands (evil-mc-make-cursor-here evil-mc-make-all-cursors
             evil-mc-undo-all-cursors evil-mc-pause-cursors
             evil-mc-resume-cursors evil-mc-make-and-goto-first-cursor
             evil-mc-make-and-goto-last-cursor
             evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line evil-mc-make-cursor-at-pos
             evil-mc-has-cursors-p evil-mc-make-and-goto-next-cursor
             evil-mc-skip-and-goto-next-cursor evil-mc-make-and-goto-prev-cursor
             evil-mc-skip-and-goto-prev-cursor evil-mc-make-and-goto-next-match
             evil-mc-skip-and-goto-next-match evil-mc-skip-and-goto-next-match
             evil-mc-make-and-goto-prev-match evil-mc-skip-and-goto-prev-match)
  :init
  (defvar evil-mc-key-map (make-sparse-keymap))
  :config
  (global-evil-mc-mode +1)

  (after! smartparens
    ;; Make evil-mc cooperate with smartparens better
    (let ((vars (cdr (assq :default evil-mc-cursor-variables))))
      (unless (memq (car sp--mc/cursor-specific-vars) vars)
        (setcdr (assq :default evil-mc-cursor-variables)
                (append vars sp--mc/cursor-specific-vars)))))

  ;; Add custom commands to whitelisted commands
  (dolist (fn '(doom/backward-to-bol-or-indent doom/forward-to-last-non-comment-or-eol
                doom/backward-kill-to-bol-and-indent))
    (add-to-list 'evil-mc-custom-known-commands `(,fn (:default . evil-mc-execute-default-call))))

  ;; Activate evil-mc cursors upon switching to insert mode
  (defun +evil-mc|resume-cursors () (setq evil-mc-frozen nil))
  (add-hook 'evil-insert-state-entry-hook #'+evil-mc|resume-cursors)

  ;; disable evil-escape in evil-mc; causes unwanted text on invocation
  (add-to-list 'evil-mc-incompatible-minor-modes 'evil-escape-mode nil #'eq)

  (defun +evil|escape-multiple-cursors ()
    "Clear evil-mc cursors and restore state."
    (when (evil-mc-has-cursors-p)
      (evil-mc-undo-all-cursors)
      (evil-mc-resume-cursors)
      t))
  (add-hook 'doom-escape-hook #'+evil|escape-multiple-cursors))


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


(def-package! evil-vimish-fold
  :commands (evil-vimish-fold/next-fold evil-vimish-fold/previous-fold
             evil-vimish-fold/delete evil-vimish-fold/delete-all
             evil-vimish-fold/create evil-vimish-fold/create-line)
  :init
  (setq vimish-fold-dir (concat doom-cache-dir "vimish-fold/")
        vimish-fold-indication-mode 'right-fringe)
  :config
  (vimish-fold-global-mode +1))


;; Without `evil-visualstar', * and # grab the word at point and search, no
;; matter what mode you're in. I want to be able to visually select a region and
;; search for other occurrences of it.
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
;;

(def-package! evil-args
  :commands (evil-inner-arg evil-outer-arg
             evil-forward-arg evil-backward-arg
             evil-jump-out-args)
  :config
  (unless (member "<" evil-args-openers)
    (push "<" evil-args-openers)
    (push ">" evil-args-closers)))

(def-package! exato
  :commands (evil-outer-xml-attr evil-inner-xml-attr))


;;
;; Multiple cursors compatibility (for the plugins that use it)
;;

;; mc doesn't play well with evil, this attempts to assuage some of its problems
;; so that any plugins that depend on multiple-cursors (which I have no control
;; over) can still use it in relative safety.
(after! multiple-cursors-core
  (evil-define-key* '(normal emacs) mc/keymap [escape] #'mc/keyboard-quit)

  (defvar +evil--mc-compat-evil-prev-state nil)
  (defvar +evil--mc-compat-mark-was-active nil)

  (defun +evil|mc-compat-switch-to-emacs-state ()
    (when (and (bound-and-true-p evil-mode)
               (not (memq evil-state '(insert emacs))))
      (setq +evil--mc-compat-evil-prev-state evil-state)
      (when (region-active-p)
        (setq +evil--mc-compat-mark-was-active t))
      (let ((mark-before (mark))
            (point-before (point)))
        (evil-emacs-state 1)
        (when (or +evil--mc-compat-mark-was-active (region-active-p))
          (goto-char point-before)
          (set-mark mark-before)))))

  (defun +evil|mc-compat-back-to-previous-state ()
    (when +evil--mc-compat-evil-prev-state
      (unwind-protect
          (case +evil--mc-compat-evil-prev-state
            ((normal visual) (evil-force-normal-state))
            (t (message "Don't know how to handle previous state: %S"
                        +evil--mc-compat-evil-prev-state)))
        (setq +evil--mc-compat-evil-prev-state nil)
        (setq +evil--mc-compat-mark-was-active nil))))

  (add-hook 'multiple-cursors-mode-enabled-hook #'+evil|mc-compat-switch-to-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook #'+evil|mc-compat-back-to-previous-state)

  ;; When running edit-lines, point will return (position + 1) as a
  ;; result of how evil deals with regions
  (defun +evil*mc/edit-lines (&rest _)
    (when (and (bound-and-true-p evil-mode)
               (not (memq evil-state '(insert emacs))))
      (if (> (point) (mark))
          (goto-char (1- (point)))
        (push-mark (1- (mark))))))
  (advice-add #'mc/edit-lines :before #'+evil*mc/edit-lines)

  (defun +evil|mc-evil-compat-rect-switch-state ()
    (if rectangular-region-mode
        (+evil|mc-compat-switch-to-emacs-state)
      (setq +evil--mc-compat-evil-prev-state nil)))
  (add-hook 'rectangular-region-mode-hook '+evil|mc-evil-compat-rect-switch-state)

  (defvar mc--default-cmds-to-run-once nil))
