;;; feature/evil/config.el

;; I'm a vimmer at heart. Its modal philosophy suits me better, and this module
;; strives to make Emacs a much better vim than vim was.

(def-setting! :evil-state (&rest mode-state-list)
  "Set the initialize STATE of MODE using `evil-set-initial-state'."
  (if (cl-every #'listp mode-state-list)
      `(progn
         ,@(let (forms)
             (dolist (it mode-state-list (nreverse forms))
               (unless (consp it)
                 (error ":evil-state expected cons cells, got %s" it))
               (push `(evil-set-initial-state ',(car it) ',(cdr it)) forms))))
    (let ((argc (length mode-state-list)))
      (unless (= argc 2)
        (error ":evil-state expected 2 arguments, got %s" argc)))
    `(evil-set-initial-state ',(car mode-state-list) ',(cadr mode-state-list))))


;;
;; evil-mode
;;

(def-package! evil :demand t
  :init
  (setq evil-want-C-u-scroll t
        evil-want-visual-char-semi-exclusive t
        evil-want-Y-yank-to-eol t
        evil-magic t
        evil-echo-state t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-insert-skip-empty-lines t
        evil-mode-line-format 'nil
        ;; Move to new split
        evil-split-window-below t
        evil-vsplit-window-right t
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; don't activate mark on shift-click
        shift-select-mode nil)

  :config
  (evil-mode +1)
  (evil-select-search-module 'evil-search-module 'evil-search)

  (set! :popup
    '("*evil-registers*" :size 0.3)
    '("*Command Line*" :size 8))

  ;; Don't interfere with localleader key
  (define-key evil-motion-state-map "\\" nil)

  ;; Set cursor colors later, once theme is loaded
  (defun +evil*init-cursors (&rest _)
    (setq evil-default-cursor (face-background 'cursor nil t)
          evil-normal-state-cursor 'box
          evil-emacs-state-cursor  `(,(face-foreground 'warning) box)
          evil-insert-state-cursor 'bar
          evil-visual-state-cursor 'hollow))
  (advice-add #'load-theme :after #'+evil*init-cursors)

  ;; default modes
  (dolist (mode '(tabulated-list-mode view-mode comint-mode term-mode calendar-mode Man-mode grep-mode))
    (evil-set-initial-state mode 'emacs))
  (dolist (mode '(help-mode debugger-mode))
    (evil-set-initial-state mode 'normal))

  ;; make `try-expand-dabbrev' from `hippie-expand' work in mini-buffer
  ;; @see `he-dabbrev-beg', so we need re-define syntax for '/'
  (defun minibuffer-inactive-mode-hook-setup ()
    (set-syntax-table (let* ((table (make-syntax-table)))
                        (modify-syntax-entry ?/ "." table)
                        table)))
  (add-hook 'minibuffer-inactive-mode-hook #'minibuffer-inactive-mode-hook-setup)

  (defsubst +evil--textobj (key inner-fn &optional outer-fn)
    "Define a text object."
    (declare (indent defun))
    (define-key evil-inner-text-objects-map key inner-fn)
    (define-key evil-outer-text-objects-map key (or outer-fn inner-fn)))


  ;; --- keybind fixes ----------------------
  (map! ;; undo/redo for visual regions
        :v "C-u" #'undo-tree-undo
        :v "C-r" #'undo-tree-redo

        (:after wgrep
          ;; a wrapper that invokes `wgrep-mark-deletion' across lines
          ;; you use `evil-delete' on.
          :map wgrep-mode-map [remap evil-delete] #'+evil-delete))


  ;; --- evil hacks -------------------------
  (defvar +evil-esc-hook nil
    "A hook run after ESC is pressed in normal mode (invoked by
`evil-force-normal-state').")

  (defun +evil*attach-escape-hook ()
    "Run the `+evil-esc-hook'."
    (run-hooks '+evil-esc-hook))
  (advice-add #'evil-force-normal-state :after #'+evil*attach-escape-hook)

  (defun +evil|escape-minibuffer ()
    "Quit the minibuffer if open."
    (when (minibuffer-window-active-p (minibuffer-window))
      (abort-recursive-edit)))

  (defun +evil|escape-highlights ()
    "Disable ex search buffer highlights."
    (when (evil-ex-hl-active-p 'evil-ex-search)
      (evil-ex-nohighlight)))

  (add-hook! '+evil-esc-hook '(+evil|escape-minibuffer +evil|escape-highlights))

  (defun +evil*restore-normal-state-on-windmove (orig-fn &rest args)
    "If in anything but normal or motion mode when moving to another window,
restore normal mode. This prevents insert state from bleeding into other modes
across windows."
    (unless (memq evil-state '(normal motion))
      (evil-normal-state +1))
    (apply orig-fn args))
  (advice-add #'windmove-do-window-select :around #'+evil*restore-normal-state-on-windmove)

  (defun +evil*static-reindent (orig-fn &rest args)
    "Don't move cursor on indent."
    (save-excursion (apply orig-fn args)))
  (advice-add #'evil-indent :around #'+evil*static-reindent)

  ;; monkey patch `evil-ex-replace-special-filenames' to add more ex
  ;; substitution flags to evil-mode
  (advice-add #'evil-ex-replace-special-filenames
              :override #'+evil*ex-replace-special-filenames)

  ;; By default :g[lobal] doesn't highlight matches in the current buffer. I've
  ;; got to write my own argument type and interactive code to get it to do so.
  ;; While I'm at it, I use this to write an :al[ign] command as a wrapper
  ;; around `align-regexp'.

  ;; TODO Must be simpler way to do this
  (evil-ex-define-argument-type buffer-match :runner +evil-ex-buffer-match)
  (evil-ex-define-argument-type global-match :runner +evil-ex-global-match)

  (evil-define-interactive-code "<//>"
    :ex-arg buffer-match (list (when (evil-ex-p) evil-ex-argument)))
  (evil-define-interactive-code "<g//>"
    :ex-arg global-match (when (evil-ex-p) (evil-ex-parse-global evil-ex-argument)))

  (evil-define-operator +evil:global (beg end pattern command &optional invert)
    "Rewritten :g[lobal] that will highlight buffer matches. Takes the same arguments."
    :motion mark-whole-buffer :move-point nil
    (interactive "<r><g//><!>")
    (evil-ex-global beg end pattern command invert))

  (evil-define-operator +evil:align (&optional beg end bang pattern)
    "Ex interface to `align-regexp'. Accepts vim-style regexps."
    (interactive "<r><!><//>")
    (align-regexp
     beg end
     (concat "\\(\\s-*\\)"
             (if bang
                 (regexp-quote pattern)
               (evil-transform-vim-style-regexp pattern)))
     1 1))

  ;; Must be aggressively defined here, otherwise the above highlighting won't
  ;; work on first invocation
  (evil-ex-define-cmd "g[lobal]" #'+evil:global)
  (evil-ex-define-cmd "al[ign]"  #'+evil:align))


;;
;; Plugins
;;

(def-package! evil-args
  :commands (evil-inner-arg evil-outer-arg
             evil-forward-arg evil-backward-arg
             evil-jump-out-args)
  :init (+evil--textobj "a" #'evil-inner-arg #'evil-outer-arg))


(def-package! evil-commentary
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :config (evil-commentary-mode 1))


(def-package! evil-easymotion
  :defer 1
  :commands evilem-define
  :config
  (let ((prefix "g SPC"))
    (evilem-default-keybindings prefix)
    (evilem-define (kbd (concat prefix " n")) #'evil-ex-search-next)
    (evilem-define (kbd (concat prefix " N")) #'evil-ex-search-previous)
    (evilem-define (kbd (concat prefix " s")) 'evil-snipe-repeat
                   :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                   :bind ((evil-snipe-scope 'buffer)
                          (evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight)))
    (evilem-define (kbd (concat prefix " S")) #'evil-snipe-repeat-reverse
                   :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                   :bind ((evil-snipe-scope 'buffer)
                          (evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight))))

  (defvar +evil--snipe-repeat-fn
    (evilem-create #'evil-snipe-repeat
                   :bind ((evil-snipe-scope 'whole-buffer)
                          (evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight)))))


(def-package! evil-embrace
  :after evil-surround
  :config
  (setq evil-embrace-show-help-p nil)
  (evil-embrace-enable-evil-surround-integration)

  ;; Defuns
  (defun +evil--embrace-get-pair (char)
    (if-let (pair (cdr-safe (assoc (string-to-char char) evil-surround-pairs-alist)))
        pair
      (if-let (pair (assoc-default char embrace--pairs-list))
          (if-let (real-pair (and (functionp (embrace-pair-struct-read-function pair))
                                  (funcall (embrace-pair-struct-read-function pair))))
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
  (push (cons ?\\ (make-embrace-pair-struct
                   :key ?\\
                   :read-function #'+evil--embrace-escaped
                   :left-regexp "\\[[{(]"
                   :right-regexp "\\[]})]"))
        (default-value 'embrace--pairs-list))

  ;; Add extra pairs
  (add-hook 'LaTeX-mode-hook #'embrace-LaTeX-mode-hook)
  (add-hook 'org-mode-hook   #'embrace-org-mode-hook)
  (add-hook! emacs-lisp-mode
    (embrace-add-pair ?\` "`" "'"))
  (add-hook! (emacs-lisp-mode lisp-mode)
    (embrace-add-pair-regexp ?f "([^ ]+ " ")" #'+evil--embrace-elisp-fn))
  (add-hook! (org-mode LaTeX-mode)
    (embrace-add-pair-regexp ?l "\\[a-z]+{" "}" #'+evil--embrace-latex)))


(def-package! evil-escape
  :demand t
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit)
        evil-escape-excluded-major-modes '(neotree-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.25)

  :config
  (evil-escape-mode +1)
  (map! :irvo "C-g" #'evil-escape))


(def-package! evil-exchange
  :commands evil-exchange
  :config
  (defun +evil|escape-exchange ()
    (if evil-exchange--overlays (evil-exchange-cancel)))
  (add-hook '+evil-esc-hook #'+evil|escape-exchange))


(def-package! evil-indent-plus
  :commands (evil-indent-plus-i-indent
             evil-indent-plus-a-indent
             evil-indent-plus-i-indent-up
             evil-indent-plus-a-indent-up
             evil-indent-plus-i-indent-up-down
             evil-indent-plus-a-indent-up-down)
  :init
  (+evil--textobj "i" #'evil-indent-plus-i-indent #'evil-indent-plus-a-indent)
  (+evil--textobj "I" #'evil-indent-plus-i-indent-up #'evil-indent-plus-a-indent-up)
  (+evil--textobj "J" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down))


(def-package! evil-matchit
  :commands (evilmi-jump-items evilmi-text-object global-evil-matchit-mode)
  :config (global-evil-matchit-mode 1)
  :init
  (map! :m "%" #'evilmi-jump-items)
  (+evil--textobj "%" #'evilmi-text-object)
  :config
  (defun +evil|simple-matchit ()
    "A hook to force evil-matchit to favor simple bracket jumping. Helpful when
the new algorithm is confusing, like in python or ruby."
    (setq-local evilmi-always-simple-jump t))
  (add-hook 'python-mode-hook #'+evil|simple-matchit))


(def-package! evil-mc
  :commands evil-mc-make-cursor-here
  :init (defvar evil-mc-key-map (make-sparse-keymap))
  :config
  ;; Start evil-mc in paused mode.
  (add-hook 'evil-mc-mode-hook #'evil-mc-pause-cursors)
  (add-hook 'evil-mc-before-cursors-created #'evil-mc-pause-cursors)

  (global-evil-mc-mode 1)
  (setq evil-mc-custom-known-commands
        '((doom/deflate-space-maybe . ((:default . evil-mc-execute-default-call)))))

 ;; My workflow is to place the cursors, get into position, then enable evil-mc
 ;; by invoking `+evil/mc-toggle-cursors'
  (defun +evil/mc-toggle-cursors ()
    "Toggle frozen state of evil-mc cursors."
    (interactive)
    (setq evil-mc-frozen (not (and (evil-mc-has-cursors-p)
                                   evil-mc-frozen))))
  ;; ...or going into insert mode
  (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors)

  (defun +evil|escape-multiple-cursors ()
    "Undo cursors and freeze them again (for next time)."
    (when (evil-mc-has-cursors-p)
      (evil-mc-undo-all-cursors)))
  (add-hook '+evil-esc-hook #'+evil|escape-multiple-cursors)

  ;; disable evil-escape in evil-mc
  (push 'evil-escape-mode evil-mc-incompatible-minor-modes))


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


(def-package! evil-textobj-anyblock
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :init
  (+evil--textobj "B"
    #'evil-textobj-anyblock-inner-block
    #'evil-textobj-anyblock-a-block))


(def-package! evil-snipe :demand t
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-override-evil-repeat-keys nil
        evil-snipe-char-fold t
        evil-snipe-aliases '((?\[ "[[{(]")
                             (?\] "[]})]")
                             (?\; "[;:]")))

  :config
  (evil-snipe-override-mode +1)
  ;; Switch to evil-easymotion/avy after a snipe
  (map! :map evil-snipe-parent-transient-map
        "C-;" (λ! (require 'evil-easymotion)
                  (call-interactively +evil--snipe-repeat-fn))))


(def-package! evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))


(def-package! evil-vimish-fold
  :commands evil-vimish-fold-mode
  :init
  (setq vimish-fold-dir (concat doom-cache-dir "vimish-fold/")
        vimish-fold-indication-mode 'right-fringe))


;; Without `evil-visualstar', * and # grab the word at point and search, no
;; matter what mode you're in. I want to be able to visually select a region and
;; search for other occurrences of it.
(def-package! evil-visualstar
  :commands (global-evil-visualstar-mode
             evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (map! :v "*" #'evil-visualstar/begin-search-forward
        :v "#" #'evil-visualstar/begin-search-backward)
  :config
  (global-evil-visualstar-mode 1))


;; A side-panel for browsing my project files. Inspired by vim's NERDTree. Sure,
;; there's dired and projectile, but sometimes I'd like a bird's eye view of a
;; project.
(def-package! neotree
  :commands (neotree-show
             neotree-hide
             neotree-toggle
             neotree-dir
             neotree-find
             neo-global--with-buffer
             neo-global--window-exists-p)
  :config
  (setq neo-create-file-auto-open nil
        neo-auto-indent-point nil
        neo-autorefresh nil
        neo-mode-line-type 'none
        neo-window-width 25
        neo-show-updir-line nil
        neo-theme 'nerd ; fallback
        neo-banner-message nil
        neo-confirm-create-file #'off-p
        neo-confirm-create-directory #'off-p
        neo-show-hidden-files nil
        neo-hidden-regexp-list
        '(;; vcs folders
          "^\\.\\(git\\|hg\\|svn\\)$"
          ;; compiled files
          "\\.\\(pyc\\|o\\|elc\\|lock\\|css.map\\)$"
          ;; generated files, caches or local pkgs
          "^\\(node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
          ;; org-mode folders
          "^\\.\\(sync\\|export\\|attach\\)$"
          "~$"
          "^#.*#$"))

  (evil-set-initial-state 'neotree-mode 'motion)

  (push neo-buffer-name winner-boring-buffers)

  ;; `neotree-mode-map' are overridden when the neotree buffer is created. So we
  ;; bind them in a hook.
  (add-hook 'neo-after-create-hook #'+evil|neotree-init-keymap)
  (defun +evil|neotree-init-keymap (&rest _)
    (map! :Lm "\\\\"     'evil-window-prev
          :Lm "RET"      'neotree-enter
          :Lm "<return>" 'neotree-enter
          :Lm "ESC ESC"  'neotree-hide
          :Lm [return]   'neotree-enter
          :Lm "q"        'neotree-hide
          :Lm "J"        'neotree-select-next-sibling-node
          :Lm "K"        'neotree-select-previous-sibling-node
          :Lm "H"        'neotree-select-up-node
          :Lm "L"        'neotree-select-down-node
          :Lm "h"        '+evil/neotree-collapse-or-up
          :Lm "j"        'neotree-next-line
          :Lm "k"        'neotree-previous-line
          :Lm "l"        '+evil/neotree-expand-or-open
          :Lm "v"        'neotree-enter-vertical-split
          :Lm "s"        'neotree-enter-horizontal-split
          :Lm "c"        'neotree-create-node
          :Lm "d"        'neotree-delete-node
          :Lm "\C-r"     'neotree-refresh
          :Lm "r"        'neotree-rename-node
          :Lm "R"        'neotree-change-root)))
