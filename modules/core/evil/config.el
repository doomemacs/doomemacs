;;; core-evil.el --- come to the dark side, we have cookies

;; I'm a vimmer at heart. Its modal philosophy suits me better, and this module
;; strives to make Emacs a much better vim than vim was.

(defvar +evil-leader ","
  "The <leader> key, used by the `map!' macro for :leader bindings.")

(defvar +evil-localleader "\\"
  "The <localleader> key, used by the `map!' macro for :localleader bindings.")


;;
;; evil-mode
;;

(use-package evil
  :demand t
  :init
  (setq evil-want-C-u-scroll t
        evil-want-visual-char-semi-exclusive t
        evil-want-fine-undo nil
        evil-want-Y-yank-to-eol t
        evil-magic t
        evil-echo-state t
        evil-ex-interactive-search-highlight 'selected-window
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-insert-skip-empty-lines t)

  :config
  (defpopup!
    ("*evil-registers*" :size 0.3)
    ("*Command Line*" :size 8))

  (evil-mode +1)
  (evil-select-search-module 'evil-search-module 'evil-search)

  (mapc (lambda (r) (evil-set-initial-state (car r) (cdr r)))
        '((compilation-mode       . normal)
          (help-mode              . normal)
          (message-mode           . normal)
          (debugger-mode          . normal)
          (image-mode             . normal)
          (doc-view-mode          . normal)
          (eww-mode               . normal)
          (tabulated-list-mode    . emacs)
          (profile-report-mode    . emacs)
          (Info-mode              . emacs)
          (view-mode              . emacs)
          (comint-mode            . emacs)
          (cider-repl-mode        . emacs)
          (term-mode              . emacs)
          (calendar-mode          . emacs)
          (Man-mode               . emacs)
          (grep-mode              . emacs))))

;;; Private macros
(defsubst +evil--textobj! (key inner-fn &optional outer-fn)
  "Define a text object."
  (define-key evil-inner-text-objects-map key inner-fn)
  (define-key evil-outer-text-objects-map key (or outer-fn inner-fn)))

;; Shortcuts for the evil expression register
(defmacro $= (str &rest args) `(calc-eval (format ,str ,@args)))
(defmacro $r (char) `(evil-get-register ,char))
(defmacro $expand (path) `(evil-ex-replace-special-filenames ,path))


;;
;; evil hacks
;;

(defun +evil*esc ()
  "Disable search highlights and quit the minibuffer if open."
  (when (minibuffer-window-active-p (minibuffer-window))
    (abort-recursive-edit))
  (when (evil-ex-hl-active-p 'evil-ex-search)
    (evil-ex-nohighlight)))
(advice-add 'evil-force-normal-state :after '+evil*esc)

;; Move to new split
(defun +evil*window-follow (&rest _)  (evil-window-down 1))
(defun +evil*window-vfollow (&rest _) (evil-window-right 1))
(advice-add 'evil-window-split  :after '+evil*window-follow)
(advice-add 'evil-window-vsplit :after '+evil*window-vfollow)

;; Fix harmless (yet disruptive) error reporting w/ hidden buffers caused by
;; workgroups killing windows
;; TODO Delete timer on dead windows?
;; (defun doom*ignore-errors (orig-fn &rest args)
;;   (ignore-errors (apply orig-fn args)))
;; (advice-add 'evil-ex-hl-do-update-highlight :around 'doom*ignore-errors)

;; monkey patch `evil-ex-replace-special-filenames' to add more ex
;; substitution flags to evil-mode
(advice-add 'evil-ex-replace-special-filenames
            :override '+evil*ex-replace-special-filenames)

;; Add extra argument types that highlight matches in the current buffer.
(evil-ex-define-argument-type buffer-match :runner doom-evil-ex-buffer-match)
(evil-ex-define-argument-type global-match :runner doom-evil-ex-global-match)

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


;;
;; Plugins
;;

(use-package evil-anzu
  :init
  ;; evil-anzu is strangely slow on startup. Byte compiling doesn't help. We use
  ;; this to lazy load it instead.
  ;; (defun doom*evil-search (&rest _)
  ;;   (require 'evil-anzu)
  ;;   (advice-remove 'evil-ex-start-search 'doom*evil-search))
  ;; (advice-add 'evil-ex-start-search :before 'doom*evil-search)

  :config
  (setq anzu-cons-mode-line-p nil
        anzu-minimum-input-length 1
        anzu-search-threshold 250))


(use-package evil-args
  :commands (evil-inner-arg evil-outer-arg evil-forward-arg evil-backward-arg
             evil-jump-out-args)
  :init
  (+evil--textobj! "a" 'evil-inner-arg 'evil-outer-arg))


(use-package evil-commentary
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :config! (evil-commentary-mode 1))


(use-package evil-easymotion
  :defer 1
  :config
  (defvar doom--evil-snipe-repeat-fn)

  (evilem-default-keybindings "g SPC")
  (evilem-define (kbd "g SPC n") 'evil-ex-search-next)
  (evilem-define (kbd "g SPC N") 'evil-ex-search-previous)
  (evilem-define "gs" 'evil-snipe-repeat
                 :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight)))
  (evilem-define "gS" 'evil-snipe-repeat-reverse
                 :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight)))

  (setq doom--evil-snipe-repeat-fn
        (evilem-create 'evil-snipe-repeat
                       :bind ((evil-snipe-scope 'whole-buffer)
                              (evil-snipe-enable-highlight)
                              (evil-snipe-enable-incremental-highlight)))))


(use-package evil-embrace
  :after evil-surround
  :config
  (setq evil-embrace-show-help-p nil)
  (evil-embrace-enable-evil-surround-integration)

  ;; Defuns
  (defun +evil--embrace-get-pair (char)
    (acond ((cdr-safe (assoc (string-to-char char) evil-surround-pairs-alist))
            `(,(car it) . ,(cdr it)))
           ((assoc-default char embrace--pairs-list)
            (if (functionp (embrace-pair-struct-read-function it))
                (let ((pair (funcall (embrace-pair-struct-read-function it))))
                  `(,(car pair) . ,(cdr pair)))
              `(,(embrace-pair-struct-left it) . ,(embrace-pair-struct-right it))))
           (t `(,char . ,char))))

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
                   :read-function '+evil--embrace-escaped
                   :left-regexp "\\[[{(]"
                   :right-regexp "\\[]})]"))
        (default-value 'embrace--pairs-list))

  ;; Add extra pairs
  (add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook)
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (add-hook! emacs-lisp-mode
    (embrace-add-pair ?\` "`" "'"))
  (add-hook! (emacs-lisp-mode lisp-mode)
    (embrace-add-pair-regexp ?f "([^ ]+ " ")" '+evil--embrace-elisp-fn))
  (add-hook! (org-mode latex-mode)
    (embrace-add-pair-regexp ?l "\\[a-z]+{" "}" '+evil--embrace-latex)))


(use-package evil-escape
  :commands evil-escape-mode
  :init
  (defun +evil|escape-disable () (evil-escape-mode -1))
  (defun +evil|escape-enable ()  (evil-escape-mode +1))
  ;; I only need evil-escape in insert and replace modes.
  (add-hook 'evil-insert-state-entry-hook  '+evil|escape-enable)
  (add-hook 'evil-insert-state-exit-hook   '+evil|escape-disable)
  (add-hook 'evil-replace-state-entry-hook '+evil|escape-enable)
  (add-hook 'evil-replace-state-exit-hook  '+evil|escape-disable)
  :config
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.25))


(use-package evil-exchange
  :commands evil-exchange
  :config
  (defun +evil*exchange-off ()
    (if evil-exchange--overlays (evil-exchange-cancel)))
  (advice-add 'evil-force-normal-state :after '+evil*exchange-off))


(use-package evil-indent-plus
  :commands (evil-indent-plus-i-indent
             evil-indent-plus-a-indent
             evil-indent-plus-i-indent-up
             evil-indent-plus-a-indent-up
             evil-indent-plus-i-indent-up-down
             evil-indent-plus-a-indent-up-down)
  :init
  (+evil--textobj! "i" 'evil-indent-plus-i-indent 'evil-indent-plus-a-indent)
  (+evil--textobj! "I" 'evil-indent-plus-i-indent-up 'evil-indent-plus-a-indent-up)
  (+evil--textobj! "J" 'evil-indent-plus-i-indent-up-down 'evil-indent-plus-a-indent-up-down))


(use-package evil-matchit
  :commands (evilmi-jump-items evilmi-text-object global-evil-matchit-mode)
  :config (global-evil-matchit-mode 1)
  :init
  (+evil--textobj! "%" 'evilmi-text-object)

  (defun +evil/matchit-or-toggle-fold ()
    "If on a fold-able element, toggle the fold (`hs-toggle-hiding'). Otherwise,
if on a delimiter, jump to the matching one (`evilmi-jump-items')."
    (interactive)
    (if (ignore-errors (hs-already-hidden-p))
        (hs-toggle-hiding)
      (call-interactively 'evilmi-jump-items))))


(use-package evil-multiedit
  :commands (evil-multiedit-match-all
             evil-multiedit-match-and-next
             evil-multiedit-match-and-prev
             evil-multiedit-match-symbol-and-next
             evil-multiedit-match-symbol-and-prev
             evil-multiedit-toggle-or-restrict-region
             evil-multiedit-next
             evil-multiedit-prev
             evil-multiedit-abort
             evil-multiedit-ex-match)
  :config
  (evil-multiedit-default-keybinds))


(use-package evil-textobj-anyblock
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :init
  (+evil--textobj! "B" 'evil-textobj-anyblock-inner-block 'evil-textobj-anyblock-a-block))


(use-package evil-search-highlight-persist
  :demand t
  :commands (evil-textobj-anyblock-inner-block evil-textobj-anyblock-a-block)
  :config
  (global-evil-search-highlight-persist t)
  (advice-add 'evil-force-normal-state :after 'evil-search-highlight-persist-remove-all))


(use-package evil-snipe
  :demand t
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-repeat-keys nil ; using space to repeat
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-override-evil-repeat-keys nil ; causes problems with remapped ;
        evil-snipe-char-fold t
        evil-snipe-aliases '((?\[ "[[{(]")
                             (?\] "[]})]")
                             (?\; "[;:]")))

  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  ;; Switch to evil-easymotion/avy after first snipe
  (define-key evil-snipe-parent-transient-map "\C-;"
    (λ! (require 'evil-easymotion)
        (call-interactively doom--evil-snipe-repeat-fn))))


(use-package evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))


(use-package evil-visualstar
  :commands (global-evil-visualstar-mode
             evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :config (global-evil-visualstar-mode 1))


;; A side-panel for browsing my project files. Inspired by vim's NERDTree.
(use-package neotree
  :commands (neotree-show
             neotree-hide
             neotree-toggle
             neotree-dir
             neotree-find
             neo-global--with-buffer
             neo-global--window-exists-p)
  :init
  (setq neo-create-file-auto-open t
        neo-auto-indent-point nil
        neo-mode-line-type 'none
        neo-persist-show nil
        neo-window-width 25
        neo-show-updir-line nil
        neo-theme 'nerd ; fallback
        neo-banner-message nil
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

  :config
  (evil-set-initial-state 'neotree-mode 'motion)

  ;; Adding keybindings to `neotree-mode-map' wouldn't work for me (they get
  ;; overridden when the neotree buffer is spawned). So we bind them in a hook.
  (add-hook 'neo-after-create-hook 'doom|neotree-init-keymap)
  (defun doom|neotree-init-keymap (&rest _)
    (let ((map evil-motion-state-local-map))
      (define-key map (kbd "\\\\")     'evil-window-prev)
      (define-key map (kbd "RET")      'neotree-enter)
      (define-key map (kbd "<return>") 'neotree-enter)
      (define-key map (kbd "ESC ESC")  'neotree-hide)
      (define-key map [return]         'neotree-enter)
      (define-key map "q"              'neotree-hide)
      (define-key map "J"              'neotree-select-next-sibling-node)
      (define-key map "K"              'neotree-select-previous-sibling-node)
      (define-key map "H"              'neotree-select-up-node)
      (define-key map "L"              'neotree-select-down-node)
      (define-key map "v"              'neotree-enter-vertical-split)
      (define-key map "s"              'neotree-enter-horizontal-split)
      (define-key map "c"              'neotree-create-node)
      (define-key map "d"              'neotree-delete-node)
      (define-key map "\C-r"           'neotree-refresh)
      (define-key map "r"              'neotree-rename-node)
      (define-key map "R"              'neotree-change-root))))

