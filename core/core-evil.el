;;; core-evil.el --- the root of all evil

(use-package evil
  :init
  (setq evil-magic t
        evil-want-C-u-scroll t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-want-visual-char-semi-exclusive t
        evil-ex-search-vim-style-regexp t
        evil-ex-interactive-search-highlight 'selected-window
        evil-echo-state nil
        evil-ex-substitute-global t
        evil-insert-skip-empty-lines t
        evil-want-fine-undo nil

        evil-normal-state-tag    "N"
        evil-insert-state-tag    "I"
        evil-visual-state-tag    "V"
        evil-emacs-state-tag     "E"
        evil-operator-state-tag  "O"
        evil-motion-state-tag    "M"
        evil-replace-state-tag   "R"

        ;; Color-coded state cursors
        evil-default-cursor (face-attribute 'minibuffer-prompt :foreground nil t)
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  `(,(face-attribute 'shadow :foreground nil nil) box)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow)

  ;; highlight matching delimiters where it's important
  (defun show-paren-mode-off () (show-paren-mode -1))
  (add-hook 'evil-insert-state-entry-hook   'show-paren-mode)
  (add-hook 'evil-insert-state-exit-hook    'show-paren-mode-off)
  (add-hook 'evil-visual-state-entry-hook   'show-paren-mode)
  (add-hook 'evil-visual-state-exit-hook    'show-paren-mode-off)
  (add-hook 'evil-operator-state-entry-hook 'show-paren-mode)
  (add-hook 'evil-operator-state-exit-hook  'show-paren-mode-off)
  (add-hook 'evil-normal-state-entry-hook   'show-paren-mode-off)
  ;; Disable highlights on insert-mode
  (add-hook 'evil-insert-state-entry-hook 'evil-ex-nohighlight)

  :config
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;; Reset evil-mode in the messages buffer, because it opens before evil
  ;; normalizes its keymaps, so none of the custom keybindings work in it.
  (add-hook! emacs-startup
    (with-current-buffer "*Messages*"
      (evil-mode -1)
      (evil-mode +1)))

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
          (grep-mode              . emacs)))

  ;; Popups
  (def-popup! "*evil-registers*" :align below :size 0.3)
  (def-popup! "*Command Line*"   :align below :size 8 :select t)

  ;;; Evil hacks
  ;; Don't interfere with neotree + auto move to new split
  (advice-add 'evil-window-split :around 'doom*evil-window-split)
  (advice-add 'evil-window-vsplit :around 'doom*evil-window-vsplit)
  ;; Integrate evil's command window into shackle
  (advice-add 'evil-command-window :override 'doom*evil-command-window)
  (add-hook 'evil-command-window-mode-hook 'doom-hide-mode-line-mode)
  ;; Close popups, disable search highlights and quit the minibuffer if open
  (advice-add 'evil-force-normal-state :after 'doom*evil-esc-quit)

  ;; Fix harmless (yet disruptive) error reporting w/ hidden buffers caused by
  ;; workgroups killing windows
  ;; TODO Delete timer on dead windows?
  (defadvice evil-ex-hl-do-update-highlight
      (around evil-ex-hidden-buffer-ignore-errors activate)
    (ignore-errors ad-do-it))

  ;; Hide keystroke display while isearch is active
  (add-hook! isearch-mode     (setq echo-keystrokes 0))
  (add-hook! isearch-mode-end (setq echo-keystrokes 0.02))

  (after! evil-snipe
    (def-repeat! evil-snipe-f evil-snipe-repeat evil-snipe-repeat-reverse)
    (def-repeat! evil-snipe-F evil-snipe-repeat evil-snipe-repeat-reverse)
    (def-repeat! evil-snipe-t evil-snipe-repeat evil-snipe-repeat-reverse)
    (def-repeat! evil-snipe-T evil-snipe-repeat evil-snipe-repeat-reverse)
    (def-repeat! evil-snipe-s evil-snipe-repeat evil-snipe-repeat-reverse)
    (def-repeat! evil-snipe-S evil-snipe-repeat evil-snipe-repeat-reverse)
    (def-repeat! evil-snipe-x evil-snipe-repeat evil-snipe-repeat-reverse)
    (def-repeat! evil-snipe-X evil-snipe-repeat evil-snipe-repeat-reverse))
  (after! evil-visualstar
    (def-repeat! evil-visualstar/begin-search-forward
      evil-ex-search-next evil-ex-search-previous)
    (def-repeat! evil-visualstar/begin-search-backward
      evil-ex-search-previous evil-ex-search-next))
  (def-repeat! evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
  (def-repeat! evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)
  (def-repeat! evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
  (def-repeat! evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)

  ;; monkey patch `evil-ex-replace-special-filenames' to add most of vim's file
  ;; ex substitution flags to evil-mode
  (advice-add 'evil-ex-replace-special-filenames
              :override 'doom*evil-ex-replace-special-filenames)

  ;; Extra argument types for highlight buffer (or global) regexp matches
  (evil-ex-define-argument-type buffer-match :runner doom/evil-ex-buffer-match)
  (evil-ex-define-argument-type global-match :runner doom/evil-ex-global-match)

  (evil-define-interactive-code "<//>"
    :ex-arg buffer-match
    (list (when (evil-ex-p) evil-ex-argument)))
  (evil-define-interactive-code "<g//>"
    :ex-arg global-match
    (when (evil-ex-p) (evil-ex-parse-global evil-ex-argument)))

  (evil-define-operator doom:evil-ex-global (beg end pattern command &optional invert)
    "Rewritten :g[lobal] that will highlight buffer matches. Takes the same arguments."
    :motion mark-whole-buffer :move-point nil
    (interactive "<r><g//><!>")
    (evil-ex-global beg end pattern command invert))

  (evil-define-operator doom:align (&optional beg end bang pattern)
    "Ex interface to `align-regexp'. Accepts vim-style regexps."
    (interactive "<r><!><//>")
    (align-regexp
     beg end
     (concat "\\(\\s-*\\)"
             (if bang
                 (regexp-quote pattern)
               (evil-transform-vim-style-regexp pattern)))
     1 1)))

;; evil plugins
(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

(use-package evil-anzu
  :defer 1
  :config
  (setq anzu-cons-mode-line-p nil
        anzu-minimum-input-length 1
        anzu-search-threshold 250))

(use-package evil-args
  :commands (evil-inner-arg evil-outer-arg evil-forward-arg evil-backward-arg evil-jump-out-args)
  :init (def-text-obj! "a" 'evil-inner-arg 'evil-outer-arg))

(use-package evil-commentary
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :config (evil-commentary-mode 1))

(use-package evil-exchange
  :commands evil-exchange
  :config (advice-add 'evil-force-normal-state :after 'doom*evil-exchange-off))

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
  :config (evil-multiedit-default-keybinds))

(use-package evil-indent-plus
  :commands (evil-indent-plus-i-indent
             evil-indent-plus-a-indent
             evil-indent-plus-i-indent-up
             evil-indent-plus-a-indent-up
             evil-indent-plus-i-indent-up-down
             evil-indent-plus-a-indent-up-down)
  :init
  (def-text-obj! "i" 'evil-indent-plus-i-indent 'evil-indent-plus-a-indent)
  (def-text-obj! "I" 'evil-indent-plus-i-indent-up 'evil-indent-plus-a-indent-up)
  (def-text-obj! "J" 'evil-indent-plus-i-indent-up-down 'evil-indent-plus-a-indent-up-down))

(use-package evil-matchit
  :commands (evilmi-jump-items evilmi-text-object global-evil-matchit-mode)
  :config (global-evil-matchit-mode 1)
  :init (def-text-obj! "%" 'evilmi-text-object))

(use-package evil-textobj-anyblock
  :commands (evil-textobj-anyblock-inner-block evil-textobj-anyblock-a-block)
  :init (def-text-obj! "B" 'evil-textobj-anyblock-inner-block 'evil-textobj-anyblock-a-block))

(use-package evil-search-highlight-persist
  :config
  (global-evil-search-highlight-persist t)
  (advice-add 'evil-force-normal-state :after 'evil-search-highlight-persist-remove-all))

(use-package evil-easymotion
  :defer 1
  :init (defvar doom--evil-snipe-repeat-fn)
  :config
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

(use-package evil-snipe
  :init
  (setq-default
   evil-snipe-smart-case t
   evil-snipe-repeat-keys nil ; using space to repeat
   evil-snipe-scope 'line
   evil-snipe-repeat-scope 'visible
   evil-snipe-override-evil-repeat-keys nil ; causes problems with remapped ;
   evil-snipe-aliases '((?\[ "[[{(]")
                        (?\] "[]})]")
                        (?\; "[;:]")))
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  (define-key evil-snipe-parent-transient-map (kbd "C-;") 'doom/evil-snipe-easymotion))

(use-package evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config
  (global-evil-surround-mode 1))

(use-package evil-embrace
  :after evil-surround
  :config
  (setq evil-embrace-show-help-p nil)
  (evil-embrace-enable-evil-surround-integration)
  (push (cons ?\\ (make-embrace-pair-struct
                   :key ?\\
                   :read-function 'doom/embrace-escaped
                   :left-regexp "\\[[{(]"
                   :right-regexp "\\[]})]"))
        (default-value 'embrace--pairs-list))

  (add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook)
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (add-hook! emacs-lisp-mode
    (embrace-add-pair ?\` "`" "'"))
  (add-hook! (emacs-lisp-mode lisp-mode)
    (embrace-add-pair-regexp ?f "([^ ]+ " ")" 'doom/embrace-elisp-fn))
  (add-hook! (org-mode latex-mode)
    (embrace-add-pair-regexp ?l "\\[a-z]+{" "}" 'doom/embrace-latex)))

(use-package evil-visualstar
  :commands (global-evil-visualstar-mode
             evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :config
  (global-evil-visualstar-mode 1))

(use-package evil-escape
  :config
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.25)
  (push 'neotree-mode evil-escape-excluded-major-modes)
  ;; evil-escape causes noticable lag in commands that start with j, so we
  ;; enable it only where we need it.
  (defun doom|evil-escape-disable () (evil-escape-mode -1))
  (defun doom|evil-escape-enable ()  (evil-escape-mode +1))
  (add-hook 'evil-insert-state-entry-hook 'doom|evil-escape-enable)
  (add-hook 'evil-insert-state-exit-hook  'doom|evil-escape-disable)
  (add-hook 'evil-replace-state-entry-hook 'doom|evil-escape-enable)
  (add-hook 'evil-replace-state-exit-hook  'doom|evil-escape-disable))

(provide 'core-evil)
;;; core-evil.el ends here
