;;; core-evil.el --- the root of all evil

(use-package evil
  :init
  ;; Disable highlights on insert-mode
  (add-hook 'evil-insert-state-entry-hook 'evil-ex-nohighlight)
  (setq evil-magic t
        evil-want-C-u-scroll t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-want-visual-char-semi-exclusive t
        evil-ex-search-vim-style-regexp t
        evil-ex-interactive-search-highlight 'selected-window
        evil-echo-state nil
        evil-ex-substitute-global t
        evil-insert-skip-empty-lines t

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
        evil-emacs-state-cursor  `(,(face-attribute 'highlight :foreground nil t) box)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow

        ;; NOTE: a bug in emacs 25 breaks undoing in evil. See
        ;; https://bitbucket.org/lyro/evil/issues/594/undo-doesnt-behave-like-vim
        evil-want-fine-undo (if (> emacs-major-version 24) 'fine))

  ;; highlight matching delimiters where it's important
  (defun show-paren-mode-off () (show-paren-mode -1))
  (add-hook 'evil-insert-state-entry-hook   'show-paren-mode)
  (add-hook 'evil-insert-state-exit-hook    'show-paren-mode-off)
  (add-hook 'evil-visual-state-entry-hook   'show-paren-mode)
  (add-hook 'evil-visual-state-exit-hook    'show-paren-mode-off)
  (add-hook 'evil-operator-state-entry-hook 'show-paren-mode)
  (add-hook 'evil-operator-state-exit-hook  'show-paren-mode-off)
  (add-hook 'evil-normal-state-entry-hook   'show-paren-mode-off)

  :config
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (mapc (lambda (r) (evil-set-initial-state (car r) (cdr r)))
        '((compilation-mode       . normal)
          (help-mode              . normal)
          (message-mode           . normal)
          (debugger-mode          . normal)
          (image-mode             . normal)
          (doc-view-mode          . normal)
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

  (map! :map evil-command-window-mode-map :n [escape] 'kill-buffer-and-window)

  (progn ; evil hacks
    (advice-add 'evil-force-normal-state :after 'doom*evil-esc-quit)
    (defun doom*evil-esc-quit ()
      "Close popups, disable search highlights and quit the minibuffer if open."
      (if (eq major-mode 'help-mode)
          (doom/popup-close)
        (let ((minib-p (minibuffer-window-active-p (minibuffer-window)))
              (evil-hl-p (evil-ex-hl-active-p 'evil-ex-search)))
          (when minib-p (abort-recursive-edit))
          (when evil-hl-p (evil-ex-nohighlight))
          ;; Close non-repl popups and clean up `doom-popup-windows'
          (unless (or minib-p evil-hl-p
                      (memq (get-buffer-window) doom-popup-windows))
            (mapc (lambda (w)
                    (if (window-live-p w)
                        (with-selected-window w
                          (unless (derived-mode-p 'comint-mode)
                            (doom/popup-close w)))
                      (doom/popup-remove w)))
                  doom-popup-windows)))))

    ;; Fix harmless (yet disruptive) error reporting w/ hidden buffers caused by
    ;; workgroups killing windows
    ;; TODO Delete timer on dead windows
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

    ;; A monkey patch to add all of vim's file ex substitution flags to evil-mode.
    (defun evil-ex-replace-special-filenames (file-name)
      "Replace special symbols in FILE-NAME."
      (let ((case-fold-search nil)
            (regexp (concat "\\(?:^\\|[^\\\\]\\)"
                            "\\([#%@]\\)"
                            "\\(\\(?::\\(?:[phtreS~.]\\|g?s[^: $]+\\)\\)*\\)")))
        (dolist (match (s-match-strings-all regexp file-name))
          (let ((flags (split-string (caddr match) ":" t))
                (path (file-relative-name
                       (pcase (cadr match)
                         ("@" (doom/project-root))
                         ("%" (buffer-file-name))
                         ("#" (and (other-buffer) (buffer-file-name (other-buffer)))))
                       default-directory))
                flag global)
            (when path
              (while flags
                (setq flag (pop flags))
                (when (string-suffix-p "\\" flag)
                  (setq flag (concat flag (pop flags))))
                (when (string-prefix-p "gs" flag)
                  (setq global t flag (string-remove-prefix "g" flag)))
                (setq path
                      (or (pcase (substring flag 0 1)
                            ("p" (expand-file-name path))
                            ("~" (file-relative-name path "~"))
                            ("." (file-relative-name path default-directory))
                            ("h" (directory-file-name path))
                            ("t" (file-name-nondirectory (directory-file-name path)))
                            ("r" (file-name-sans-extension path))
                            ("e" (file-name-extension path))
                            ("s" (let* ((args (evil-delimited-arguments (substring flag 1) 2))
                                        (pattern (evil-transform-vim-style-regexp (car args)))
                                        (replace (cadr args)))
                                   (replace-regexp-in-string
                                    (if global pattern (concat "\\(" pattern "\\).*\\'"))
                                    (evil-transform-vim-style-regexp replace) path t t
                                    (unless global 1))))
                            ("S" (shell-quote-argument path))
                            (t path))
                          "")))
              (setq file-name
                    (replace-regexp-in-string (format "\\(?:^\\|[^\\\\]\\)\\(%s\\)"
                                                      (string-trim-left (car match)))
                                              path file-name t t 1)))))
        ;; Clean up
        (setq file-name (replace-regexp-in-string regexp "\\1" file-name t)))))

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
             evil-multiedit-toggle-or-restrict-region
             evil-multiedit-next
             evil-multiedit-prev
             evil-multiedit-abort
             evil-multiedit-ex-match)
  :config
  (map! :map evil-multiedit-state-map
        "RET" 'evil-multiedit-toggle-or-restrict-region
        "C-n" 'evil-multiedit-next
        "C-p" 'evil-multiedit-prev
        :map evil-multiedit-insert-state-map
        "C-n" 'evil-multiedit-next
        "C-p" 'evil-multiedit-prev))

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
  (add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook)
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (add-hook! (text-mode prog-mode)
    ;; Escaped surround characters
    (embrace-add-pair-regexp ?\\ "\\[[{(]" "\\[]})]" 'doom/embrace-escaped))
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
  (evil-escape-mode +1)

  ;; evil-escape causes noticable lag in linewise motions in visual mode, so disable it in
  ;; visual mode
  (defun doom|evil-escape-disable () (evil-escape-mode -1))
  (defun doom|evil-escape-enable () (evil-escape-mode +1))
  (add-hook 'evil-visual-state-entry-hook 'doom|evil-escape-disable)
  (add-hook 'evil-visual-state-exit-hook 'doom|evil-escape-enable)
  (add-hook 'evil-insert-state-exit-hook 'doom|evil-escape-enable)

  (push 'neotree-mode evil-escape-excluded-major-modes))

(provide 'core-evil)
;;; core-evil.el ends here
