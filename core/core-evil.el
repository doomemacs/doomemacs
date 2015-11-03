;;; core-evil.el --- the root of all evil
;; see lib/evil-defuns.el

(use-package evil
  :init
  ;; highlight matching delimiters where it's important
  (defun show-paren-mode-off () (show-paren-mode -1))
  (add-hook! evil-insert-state-entry    'show-paren-mode)
  (add-hook! evil-insert-state-exit     'show-paren-mode-off)
  (add-hook! evil-visual-state-entry    'show-paren-mode)
  (add-hook! evil-visual-state-exit     'show-paren-mode-off)
  (add-hook! evil-motion-state-entry    'show-paren-mode)
  (add-hook! evil-motion-state-exit     'show-paren-mode-off)
  (add-hook! evil-operator-state-entry  'show-paren-mode)
  (add-hook! evil-operator-state-exit   'show-paren-mode-off)

  ;; Always ensure evil-shift-width is consistent with tab-width
  (add-hook! evil-local-mode (setq evil-shift-width tab-width))
  ;; Disable highlights on insert-mode
  (add-hook! evil-insert-state-entry 'evil-ex-nohighlight)

  :config
  (setq evil-magic t
        evil-want-C-u-scroll t       ; enable C-u for scrolling
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-want-fine-undo nil
        evil-want-visual-char-semi-exclusive t
        evil-ex-search-vim-style-regexp t
        evil-ex-interactive-search-highlight 'selected-window
        evil-echo-state nil

        ;; Color-coded state cursors
        evil-normal-state-cursor  '("white" box)
        evil-emacs-state-cursor   '("green" bar)
        evil-insert-state-cursor  '("white" bar)
        evil-visual-state-cursor  '("cyan" hollow)
        evil-iedit-state-cursor   '("orange" box))

  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search)

  (evil-define-key 'normal evil-command-window-mode-map [escape] 'kill-buffer-and-window)

  ;; modes to map to different default states
  (dolist (mode-map '((cider-repl-mode   . emacs)
                      (comint-mode       . emacs)
                      (term-mode         . emacs)
                      (help-mode         . normal)
                      (message-mode      . normal)
                      (compilation-mode  . normal)))
    (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))

  (progn ; evil hacks
    (defadvice evil-force-normal-state (after evil-esc-quit activate)
      (ignore-errors
        (when (popwin:popup-window-live-p)
          (popwin:close-popup-window))
         ; close popups, if any
        (evil-ex-nohighlight)
        ;; Exit minibuffer if alive
        (if (minibuffer-window-active-p (minibuffer-window))
            (narf/minibuffer-quit))))
    ;; Jump to new splits
    (defadvice evil-window-split (after evil-window-split-jump activate)
      (evil-window-down 1))
    (defadvice evil-window-vsplit (after evil-window-vsplit-jump activate)
      (evil-window-right 1))

    ;; Fix disruptive errors w/ hidden buffers caused by popwin
    (defadvice evil-ex-hl-do-update-highlight (around evil-ex-hidden-buffer-ignore-errors activate)
      (ignore-errors ad-do-it))

    ;; buffer-local ex commands, thanks to: http://emacs.stackexchange.com/questions/13186
    (defun evil-ex-define-cmd-local (cmd function)
      "Locally binds the function FUNCTION to the command CMD."
      (unless (local-variable-p 'evil-ex-commands)
        (setq-local evil-ex-commands (copy-alist evil-ex-commands)))
      (evil-ex-define-cmd cmd function))

    ;; Restore vimmish ex-mode keymaps in isearch
    ;; Hide keystroke display while isearch is active
    (add-hook! isearch-mode     (setq echo-keystrokes 0))
    (add-hook! isearch-mode-end (setq echo-keystrokes 0.02))
    (let ((map evil-ex-search-keymap))
      (define-key map (kbd "C-w") 'evil-delete-backward-word)
      (define-key map (kbd "C-u") 'evil-delete-whole-line))

    ;; Repeat motions with SPC/S-SPC
    (defmacro narf-space-setup! (command next-func prev-func)
      `(defadvice ,command
           (before ,(intern (format "narf-space--%s" (symbol-name command))) activate)
         (define-key evil-motion-state-map (kbd "SPC") ',next-func)
         (define-key evil-motion-state-map (kbd "S-SPC") ',prev-func)))

    (after! evil-snipe
      (narf-space-setup! evil-snipe-f evil-snipe-repeat evil-snipe-repeat-reverse)
      (narf-space-setup! evil-snipe-F evil-snipe-repeat evil-snipe-repeat-reverse)
      (narf-space-setup! evil-snipe-t evil-snipe-repeat evil-snipe-repeat-reverse)
      (narf-space-setup! evil-snipe-T evil-snipe-repeat evil-snipe-repeat-reverse)
      (narf-space-setup! evil-snipe-s evil-snipe-repeat evil-snipe-repeat-reverse)
      (narf-space-setup! evil-snipe-S evil-snipe-repeat evil-snipe-repeat-reverse)
      (narf-space-setup! evil-snipe-x evil-snipe-repeat evil-snipe-repeat-reverse)
      (narf-space-setup! evil-snipe-X evil-snipe-repeat evil-snipe-repeat-reverse))

    (after! evil-visualstar
      (narf-space-setup! evil-visualstar/begin-search-forward evil-ex-search-next evil-ex-search-previous)
      (narf-space-setup! evil-visualstar/begin-search-backward evil-ex-search-previous evil-ex-search-next))

    (narf-space-setup! evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
    (narf-space-setup! evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)

    (narf-space-setup! evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
    (narf-space-setup! evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)))

;; evil plugins
(use-package evil-anzu
  :config (setq anzu-cons-mode-line-p nil))

(use-package evil-args
  :commands (evil-inner-arg evil-outer-arg evil-forward-arg evil-backward-arg evil-jump-out-args)
  :init
  (define-key evil-inner-text-objects-map "a" #'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" #'evil-outer-arg)
  (define-key evil-normal-state-map "K" #'evil-jump-out-args))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :commands (evil-commentary
             evil-commentary-yank
             evil-commentary-line)
  :config (evil-commentary-mode 1))

(use-package evil-ex-registers
  :commands (evil-get-spec-register
             evil-ex-paste-from-register))

(use-package evil-exchange
  :commands evil-exchange
  :config
  (advice-add 'evil-force-normal-state :after 'narf*evil-exchange-off))

(use-package evil-iedit-state
  :functions (iedit-current-occurrence-string iedit-restrict-region)
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :config
  (define-key evil-visual-state-map (kbd "SPC") 'narf:iedit-restrict-to-region)
  (let ((map evil-iedit-state-map))
    ;; Don't interfere with evil-snipe
    (define-key map "s" nil)
    (define-key map "S" nil)

    (define-key map (kbd "V")  'evil-visual-line)
    (define-key map (kbd "C")  'evil-iedit-state/substitute) ; instead of s/S
    (define-key map (kbd "za") 'iedit-toggle-unmatched-lines-visible)))

(use-package evil-indent-textobject
  :commands (evil-indent-i-indent
             evil-indent-a-indent
             evil-indent-a-indent-lines)
  :init
  (let ((map evil-inner-text-objects-map))
    (define-key map "i" 'evil-indent-i-indent)
    (define-key map "i" 'evil-indent-a-indent)
    (define-key map "I" 'evil-indent-a-indent-lines)))

(use-package evil-jumper
  :init
  (setq evil-jumper-file (concat narf-temp-dir "jumplist")
        evil-jumper-auto-center t
        evil-jumper-auto-save-interval 3600))

(use-package evil-matchit
  :commands (evilmi-jump-items evilmi-text-object global-evil-matchit-mode)
  :config (global-evil-matchit-mode 1)
  :init
  (define-key evil-normal-state-map "%" #'evilmi-jump-items)
  (define-key evil-inner-text-objects-map "%" #'evilmi-text-object)
  (define-key evil-outer-text-objects-map "%" #'evilmi-text-object))

(use-package evil-easymotion
  :defer 1
  :config
  (evilem-default-keybindings "g SPC")
  (evilem-define (kbd "g SPC n") 'evil-ex-search-next)
  (evilem-define (kbd "g SPC N") 'evil-ex-search-previous)
  (evilem-define (kbd "g s") 'evil-snipe-repeat
    (lambda ()
      (save-excursion
        (ignore-errors
          (call-interactively #'evil-snipe-s))))
    nil
    ((evil-snipe-enable-highlight)
     (evil-snipe-enable-incremental-highlight)))

  (evilem-define (kbd "g S") 'evil-snipe-repeat-reverse
    (lambda ()
      (save-excursion
        (ignore-errors
          (call-interactively #'evil-snipe-s))))
    nil
    ((evil-snipe-enable-highlight)
     (evil-snipe-enable-incremental-highlight))))

(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

(use-package evil-search-highlight-persist
  :config
  (global-evil-search-highlight-persist t)
  (advice-add 'evil-force-normal-state :after 'evil-search-highlight-persist-remove-all))

(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :commands (evil-snipe-f evil-snipe-F evil-snipe-t evil-snipe-T evil-snipe-s evil-snipe-S evil-snipe-x evil-snipe-X )
  :init
  (setq-default
   evil-snipe-smart-case t
   evil-snipe-repeat-keys nil ; using space to repeat
   evil-snipe-scope 'line
   evil-snipe-repeat-scope 'visible
   evil-snipe-override-evil-repeat-keys nil ; causes problems with remapped ;
   evil-snipe-symbol-groups '((?\[ "[[{(]")
                              (?\] "[]})]")
                              (?\; "[;:]")))

  (define-key evil-normal-state-map (kbd "s") nil)
  (define-key evil-normal-state-map (kbd "S") nil)
  (define-key evil-motion-state-map (kbd "s") 'evil-snipe-s)
  (define-key evil-motion-state-map (kbd "S") 'evil-snipe-S)
  (define-key evil-motion-state-map (kbd "f") 'evil-snipe-f)
  (define-key evil-motion-state-map (kbd "F") 'evil-snipe-F)
  (define-key evil-motion-state-map (kbd "t") 'evil-snipe-t)
  (define-key evil-motion-state-map (kbd "T") 'evil-snipe-T)
  (define-key evil-operator-state-map (kbd "z") 'evil-snipe-s)
  (define-key evil-operator-state-map (kbd "Z") 'evil-snipe-S)
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config
  (global-evil-surround-mode 1)
  ;; Escaped surround characters
  (push '(?\\ . narf/evil-surround-escaped) evil-surround-pairs-alist))

(use-package evil-visualstar
  :commands (global-evil-visualstar-mode
             evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :config
  (global-evil-visualstar-mode 1))

(provide 'core-evil)
;;; core-evil.el ends here
