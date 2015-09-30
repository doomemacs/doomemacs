;;; core-evil.el --- the root of all evil
;; see lib/evil-defuns.el

(use-package evil
  :init
  ;; Disable highlights on insert-mode
  (add-hook! evil-insert-state-entry 'evil-ex-nohighlight)
  (add-hook! undo-tree-mode (diminish 'undo-tree-mode))
  ;; Always ensure evil-shift-width is consistent with tab-width
  (add-hook! evil-local-mode (setq evil-shift-width tab-width))
  :config
  (setq evil-magic                t
        evil-want-C-u-scroll      t  ; enable C-u for scrolling
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-want-fine-undo       nil
        evil-want-visual-char-semi-exclusive t
        evil-ex-search-vim-style-regexp t
        evil-ex-interactive-search-highlight 'selected-window

        ;; Color-coded state cursors
        evil-normal-state-cursor  '("white" box)
        evil-emacs-state-cursor   '("cyan" bar)
        evil-insert-state-cursor  '("white" bar)
        evil-visual-state-cursor  '("white" hollow)
        evil-iedit-state-cursor   '("orange" box))

  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search)

  (bind! :map evil-command-window-mode-map :n [escape] 'kill-buffer-and-window)

  ;; modes to map to different default states
  (dolist (mode-map '((cider-repl-mode   . emacs)
                      (comint-mode       . emacs)
                      (term-mode         . emacs)
                      (help-mode         . normal)
                      (message-mode      . normal)
                      (compilation-mode  . normal)))
    (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))

  (progn ; evil hacks
    (defadvice evil-force-normal-state (before evil-esc-quit activate)
      (ignore-errors
        (popwin:close-popup-window)                 ; close popups, if any
        (evil-ex-nohighlight)
        ;; Exit minibuffer if alive
        (if (minibuffer-window-active-p (minibuffer-window))
            (narf/minibuffer-quit))))

    ;; Jump to new splits
    (defadvice evil-window-split (after evil-window-split-jump activate)
      (evil-window-down 1))
    (defadvice evil-window-vsplit (after evil-window-vsplit-jump activate)
      (evil-window-right 1))

    ;; Restore vimmish ex-mode keymaps in isearch
    ;; Hide keystroke display while isearch is active
    (add-hook! isearch-mode     (setq echo-keystrokes 0))
    (add-hook! isearch-mode-end (setq echo-keystrokes 0.02))
    (bind! :map evil-ex-search-keymap
           "C-w" 'evil-delete-backward-word
           "C-u" 'evil-delete-whole-line)))

;; evil plugins
(use-package evil-anzu)

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
  (bind! :v "SPC" 'narf:iedit-restrict-to-region
         (:map evil-iedit-state-map
           ;; Don't interfere with evil-snipe
           "s"   nil
           "S"   nil
           "V"   'evil-visual-line
           "C"   'evil-iedit-state/substitute  ; instead of s/S
           "za"  'iedit-toggle-unmatched-lines-visible)))

(use-package evil-indent-textobject
  :commands (evil-indent-i-indent
             evil-indent-a-indent
             evil-indent-a-indent-lines)
  :init
  (bind! :map evil-inner-text-objects-map
         "i" 'evil-indent-i-indent
         "i" 'evil-indent-a-indent
         "I" 'evil-indent-a-indent-lines))

(use-package evil-jumper
  :init
  (setq evil-jumper-file (! (concat narf-temp-dir "jumplist"))
        evil-jumper-auto-center t
        evil-jumper-auto-save-interval 3600))

(use-package evil-matchit
  :commands (evilmi-jump-items global-evil-matchit-mode)
  :config   (global-evil-matchit-mode 1))

(use-package evil-easymotion
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
  :diminish evil-snipe-mode
  :init
  (setq-default
   evil-snipe-smart-case t
   evil-snipe-repeat-keys nil ; using evil-space to repeat
   evil-snipe-scope 'line
   evil-snipe-repeat-scope 'buffer
   evil-snipe-override-evil-repeat-keys nil ; causes problems with remapped ;
   evil-snipe-symbol-groups '((?\[ "[[{(]")
                              (?\] "[]})]")))
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package evil-space
  :diminish (evil-space-mode . "_")
  :init (setq evil-space-auto-setup nil)
  :config
  (evil-space-mode 1)

  (evil-space-setup "/" "n" "N")
  (evil-space-setup "?" "N" "n")

  (after! evil-snipe
    (evil-space-setup 'evil-snipe-f 'evil-snipe-repeat 'evil-snipe-repeat-reverse)
    (evil-space-setup 'evil-snipe-F 'evil-snipe-repeat 'evil-snipe-repeat-reverse)
    (evil-space-setup 'evil-snipe-t 'evil-snipe-repeat 'evil-snipe-repeat-reverse)
    (evil-space-setup 'evil-snipe-T 'evil-snipe-repeat 'evil-snipe-repeat-reverse)
    (evil-space-setup 'evil-snipe-s 'evil-snipe-repeat 'evil-snipe-repeat-reverse)
    (evil-space-setup 'evil-snipe-S 'evil-snipe-repeat 'evil-snipe-repeat-reverse))

  (after! evil-visualstar
    (evil-space-setup 'evil-visualstar/begin-search-forward "n" "N")
    (evil-space-setup 'evil-visualstar/begin-search-backward "n" "N")))

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
