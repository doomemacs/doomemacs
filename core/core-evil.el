;;;; Eeeeeeevil ;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil
  :init
  (progn
    (use-package goto-last-change)

    ;; highlight matching delimiters where it's important
    (defun show-paren-mode-off () (show-paren-mode -1))
    (add-hook 'evil-insert-state-entry-hook    'show-paren-mode)
    (add-hook 'evil-insert-state-exit-hook     'show-paren-mode-off)
    (add-hook 'evil-visual-state-entry-hook    'show-paren-mode)
    (add-hook 'evil-visual-state-exit-hook     'show-paren-mode-off)
    (add-hook 'evil-motion-state-entry-hook    'show-paren-mode)
    (add-hook 'evil-motion-state-exit-hook     'show-paren-mode-off)
    (add-hook 'evil-operator-state-entry-hook  'show-paren-mode)
    (add-hook 'evil-operator-state-exit-hook   'show-paren-mode-off)

    ;; Disable highlights on insert-mode
    (add-hook 'evil-insert-state-entry-hook 'evil-ex-nohighlight)

    (add-hook! 'undo-tree-mode-hook (diminish 'undo-tree-mode))
    ;; Always ensure evil-shift-width is consistent with tab-width
    (add-hook! 'evil-local-mode-hook (setq evil-shift-width tab-width)))
  :config
  (progn
    (progn ; open/close fold mods
      ;; Instead of `evil-open-folds'. Accepts COUNT for dictating fold level.
      (evil-define-command narf:open-folds (count)
        (interactive "P")
        (if count (hs-hide-level count) (evil-open-folds)))

      ;; Instead of `evil-close-folds'. Accepts COUNT for dictating fold level.
      (evil-define-command narf:close-folds (count)
        (interactive "P")
        (if count (hs-hide-level count) (evil-close-folds))))

    (setq evil-magic                t
          evil-want-C-u-scroll      t  ; enable C-u for scrolling
          evil-ex-visual-char-range t  ; column range for ex commands
          evil-want-fine-undo       nil
          evil-want-visual-char-semi-exclusive nil
          evil-ex-search-vim-style-regexp t
          evil-ex-interactive-search-highlight 'selected-window

          ;; Color-coded state cursors
          evil-normal-state-cursor  '("white" box)
          evil-emacs-state-cursor   '("cyan" bar)
          evil-insert-state-cursor  '("white" bar)
          evil-visual-state-cursor  '("white" hollow)
          evil-iedit-state-cursor   '("orange" box))

    (add-to-list 'evil-overriding-maps '(narf-mode-map))
    (evil-mode 1)
    (evil-select-search-module 'evil-search-module 'evil-search)

    (defadvice evil-ex-hl-do-update-highlight (around evil-ex-hl-shut-up activate)
      (ignore-errors ad-do-it))

    ;; modes to map to different default states
    (dolist (mode-map '((cider-repl-mode   . emacs)
                        (comint-mode       . emacs)
                        (term-mode         . emacs)
                        (fundamental-mode  . normal)
                        (help-mode         . normal)
                        (message-mode      . normal)
                        (compilation-mode  . normal)
                        (text-mode         . normal)))
      (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))

    ;; Ace Jump
    ;; https://github.com/winterTTr/ace-jump-mode/issues/23
    (evil-define-motion evil-ace-jump-two-chars-mode (count)
      :type exclusive
      :repeat abort
      (evil-without-repeat
        (evil-enclose-ace-jump-for-motion
          (call-interactively 'ace-jump-two-chars-mode))))

    (progn ; evil helpers
      (defun evil-visual-line-state-p ()
      "Returns non-nil if in visual-line mode, nil otherwise."
      (and (evil-visual-state-p)
           (eq (evil-visual-type) 'line))))

    (progn ; evil plugins
      (use-package evil-anzu)

      (use-package evil-iedit-state
        :functions (iedit-current-occurrence-string iedit-restrict-region)
        :commands (evil-iedit-state evil-iedit-state/iedit-mode)
        :config
        (progn
          (bind :map evil-iedit-state-map ; Don't interfere with evil-snipe
                "s" nil
                "S" nil)
          (bind iedit
              "V"  'evil-visual-line
              "C"  'evil-iedit-state/substitute  ; instead of s/S
              "za" 'iedit-toggle-unmatched-lines-visible

              visual "SPC" (λ (if (iedit-current-occurrence-string)
                                  (let ((current-prefix-arg '(4)))
                                    (iedit-done)
                                    (call-interactively 'iedit-mode)
                                    (save-excursion (iedit-restrict-region (region-beginning) (region-end)))
                                    (evil-previous-line))
                                (call-interactively 'evil-ret))))))


      (use-package evil-search-highlight-persist
        :config   (global-evil-search-highlight-persist t))

      (use-package evil-indent-textobject    ; vii/vai/vaI
        :commands (evil-indent-i-indent
                   evil-indent-a-indent
                   evil-indent-a-indent-lines)
        :init
        (bind :map evil-inner-text-objects-map
              "i" 'evil-indent-i-indent
              "i" 'evil-indent-a-indent
              "I" 'evil-indent-a-indent-lines))

      (use-package evil-ex-registers
        :commands (evil-get-spec-register
                   evil-ex-paste-from-register))

      (use-package evil-surround
        :commands (global-evil-surround-mode
                   evil-surround-edit
                   evil-Surround-edit
                   evil-surround-region)
        :config
        (progn
          (evil-define-motion evil-surround-line (count)
            "Move COUNT - 1 lines down but return exclusive character motion."
            :type exclusive
            (let ((beg (line-beginning-position)))
              (evil-line count)
              (end-of-line)
              (let ((range (evil-range beg (point) 'exclusive)))
                (evil-expand-range range)
                range)))

          ;; Escaped surround characters
          (defun evil-surround-escaped ()
            (let* ((char (string (read-char "\\")))
                   (pair (cond ((string-match "[]})[{(]" char)
                                (let ((-pair (cdr (assoc (string-to-char char) evil-surround-pairs-alist))))
                                  `(,(car -pair) . ,(cdr -pair))))
                               (t
                                `(,char . ,char))))
                   (format (if (sp-point-in-string) "\\\\%s" "\\%s")))
              (cons (format format (car pair))
                    (format format (cdr pair)))))

          (global-evil-surround-mode 1)

          (push '(?\C-\[ . ("" . "")) evil-surround-pairs-alist)))

      (use-package evil-numbers
        :commands (evil-numbers/inc-at-pt
                   evil-numbers/dec-at-pt))

      (use-package evil-matchit
        :commands (evilmi-jump-items global-evil-matchit-mode)
        :config   (global-evil-matchit-mode 1))

      (use-package evil-commentary
        :diminish evil-commentary-mode
        :commands (evil-commentary
                   evil-commentary-mode
                   evil-commentary-yank
                   evil-commentary-line)
        :config   (evil-commentary-mode 1))

      (use-package evil-jumper
        :init (setq evil-jumper-file (expand-file-name "jumplist" TMP-DIR))
        :config
        (setq evil-jumper-auto-center t
              evil-jumper-auto-save-interval 3600))

      (use-package evil-exchange
        :commands evil-exchange
        :config
        (defadvice evil-force-normal-state (before evil-esc-quit-exchange activate)
          (when evil-exchange--overlays (evil-exchange-cancel))))

      (use-package evil-visualstar
        :commands (global-evil-visualstar-mode
                   evil-visualstar/begin-search-forward
                   evil-visualstar/begin-search-backward)
        :config
        (global-evil-visualstar-mode 1))

      (use-package evil-snipe
        :diminish evil-snipe-mode
        :init
        (setq evil-snipe-smart-case   t
              evil-snipe-scope        'line
              evil-snipe-repeat-scope 'buffer
              evil-snipe-symbol-groups '((?\[ "[[{(]")
                                         (?\] "[]})]")))
        :config
        (progn
          (evil-snipe-mode 1)
          (evil-snipe-override-mode 1)

          (bind motion :map evil-snipe-mode-map
                "C-;" 'evil-snipe-repeat
                "C-," 'evil-snipe-repeat-reverse)))

      (use-package evil-space
        :diminish (evil-space-mode . "_")
        :init (setq evil-space-auto-setup nil)
        :config
        (progn
          (evil-space-mode 1)

          (evil-space-setup "/" "n" "N")
          (evil-space-setup "?" "n" "N")

          (after evil-snipe
            (evil-space-setup evil-snipe-f evil-snipe-repeat evil-snipe-repeat-reverse)
            (evil-space-setup evil-snipe-F evil-snipe-repeat evil-snipe-repeat-reverse)
            (evil-space-setup evil-snipe-t evil-snipe-repeat evil-snipe-repeat-reverse)
            (evil-space-setup evil-snipe-T evil-snipe-repeat evil-snipe-repeat-reverse)
            (evil-space-setup evil-snipe-s evil-snipe-repeat evil-snipe-repeat-reverse)
            (evil-space-setup evil-snipe-S evil-snipe-repeat evil-snipe-repeat-reverse))

          (after evil-numbers
            (let ((map (evil-get-auxiliary-keymap narf-mode-map 'normal)))
              (evil-space-setup "g=" "g=" "g-" map)
              (evil-space-setup "g-" "g-" "g=" map)))

          (after evil-visualstar
            (evil-space-setup evil-visualstar/begin-search-forward "n" "N")
            (evil-space-setup evil-visualstar/begin-search-backward "n" "N")))))

    (progn ; evil hacks
      (defadvice evil-force-normal-state (before evil-esc-quit activate)
        (ignore-errors
          (popwin:close-popup-window)                 ; close popups, if any
          (evil-search-highlight-persist-remove-all)  ; turn off highlights
          (evil-ex-nohighlight)
          ;; Exit minibuffer if alive
          (if (minibuffer-window-active-p (minibuffer-window))
              (narf/minibuffer-quit))))

      ;; Jump to new splits
      (defadvice evil-window-split (after evil-window-split-jump activate)
        (evil-window-down 1))
      (defadvice evil-window-vsplit (after evil-window-vsplit-jump activate)
        (evil-window-right 1))

      (progn ; Restore vimmish ex-mode keymaps to isearch
        (defun narf:isearch-delete-word ()
          (interactive)
          (let ((num (length isearch-string))
                (string (s-reverse isearch-string)))
            (when (string-match "[^a-zA-Z0-9]" string 1)
              (setq num (match-beginning 0)))
            (dotimes (i num)
              (isearch-pop-state))
            (isearch-update)))

        (defun narf:isearch-delete-line ()
          (interactive)
          (let ((num (length isearch-string)))
            (dotimes (i num) (isearch-pop-state))
            (isearch-update)))

        (defun narf:isearch-paste-from-register (reg)
          (interactive)
          (let ((str (evil-get-register reg t)))
            (when (> (length str) 0)
              (isearch-yank-string str))))

        (defun narf:isearch-paste-from-clipboard ()
          (interactive)
          (narf:isearch-paste-from-register ?+))

        ;; Hide keystroke display while isearch is active
        (add-hook! 'isearch-mode-hook     (setq echo-keystrokes 0))
        (add-hook! 'isearch-mode-end-hook (setq echo-keystrokes 0.02))

        (bind :map isearch-mode-map
              "C-r" nil

              "C-r %" (λ (narf:isearch-paste-from-register ?%))
              "C-r #" (λ (narf:isearch-paste-from-register ?#))
              "C-r /" (λ (narf:isearch-paste-from-register ?/))
              "C-r :" (λ (narf:isearch-paste-from-register ?:))
              "C-r ." (λ (narf:isearch-paste-from-register ?.))
              "C-r -" (λ (narf:isearch-paste-from-register ?-))
              "C-r _" (λ (narf:isearch-paste-from-register ?_))
              "C-r =" (λ (narf:isearch-paste-from-register ?=))
              "C-r +" 'narf:isearch-paste-from-clipboard

              "C-w"   'narf:isearch-delete-word
              "C-u"   'narf:isearch-delete-line
              "M-v"   'narf:isearch-paste-from-clipboard)))))


(provide 'core-evil)
;;; core-evil.el ends here
