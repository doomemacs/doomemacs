;;;; Eeeeeeevil ;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil
  :diminish undo-tree-mode
  :config
  (progn
    (setq evil-want-visual-char-semi-exclusive nil
          evil-search-module        'evil-search
          evil-search-wrap          nil
          evil-magic                'magic
          evil-want-C-u-scroll      t  ; enable C-u for scrolling
          evil-ex-visual-char-range t  ; column range for ex commands
          evil-ex-search-vim-style-regexp t
          evil-ex-interactive-search-highlight 'selected-window

          ;; Color-coded state cursors
          evil-normal-state-cursor  '("white" box)
          evil-emacs-state-cursor   '("cyan" bar)
          evil-insert-state-cursor  '("white" bar)
          evil-visual-state-cursor  'hollow)

    (evil-mode 1)

    ;; Always ensure evil-shift-width is consistent with tab-width
    (add-hook! 'evil-local-mode-hook (setq evil-shift-width tab-width))
    ;; Fix code folding
    (add-hook! 'prog-mode-hook (hs-minor-mode 1) (diminish 'hs-minor-mode))

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

    ;; modes to map to different default states
    (dolist (mode-map '((cider-repl-mode . emacs)
                        (comint-mode . emacs)
                        (fundamental-mode . normal)
                        (help-mode . normal)
                        (term-mode . emacs)))
      (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))

    ;; predicate for visual line mode
    (defun evil-visual-line-state-p ()
      "Returns non-nil if in visual-line mode, nil otherwise."
      (and (evil-visual-state-p)
           (eq (evil-visual-type) 'line)))


    (progn ; evil plugins
      (use-package evil-indent-textobject)    ; vii/vai/vaI

      ;; (use-package evil-ex-registers)
      (use-package evil-surround
        :commands (evil-surround-edit
                   evil-Surround-edit
                   evil-surround-region)
        :config (global-evil-surround-mode 1))

      (use-package evil-numbers
        :commands (evil-numbers/inc-at-pt
                   evil-numbers/dec-at-pt))

      (use-package evil-matchit
        :commands (evilmi-jump-items)
        :config   (global-evil-matchit-mode 1))

      (use-package evil-search-highlight-persist
        :config   (global-evil-search-highlight-persist t))

      (use-package evil-commentary
        :diminish evil-commentary-mode
        :commands (evil-commentary
                   evil-commentary-yank
                   evil-commentary-line)
        :config   (evil-commentary-mode 1))

      (use-package evil-jumper
        :init (setq evil-jumper-file (expand-file-name "jumplist" my-tmp-dir))
        :config
        (setq evil-jumper-auto-center t
              evil-jumper-auto-save-interval 3600))

      (use-package evil-exchange
        :commands (evil-exchange)
        :config
        (defadvice evil-force-normal-state (before evil-esc-quit-exchange activate)
          (when evil-exchange--overlays
            (evil-exchange-cancel))))

      (use-package evil-visualstar
        :commands (evil-visualstar/begin-search
                   evil-visualstar/begin-search-forward
                   evil-visualstar/begin-search-backward)
        :config
        (progn
          ;; I cut this down because the original visualstar wouldn't remember
          ;; the last search if evil-search-module was 'evil-search.
          (defun evil-visualstar/begin-search (beg end direction)
            (when (evil-visual-state-p)
              (evil-exit-visual-state)
              (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
                (setq isearch-forward direction)
                (evil-search selection direction t))))
          (global-evil-visualstar-mode 1)))

      (use-package evil-snipe
        :diminish evil-snipe-mode
        :config
        (progn
          (global-evil-snipe-mode +1)
          (setq evil-snipe-smart-case t
                evil-snipe-override-evil t
                evil-snipe-scope 'line
                evil-snipe-repeat-scope 'buffer
                evil-snipe-override-evil-repeat-keys nil)
          (setq-default evil-snipe-symbol-groups
                '((?\[ "[[{(]")
                  (?\] "[]})]")))
          (bind 'motion
                "C-;" 'evil-snipe-repeat
                "C-," 'evil-snipe-repeat-reverse))))

    (bind evil-ex-completion-map
          "C-r"           #'evil-ex-paste-from-register   ; registers in ex-mode
          "C-a"            'move-beginning-of-line
          "<s-left>"       'move-beginning-of-line
          "<s-right>"      'move-beginning-of-line
          "<s-backspace>"  'evil-delete-whole-line)

    (progn ; evil hacks
      (defadvice evil-force-normal-state (before evil-esc-quit activate)
        (shut-up (evil-search-highlight-persist-remove-all) ; turn off highlights
                 (evil-ex-nohighlight)
                 ;; Exit minibuffer is alive
                 (if (minibuffer-window-active-p (minibuffer-window))
                     (my--minibuffer-quit))))

      ;; Popwin: close popup window, if any
      (defadvice evil-force-normal-state (before evil-esc-quit-popwin activate)
        (shut-up (popwin:close-popup-window)))

      ;; Jump to new splits
      (defadvice evil-window-split (after evil-window-split-jump activate)
        (evil-window-down 1))
      (defadvice evil-window-vsplit (after evil-window-vsplit-jump activate)
        (evil-window-right 1)))

    (progn ; ex-commands
      (evil-define-command my:kill-buffers (&optional bang)
        :repeat nil
        (interactive "<!>")
        (if (and (not bang) (projectile-project-p))
            (projectile-kill-buffers)
          (mapc 'kill-buffer (buffer-list)))
        (delete-other-windows)
        (switch-to-buffer (if (buffer-live-p project-scratch-buffer) project-scratch-buffer (get-buffer-create "*scratch*"))))

      (evil-define-command my:kill-buried-buffers (&optional bang)
        :repeat nil
        (interactive "<!>")
        (mapc 'kill-buffer
              (my-living-buffer-list (if bang (projectile-project-buffers) (buffer-list)))))

      (evil-define-command my:init-files (&optional bang) :repeat nil
        (interactive "<!>")
        (if bang
            (ido-find-file-in-dir my-modules-dir)
          (ido-find-file-in-dir my-dir)))

      (evil-define-command my:notes () :repeat nil
        (interactive)
        (require 'org)
        (ido-find-file-in-dir org-directory))

      (evil-define-command my:byte-compile (&optional bang)
        :repeat nil
        (interactive "<!>")
        (byte-recompile-file (expand-file-name "init.el" my-dir) bang 0)
        (dolist (file (append (f-glob "core*.el" my-modules-dir)
                              (f-glob "defuns*.el" my-modules-dir)
                              (f-glob "my*.el" my-modules-dir)))
          (byte-recompile-file file bang 0)))

      (evil-define-command my:cd (dir)
        :repeat nil
        (interactive "<f>")
        (cd (if (zerop (length dir)) "~" dir)))

      (defun --save-exit() (save-buffer) (kill-buffer) (remove-hook 'yas-after-exit-snippet-hook '--save-exit))
      (evil-define-command my:create-file (path &optional bang)
        "Deploy files (and their associated templates) quickly. Will prompt
you to fill in each snippet field before buffer closes unless BANG is
provided."
        :repeat nil
        (interactive "<f><!>")
        (let ((dir (f-dirname path))
              (fullpath (f-full path))
              (is-auto t))
          (when (and bang (not (f-exists? dir))) (f-mkdir dir))
          (if (f-exists? dir)
              (if (f-exists? fullpath)
                  (error "File already exists: %s" path)
                (find-file fullpath)
                (add-hook 'yas-after-exit-snippet-hook '--save-exit)
                (if bang (--save-exit)))
            (error "Directory doesn't exist: %s" dir))))

      (evil-define-command my:rename-this-file (new-name)
        "Renames current buffer and file it is visiting. Replaces %, # and other
  variables (see `evil-ex-replace-special-filenames')"
        :repeat nil
        (interactive "<f>")
        (let ((name (buffer-name))
              (filename (buffer-file-name)))
          (if (not (and filename (file-exists-p filename)))
              (error "Buffer '%s' is not visiting a file!" name)
            (let ((new-name
                   (evil-ex-replace-special-filenames (if new-name
                                                          new-name
                                                        (read-file-name "New name: " filename)))))
              (if (get-buffer new-name)
                  (error "A buffer named '%s' already exists!" new-name)
                (rename-file filename new-name 1)
                (rename-buffer new-name)
                (set-visited-file-name new-name)
                (set-buffer-modified-p nil)
                (save-place-forget-unreadable-files)
                (message "File '%s' successfully renamed to '%s'"
                         name (file-name-nondirectory new-name)))))))

      (evil-define-operator my:scratch-buffer (&optional beg end bang)
        "Send a selection to the scratch buffer. If BANG, then send it to org-capture
  instead."
        :move-point nil
        :type inclusive
        (interactive "<r><!>")
        (let ((mode major-mode)
              (text (when (and (evil-visual-state-p) beg end)
                      (buffer-substring beg end))))
          (if bang
              ;; use org-capture with bang
              (if text
                  (org-capture-string text)
                (org-capture))
            ;; or scratch buffer by default
            (let ((project-dir (projectile-project-root))
                  (buffer-name (if (projectile-project-p)
                                   (format "*scratch* (%s)" (projectile-project-name))
                                 "*scratch*")))
              (popwin:popup-buffer (get-buffer-create buffer-name))
              (when (eq (get-buffer buffer-name) (current-buffer))
                (cd project-dir)
                (if text (insert text))
                (funcall mode))))))

      (evil-define-command my:align (beg end &optional regexp bang)
        :repeat nil
        (interactive "<r><a><!>")
        (when regexp
          (align-regexp beg end
                        (concat "\\(\\s-*\\)" (rxt-pcre-to-elisp regexp)) 1 1)))

      (evil-define-operator my:retab (beg end)
        "Akin to vim's :retab, this changes all tabs-to-spaces or spaces-to-tabs,
  depending on `indent-tab-mode'. Untested."
        :motion nil
        :move-point nil
        :type line
        (interactive "<r>")
        (unless (and beg end)
          (setq beg (point-min))
          (setq end (point-max)))
        (if indent-tabs-mode
            (tabify beg end)
          (untabify beg end)))

      (evil-define-operator my:narrow-indirect (beg end)
        "Indirectly narrow the region from BEG to END."
        :move-point nil
        :type exclusive
        :repeat nil
        (interactive "<r>")
        (evil-normal-state)
        (my-narrow-to-region-indirect beg end)))))


(provide 'core-evil)
