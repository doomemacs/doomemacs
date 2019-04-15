;;; core-editor.el -*- lexical-binding: t; -*-

(defvar doom-large-file-size 2
  "Size (in MB) above which the user will be prompted to open the file literally
to avoid performance issues. Opening literally means that no major or minor
modes are active and the buffer is read-only.")

(defvar doom-large-file-modes-list
  '(fundamental-mode special-mode archive-mode tar-mode jka-compr
    git-commit-mode image-mode doc-view-mode doc-view-mode-maybe
    ebrowse-tree-mode pdf-view-mode tags-table-mode)
  "Major modes that `doom|check-large-file' will ignore.")

(defvar-local doom-inhibit-indent-detection nil
  "A buffer-local flag that indicates whether `dtrt-indent' should try to detect
indentation settings or not. This should be set by editorconfig if it
successfully sets indent_style/indent_size.")

(defvar doom-detect-indentation-excluded-modes '(fundamental-mode)
  "A list of major modes in which indentation should be automatically
detected.")

(setq-default
 large-file-warning-threshold 15000000
 vc-follow-symlinks t
 ;; Save clipboard contents into kill-ring before replacing them
 save-interprogram-paste-before-kill t
 ;; Bookmarks
 bookmark-default-file (concat doom-etc-dir "bookmarks")
 bookmark-save-flag t
 ;; Formatting
 delete-trailing-lines nil
 fill-column 80
 sentence-end-double-space nil
 word-wrap t
 ;; Scrolling
 hscroll-margin 2
 hscroll-step 1
 scroll-conservatively 1001
 scroll-margin 2
 scroll-preserve-screen-position t
 ;; Whitespace (see `editorconfig')
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 4
 tabify-regexp "^\t* [ \t]+" ; for :retab
 ;; Wrapping
 truncate-lines t
 truncate-partial-width-windows 50)

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(setq-hook! '(eshell-mode-hook term-mode-hook) hscroll-margin 0)

(defun doom*optimize-literal-mode-for-large-files (buffer)
  (with-current-buffer buffer
    (when find-file-literally
      (setq buffer-read-only t)
      (buffer-disable-undo))
    buffer))
(advice-add #'find-file-noselect-1 :filter-return #'doom*optimize-literal-mode-for-large-files)


;;
;; Extra file extensions to support

(push '("/LICENSE\\'" . text-mode) auto-mode-alist)


;;
;; Built-in plugins

(def-package! server
  :when (display-graphic-p)
  :after-call (pre-command-hook after-find-file)
  :config
  (when-let* ((name (getenv "EMACS_SERVER_NAME")))
    (setq server-name name))
  (unless (server-running-p)
    (server-start)))

(def-package! autorevert
  ;; revert buffers for changed files
  :after-call after-find-file
  :config
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode +1))

(def-package! savehist
  ;; persist variables across sessions
  :defer-incrementally (custom)
  :after-call post-command-hook
  :config
  (setq savehist-file (concat doom-cache-dir "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil ; save on kill only
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode +1)

  (defun doom|unpropertize-kill-ring ()
    "Remove text properties from `kill-ring' in the interest of shrinking the
savehist file."
    (setq kill-ring (cl-loop for item in kill-ring
                             if (stringp item)
                             collect (substring-no-properties item)
                             else if item collect it)))
  (add-hook 'kill-emacs-hook #'doom|unpropertize-kill-ring))

(def-package! saveplace
  ;; persistent point location in buffers
  :after-call (after-find-file dired-initial-position-hook)
  :config
  (setq save-place-file (concat doom-cache-dir "saveplace"))
  (defun doom*recenter-on-load-saveplace (&rest _)
    "Recenter on cursor when loading a saved place."
    (if buffer-file-name (ignore-errors (recenter))))
  (advice-add #'save-place-find-file-hook
              :after-while #'doom*recenter-on-load-saveplace)
  (save-place-mode +1))

(def-package! recentf
  ;; Keep track of recently opened files
  :defer-incrementally (easymenu tree-widget timer)
  :after-call after-find-file
  :commands recentf-open-files
  :config
  (setq recentf-save-file (concat doom-cache-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-filename-handlers '(file-truename abbreviate-file-name)
        recentf-exclude
        (list #'file-remote-p "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
              "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$"
              ;; ignore private DOOM temp files (but not all of them)
              (lambda (file) (file-in-directory-p file doom-local-dir))))
  (unless noninteractive
    (add-hook 'kill-emacs-hook #'recentf-cleanup)
    (quiet! (recentf-mode +1))))


;;
;; Packages

(def-package! smartparens
  ;; Auto-close delimiters and blocks as you type. It's more powerful than that,
  ;; but that is all Doom uses it for.
  :after-call (doom-switch-buffer-hook after-find-file)
  :commands (sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string)
  :config
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil
        sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0.1
        sp-max-pair-length 4
        sp-max-prefix-length 50
        sp-escape-quotes-after-insert nil)  ; not smart enough

  ;; Smartparens' navigation feature is neat, but does not justify how expensive
  ;; it is. It's also less useful for evil users. This may need to be
  ;; reactivated for non-evil users though. Needs more testing!
  (defun doom|disable-smartparens-navigate-skip-match ()
    (setq sp-navigate-skip-match nil
          sp-navigate-consider-sgml-tags nil))
  (add-hook 'after-change-major-mode-hook #'doom|disable-smartparens-navigate-skip-match)

  ;; autopairing in `eval-expression' and `evil-ex'
  (defun doom|init-smartparens-in-eval-expression ()
    "Enable `smartparens-mode' in the minibuffer, during `eval-expression' or
`evil-ex'."
    (when (memq this-command '(eval-expression evil-ex))
      (smartparens-mode)))
  (add-hook 'minibuffer-setup-hook #'doom|init-smartparens-in-eval-expression)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

  ;; smartparens breaks evil-mode's replace state
  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook  #'turn-on-smartparens-mode)

  (smartparens-global-mode +1))


(def-package! dtrt-indent
  ;; Automatic detection of indent settings
  :unless noninteractive
  :defer t
  :init
  (defun doom|detect-indentation ()
    (unless (or (not after-init-time)
                doom-inhibit-indent-detection
                (member (substring (buffer-name) 0 1) '(" " "*"))
                (memq major-mode doom-detect-indentation-excluded-modes))
      (dtrt-indent-mode +1)))
  (add-hook! '(change-major-mode-after-body-hook read-only-mode-hook)
    #'doom|detect-indentation)
  :config
  (setq dtrt-indent-verbosity (if doom-debug-mode 2 0))
  ;; always keep tab-width up-to-date
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list)

  (defvar dtrt-indent-run-after-smie)
  (defun doom*fix-broken-smie-modes (orig-fn arg)
    "Some smie modes throw errors when trying to guess their indentation, like
`nim-mode'. This prevents them from leaving Emacs in a broken state."
    (let ((dtrt-indent-run-after-smie dtrt-indent-run-after-smie))
      (cl-letf* ((old-smie-config-guess (symbol-function 'smie-config-guess))
                 ((symbol-function 'smie-config-guess)
                  (lambda ()
                    (condition-case e (funcall old-smie-config-guess)
                      (error (setq dtrt-indent-run-after-smie t)
                             (message "[WARNING] Indent detection: %s"
                                      (error-message-string e))
                             (message "")))))) ; warn silently
        (funcall orig-fn arg))))
  (advice-add #'dtrt-indent-mode :around #'doom*fix-broken-smie-modes))


(def-package! undo-tree                                        
  ;; Branching & persistent undo
  :after-call (doom-switch-buffer-hook after-find-file)
  :config
  (setq undo-tree-auto-save-history t
        ;; undo-in-region is known to cause undo history corruption, which can
        ;; be very destructive! Disabling it deters the error, but does not fix
        ;; it entirely!
        undo-tree-enable-undo-in-region nil
        undo-tree-history-directory-alist
        `(("." . ,(concat doom-cache-dir "undo-tree-hist/"))))
  (global-undo-tree-mode +1)

  ;; compress undo history with xz/gzip
  (and (fset 'doom*undo-tree-make-history-save-file-name
             (cond ((executable-find "zstd") (lambda (file) (concat file ".zst")))
                   ((executable-find "gzip") (lambda (file) (concat file ".gz")))))
       (advice-add #'undo-tree-make-history-save-file-name :filter-return
                   #'doom*undo-tree-make-history-save-file-name))

  (defun doom*strip-text-properties-from-undo-history (&rest _)
    (dolist (item buffer-undo-list)
      (and (consp item)
           (stringp (car item))
           (setcar item (substring-no-properties (car item))))))
  (advice-add #'undo-list-transfer-to-tree :before #'doom*strip-text-properties-from-undo-history))


(def-package! command-log-mode
  :commands global-command-log-mode
  :config
  (setq command-log-mode-auto-show t
        command-log-mode-open-log-turns-on-mode nil
        command-log-mode-is-global t
        command-log-mode-window-size 50))


;; `helpful' --- a better *help* buffer
(def-package! helpful
  :commands helpful--read-symbol
  :init
  (define-key!
    [remap describe-function] #'helpful-callable
    [remap describe-command]  #'helpful-command
    [remap describe-variable] #'helpful-variable
    [remap describe-key]      #'helpful-key
    [remap describe-symbol]   #'doom/describe-symbol)

  (after! apropos
    ;; patch apropos buttons to call helpful instead of help
    (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
         (helpful-callable (button-get button 'apropos-symbol)))))
    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
         (helpful-variable (button-get button 'apropos-symbol)))))))


(def-package! ws-butler
  ;; a less intrusive `delete-trailing-whitespaces' on save
  :after-call (after-find-file)
  :config
  (setq ws-butler-global-exempt-modes
        (append ws-butler-global-exempt-modes
                '(special-mode comint-mode term-mode eshell-mode)))
  (ws-butler-global-mode))

(provide 'core-editor)
;;; core-editor.el ends here
