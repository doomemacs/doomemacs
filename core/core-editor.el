;;; core-editor.el -*- lexical-binding: t; -*-

(defvar doom-detect-indentation-excluded-modes '(fundamental-mode)
  "A list of major modes in which indentation should be automatically
detected.")

(defvar-local doom-inhibit-indent-detection nil
  "A buffer-local flag that indicates whether `dtrt-indent' should try to detect
indentation settings or not. This should be set by editorconfig if it
successfully sets indent_style/indent_size.")


;;
;;; File handling

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t)

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
(setq find-file-suppress-same-file-warnings t)

;; Create missing directories when we open a file that doesn't exist under a
;; directory tree that may not exist.
(add-hook! 'find-file-not-found-functions
  (defun doom-create-missing-directories-h ()
    "Automatically create missing directories when creating new files."
    (let ((parent-directory (file-name-directory buffer-file-name)))
      (when (and (not (file-exists-p parent-directory))
                 (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
        (make-directory parent-directory t)))))

;; Don't autosave files or create lock/history/backup files. The
;; editor doesn't need to hold our hands so much. We'll rely on git
;; and our own good fortune instead. Fingers crossed!
(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil
      ;; But have a place to store them in case we do use them...
      auto-save-list-file-name (concat doom-cache-dir "autosave")
      backup-directory-alist `(("." . ,(concat doom-cache-dir "backup/"))))


;;
;;; Formatting

;; Indentation
(setq-default tab-width 4
              tab-always-indent t
              indent-tabs-mode nil
              fill-column 80)

;; Word wrapping
(setq-default word-wrap t
              truncate-lines t
              truncate-partial-width-windows nil)

(setq sentence-end-double-space nil
      delete-trailing-lines nil
      require-final-newline t
      tabify-regexp "^\t* [ \t]+")  ; for :retab


;;
;;; Clipboard / kill-ring

 ;; Eliminate duplicates in the kill ring. That is, if you kill the
 ;; same thing twice, you won't have to use M-y twice to get past it
 ;; to older entries in the kill ring.
(setq kill-do-not-save-duplicates t)

;;
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Save clipboard contents into kill-ring before replacing them
(setq save-interprogram-paste-before-kill t)

;; Fix the clipboard in terminal or daemon Emacs (non-GUI)
(add-hook! 'tty-setup-hook
  (defun doom-init-clipboard-in-tty-emacs-h ()
    (unless (getenv "SSH_CONNECTION")
      (cond (IS-MAC
             (if (require 'osx-clipboard nil t) (osx-clipboard-mode)))
            ((executable-find "xclip")
             (if (require 'xclip nil t) (xclip-mode)))))))


;;
;;; Extra file extensions to support

(push '("/LICENSE\\'" . text-mode) auto-mode-alist)


;;
;;; Built-in plugins

(use-package! autorevert
  ;; revert buffers when their files/state have changed
  :hook (focus-in . doom-auto-revert-buffers-h)
  :hook (after-save . doom-auto-revert-buffers-h)
  :hook (doom-switch-buffer . doom-auto-revert-buffer-h)
  :hook (doom-switch-window . doom-auto-revert-buffer-h)
  :config
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil)

  ;; Instead of using `auto-revert-mode' or `global-auto-revert-mode', we employ
  ;; lazy auto reverting on `focus-in-hook' and `doom-switch-buffer-hook'.
  ;;
  ;; This is because autorevert abuses the heck out of inotify handles which can
  ;; grind Emacs to a halt if you do expensive IO (outside of Emacs) on the
  ;; files you have open (like compression). We only really need revert changes
  ;; when we switch to a buffer or when we focus the Emacs frame.
  (defun doom-auto-revert-buffer-h ()
    "Auto revert current buffer, if necessary."
    (unless auto-revert-mode
      (let ((revert-without-query t))
        (auto-revert-handler))))

  (defun doom-auto-revert-buffers-h ()
    "Auto revert's stale buffers (that are visible)."
    (unless auto-revert-mode
      (dolist (buf (doom-visible-buffers))
        (with-current-buffer buf
          (doom-auto-revert-buffer-h))))))


(after! bookmark
  (setq bookmark-save-flag t))


(use-package! recentf
  ;; Keep track of recently opened files
  :defer-incrementally easymenu tree-widget timer
  :after-call after-find-file
  :commands recentf-open-files
  :config
  (setq recentf-save-file (concat doom-cache-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 200
        recentf-exclude
        (list "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^/tmp/" "^/ssh:"
              "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$"
              ;; ignore private DOOM temp files
              (lambda (path)
                (ignore-errors (file-in-directory-p path doom-local-dir)))))

  (defun doom--recent-file-truename (file)
    (if (or (file-remote-p file nil t)
            (not (file-remote-p file)))
        (file-truename file)
      file))
  (setq recentf-filename-handlers '(doom--recent-file-truename abbreviate-file-name))

  (add-hook! '(doom-switch-window-hook write-file-functions)
    (defun doom--recentf-touch-buffer-h ()
      "Bump file in recent file list when it is switched or written to."
      (when buffer-file-name
        (recentf-add-file buffer-file-name))
      ;; Return nil for `write-file-functions'
      nil))

  (add-hook! 'dired-mode-hook
    (defun doom--recentf-add-dired-directory-h ()
      "Add dired directory to recentf file list."
      (recentf-add-file default-directory)))

  (unless noninteractive
    (add-hook 'kill-emacs-hook #'recentf-cleanup)
    (quiet! (recentf-mode +1))))


(use-package! savehist
  ;; persist variables across sessions
  :defer-incrementally custom
  :after-call post-command-hook
  :config
  (setq savehist-file (concat doom-cache-dir "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil ; save on kill only
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode +1)

  (add-hook! 'kill-emacs-hook
    (defun doom-unpropertize-kill-ring-h ()
      "Remove text properties from `kill-ring' for a smaller savehist file."
      (setq kill-ring (cl-loop for item in kill-ring
                               if (stringp item)
                               collect (substring-no-properties item)
                               else if item collect it)))))


(use-package! saveplace
  ;; persistent point location in buffers
  :after-call after-find-file dired-initial-position-hook
  :config
  (setq save-place-file (concat doom-cache-dir "saveplace")
        save-place-forget-unreadable-files t
        save-place-limit 200)
  (defadvice! doom--recenter-on-load-saveplace-a (&rest _)
    "Recenter on cursor when loading a saved place."
    :after-while #'save-place-find-file-hook
    (if buffer-file-name (ignore-errors (recenter))))
  (save-place-mode +1))


(use-package! server
  :when (display-graphic-p)
  :after-call pre-command-hook after-find-file focus-out-hook
  :init
  (when-let* ((name (getenv "EMACS_SERVER_NAME")))
    (setq server-name name))
  :config
  (unless (server-running-p)
    (server-start)))


;;
;;; Packages

(use-package! better-jumper
  :after-call pre-command-hook
  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  :config
  (better-jumper-mode +1)
  (add-hook 'better-jumper-post-jump-hook #'recenter)

  (defadvice! doom-set-jump-a (orig-fn &rest args)
    "Set a jump point and ensure ORIG-FN doesn't set any new jump points."
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply orig-fn args)))

  (defadvice! doom-set-jump-maybe-a (orig-fn &rest args)
    "Set a jump point if ORIG-FN returns non-nil."
    (let ((origin (point-marker))
          (result
           (let* ((evil--jumps-jumping t)
                  (better-jumper--jumping t))
             (apply orig-fn args))))
      (unless result
        (with-current-buffer (marker-buffer origin)
          (better-jumper-set-jump
           (if (markerp (car args))
               (car args)
             origin))))
      result))

  (defun doom-set-jump-h ()
    "Run `better-jumper-set-jump' but return nil, for short-circuiting hooks."
    (better-jumper-set-jump)
    nil))


(use-package! command-log-mode
  :commands global-command-log-mode
  :config
  (setq command-log-mode-auto-show t
        command-log-mode-open-log-turns-on-mode nil
        command-log-mode-is-global t
        command-log-mode-window-size 50))


(use-package! dtrt-indent
  ;; Automatic detection of indent settings
  :unless noninteractive
  :defer t
  :init
  (add-hook! '(change-major-mode-after-body-hook read-only-mode-hook)
    (defun doom-detect-indentation-h ()
      (unless (or (not after-init-time)
                  doom-inhibit-indent-detection
                  (member (substring (buffer-name) 0 1) '(" " "*"))
                  (memq major-mode doom-detect-indentation-excluded-modes))
        ;; Don't display messages in the echo area, but still log them
        (let ((inhibit-message (not doom-debug-mode)))
          (dtrt-indent-mode +1)))))
  :config
  (setq dtrt-indent-run-after-smie t)

  ;; always keep tab-width up-to-date
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list)

  (defvar dtrt-indent-run-after-smie)
  (defadvice! doom--fix-broken-smie-modes-a (orig-fn arg)
    "Some smie modes throw errors when trying to guess their indentation, like
`nim-mode'. This prevents them from leaving Emacs in a broken state."
    :around #'dtrt-indent-mode
    (let ((dtrt-indent-run-after-smie dtrt-indent-run-after-smie))
      (cl-letf* ((old-smie-config-guess (symbol-function 'smie-config-guess))
                 ((symbol-function 'smie-config-guess)
                  (lambda ()
                    (condition-case e (funcall old-smie-config-guess)
                      (error (setq dtrt-indent-run-after-smie t)
                             (message "[WARNING] Indent detection: %s"
                                      (error-message-string e))
                             (message "")))))) ; warn silently
        (funcall orig-fn arg)))))


(use-package! helpful
  ;; a better *help* buffer
  :commands helpful--read-symbol
  :init
  (define-key!
    [remap describe-function] #'helpful-callable
    [remap describe-command]  #'helpful-command
    [remap describe-variable] #'helpful-variable
    [remap describe-key]      #'helpful-key
    [remap describe-symbol]   #'doom/describe-symbol)

  (defun doom-use-helpful-a (orig-fn &rest args)
    "Force ORIG-FN to use helpful instead of the old describe-* commands."
    (cl-letf (((symbol-function #'describe-function) #'helpful-function)
              ((symbol-function #'describe-variable) #'helpful-variable))
      (apply orig-fn args)))

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


;;;###package imenu
(add-hook 'imenu-after-jump-hook #'recenter)


(use-package! smartparens
  ;; Auto-close delimiters and blocks as you type. It's more powerful than that,
  ;; but that is all Doom uses it for.
  :after-call doom-switch-buffer-hook after-find-file
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
  :config
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)

  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already, so...
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  ;; But if someone does want overlays enabled, evil users will be stricken with
  ;; an off-by-one issue where smartparens assumes you're outside the pair when
  ;; you're really at the last character in insert mode. We must correct this
  ;; vile injustice.
  (setq sp-show-pair-from-inside t)
  ;; ...and stay highlighted until we've truly escaped the pair!
  (setq sp-cancel-autoskip-on-backward-movement nil)
  ;; The default is 100, because smartparen's scans are relatively expensive
  ;; (especially with large pair lists for somoe modes), we halve it, as a
  ;; better compromise between performance and accuracy.
  (setq sp-max-prefix-length 50)
  ;; This speeds up smartparens. No pair has any business being longer than 4
  ;; characters; if they must, the modes that need it set it buffer-locally.
  (setq sp-max-pair-length 4)
  ;; This isn't always smart enough to determine when we're in a string or not.
  ;; See https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (cdr (assq key sp-message-alist)) nil))

  (add-hook! 'minibuffer-setup-hook
    (defun doom-init-smartparens-in-minibuffer-maybe-h ()
      "Enable `smartparens-mode' in the minibuffer, during `eval-expression' or
`evil-ex'."
      (when (memq this-command '(eval-expression evil-ex))
        (smartparens-mode))))

  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)

  ;; Smartparens breaks evil-mode's replace state
  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook  #'turn-on-smartparens-mode)

  (smartparens-global-mode +1))


(use-package! so-long
  :after-call after-find-file
  :config (global-so-long-mode +1))


(use-package! undo-tree
  ;; Branching & persistent undo
  :after-call doom-switch-buffer-hook after-find-file
  :config
  (setq undo-tree-auto-save-history nil ; disable because unstable
        ;; undo-in-region is known to cause undo history corruption, which can
        ;; be very destructive! Disabling it deters the error, but does not fix
        ;; it entirely!
        undo-tree-enable-undo-in-region nil
        undo-tree-history-directory-alist
        `(("." . ,(concat doom-cache-dir "undo-tree-hist/"))))

  (when (executable-find "zstd")
    (defadvice! doom--undo-tree-make-history-save-file-name-a (file)
      :filter-return #'undo-tree-make-history-save-file-name
      (concat file ".zst")))

  (defadvice! doom--undo-tree-strip-text-properties-a (&rest _)
    :before #'undo-list-transfer-to-tree
    (dolist (item buffer-undo-list)
      (and (consp item)
           (stringp (car item))
           (setcar item (substring-no-properties (car item))))))

  (global-undo-tree-mode +1))


(use-package! ws-butler
  ;; a less intrusive `delete-trailing-whitespaces' on save
  :after-call after-find-file
  :config
  (appendq! ws-butler-global-exempt-modes
            '(special-mode comint-mode term-mode eshell-mode))
  (ws-butler-global-mode))

(provide 'core-editor)
;;; core-editor.el ends here
