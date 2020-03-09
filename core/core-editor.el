;;; core-editor.el -*- lexical-binding: t; -*-

(defvar doom-detect-indentation-excluded-modes '(fundamental-mode so-long-mode)
  "A list of major modes in which indentation should be automatically
detected.")

(defvar-local doom-inhibit-indent-detection nil
  "A buffer-local flag that indicates whether `dtrt-indent' should try to detect
indentation settings or not. This should be set by editorconfig if it
successfully sets indent_style/indent_size.")

(defvar-local doom-large-file-p nil)
(put 'doom-large-file-p 'permanent-local t)

(defvar doom-large-file-size-alist '(("." . 1.0))
  "An alist mapping regexps (like `auto-mode-alist') to filesize thresholds.

If a file is opened and discovered to be larger than the threshold, Doom
performs emergency optimizations to prevent Emacs from hanging, crashing or
becoming unusably slow.

These thresholds are in MB, and is used by `doom--optimize-for-large-files-a'.")

(defvar doom-large-file-excluded-modes
  '(so-long-mode special-mode archive-mode tar-mode jka-compr
    git-commit-mode image-mode doc-view-mode doc-view-mode-maybe
    ebrowse-tree-mode pdf-view-mode tags-table-mode)
  "Major modes that `doom-check-large-file-h' will ignore.")


;;
;;; File handling

(defadvice! doom--optimize-for-large-files-a (orig-fn &rest args)
  "Set `doom-large-file-p' if the file is too large.

Uses `doom-large-file-size-alist' to determine when a file is too large. When
`doom-large-file-p' is set, other plugins can detect this and reduce their
runtime costs (or disable themselves) to ensure the buffer is as fast as
possible."
  :around #'after-find-file
  (if (setq doom-large-file-p
            (and buffer-file-name
                 (not doom-large-file-p)
                 (file-exists-p buffer-file-name)
                 (ignore-errors
                   (> (nth 7 (file-attributes buffer-file-name))
                      (* 1024 1024
                         (assoc-default buffer-file-name doom-large-file-size-alist
                                        #'string-match-p))))))
      (prog1 (apply orig-fn args)
        (if (memq major-mode doom-large-file-excluded-modes)
            (setq doom-large-file-p nil)
          (when (fboundp 'so-long-minor-mode) ; in case the user disabled it
            (so-long-minor-mode +1))
          (message "Large file detected! Cutting a few corners to improve performance...")))
    (apply orig-fn args)))

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
(setq find-file-suppress-same-file-warnings t)

;; Create missing directories when we open a file that doesn't exist under a
;; directory tree that may not exist.
(add-hook! 'find-file-not-found-functions
  (defun doom-create-missing-directories-h ()
    "Automatically create missing directories when creating new files."
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                               parent-directory))
             (progn (make-directory parent-directory 'parents)
                    t))))))

;; Don't autosave files or create lock/history/backup files. We don't want
;; copies of potentially sensitive material floating around, and we'll rely on
;; git and our own good fortune instead. Fingers crossed!
(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil
      ;; But have a place to store them in case we do use them...
      auto-save-list-file-name (concat doom-cache-dir "autosave")
      backup-directory-alist `(("." . ,(concat doom-cache-dir "backup/"))))

(add-hook! 'after-save-hook
  (defun doom-guess-mode-h ()
    "Guess mode when saving a file in `fundamental-mode'."
    (when (eq major-mode 'fundamental-mode)
      (let ((buffer (or (buffer-base-buffer) (current-buffer))))
        (and (buffer-file-name buffer)
             (eq buffer (window-buffer (selected-window))) ; only visible buffers
             (set-auto-mode))))))


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

;; Favor hard-wrapping in text modes
(add-hook 'text-mode-hook #'auto-fill-mode)


;;
;;; Clipboard / kill-ring

;; Eliminate duplicates in the kill ring. That is, if you kill the same thing
;; twice, you won't have to use M-y twice to get past it to older entries in the
;; kill ring.
(setq kill-do-not-save-duplicates t)

;;
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Fixes the clipboard in tty Emacs by piping clipboard I/O through xclip, xsel,
;; pb{copy,paste}, wl-copy, termux-clipboard-get, or getclip (cygwin).
(unless IS-WINDOWS
  (add-hook! 'tty-setup-hook
    (defun doom-init-clipboard-in-tty-emacs-h ()
      (and (not (getenv "SSH_CONNECTION"))
           (require 'xclip nil t)
           (xclip-mode +1)))))


;;
;;; Extra file extensions to support

(push '("/LICENSE\\'" . text-mode) auto-mode-alist)
(push '("\\.log\\'" . text-mode) auto-mode-alist)
(push '("\\.env\\'" . sh-mode) auto-mode-alist)


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
        auto-revert-stop-on-user-input nil
        ;; Only prompts for confirmation when buffer is unsaved.
        revert-without-query (list "."))

  ;; Instead of using `auto-revert-mode' or `global-auto-revert-mode', we employ
  ;; lazy auto reverting on `focus-in-hook' and `doom-switch-buffer-hook'.
  ;;
  ;; This is because autorevert abuses the heck out of inotify handles which can
  ;; grind Emacs to a halt if you do expensive IO (outside of Emacs) on the
  ;; files you have open (like compression). We only really need to revert
  ;; changes when we switch to a buffer or when we focus the Emacs frame.
  (defun doom-auto-revert-buffer-h ()
    "Auto revert current buffer, if necessary."
    (unless (or auto-revert-mode (active-minibuffer-window))
      (auto-revert-handler)))

  (defun doom-auto-revert-buffers-h ()
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (doom-visible-buffers))
      (with-current-buffer buf
        (doom-auto-revert-buffer-h)))))


(use-package! recentf
  ;; Keep track of recently opened files
  :defer-incrementally easymenu tree-widget timer
  :after-call after-find-file
  :commands recentf-open-files
  :config
  (defun doom--recent-file-truename (file)
    (if (or (file-remote-p file nil t)
            (not (file-remote-p file)))
        (file-truename file)
      file))
  (setq recentf-filename-handlers
        '(substring-no-properties
          doom--recent-file-truename
          abbreviate-file-name)
        recentf-save-file (concat doom-cache-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 200)

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

  (when doom-interactive-mode
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
        save-place-limit 100)

  (defadvice! doom--recenter-on-load-saveplace-a (&rest _)
    "Recenter on cursor when loading a saved place."
    :after-while #'save-place-find-file-hook
    (if buffer-file-name (ignore-errors (recenter))))

  (defadvice! doom--inhibit-saveplace-in-long-files-a (orig-fn &rest args)
    :around #'save-place-to-alist
    (unless doom-large-file-p
      (apply orig-fn args)))

  (defadvice! doom--dont-prettify-saveplace-cache-a (orig-fn)
    "`save-place-alist-to-file' uses `pp' to prettify the contents of its cache.
`pp' can be expensive for longer lists, and there's no reason to prettify cache
files, so we replace calls to `pp' with the much faster `prin1'."
    :around #'save-place-alist-to-file
    (cl-letf (((symbol-function #'pp)
               (symbol-function #'prin1)))
      (funcall orig-fn)))

  (save-place-mode +1))


(use-package! server
  :when (display-graphic-p)
  :after-call pre-command-hook after-find-file focus-out-hook
  :defer 1
  :init
  (when-let (name (getenv "EMACS_SERVER_NAME"))
    (setq server-name name))
  :config
  (unless (server-running-p)
    (server-start)))


;;
;;; Packages

(use-package! better-jumper
  :after-call pre-command-hook
  :preface
  ;; REVIEW Suppress byte-compiler warning spawning a *Compile-Log* buffer at
  ;; startup. This can be removed once gilbertw1/better-jumper#2 is merged.
  (defvar better-jumper-local-mode nil)
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
    nil)

  ;; Creates a jump point before killing a buffer. This allows you to undo
  ;; killing a buffer easily (only works with file buffers though; it's not
  ;; possible to resurrect special buffers).
  (advice-add #'kill-current-buffer :around #'doom-set-jump-a)

  ;; Create a jump point before jumping with imenu.
  (advice-add #'imenu :around #'doom-set-jump-a))


(use-package! dtrt-indent
  ;; Automatic detection of indent settings
  :when doom-interactive-mode
  :defer t
  :init
  (add-hook! '(change-major-mode-after-body-hook read-only-mode-hook)
    (defun doom-detect-indentation-h ()
      (unless (or (not after-init-time)
                  doom-inhibit-indent-detection
                  doom-large-file-p
                  (memq major-mode doom-detect-indentation-excluded-modes)
                  (member (substring (buffer-name) 0 1) '(" " "*")))
        ;; Don't display messages in the echo area, but still log them
        (let ((inhibit-message (not doom-debug-mode)))
          (dtrt-indent-mode +1)))))
  :config
  ;; Enable dtrt-indent even in smie modes so that it can update `tab-width',
  ;; `standard-indent' and `evil-shift-width' there as well.
  (setq dtrt-indent-run-after-smie t)
  ;; Reduced from the default of 5000 for slightly faster analysis
  (setq dtrt-indent-max-lines 2000)

  ;; always keep tab-width up-to-date
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list)

  (defvar dtrt-indent-run-after-smie)
  (defadvice! doom--fix-broken-smie-modes-a (orig-fn arg)
    "Some smie modes throw errors when trying to guess their indentation, like
`nim-mode'. This prevents them from leaving Emacs in a broken state."
    :around #'dtrt-indent-mode
    (let ((dtrt-indent-run-after-smie dtrt-indent-run-after-smie))
      (cl-letf* ((old-smie-config-guess (symbol-function 'smie-config-guess))
                 (old-smie-config--guess (symbol-function 'symbol-config--guess))
                 ((symbol-function 'symbol-config--guess)
                  (lambda (beg end)
                    (funcall old-smie-config--guess beg (min end 10000))))
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
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol)

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
  ;; smartparens recognizes `slime-mrepl-mode', but not `sly-mrepl-mode', so...
  (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already (and is faster), so...
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (with-eval-after-load 'evil
    ;; But if someone does want overlays enabled, evil users will be stricken
    ;; with an off-by-one issue where smartparens assumes you're outside the
    ;; pair when you're really at the last character in insert mode. We must
    ;; correct this vile injustice.
    (setq sp-show-pair-from-inside t)
    ;; ...and stay highlighted until we've truly escaped the pair!
    (setq sp-cancel-autoskip-on-backward-movement nil))

  ;; The default is 100, because smartparen's scans are relatively expensive
  ;; (especially with large pair lists for some modes), we reduce it, as a
  ;; better compromise between performance and accuracy.
  (setq sp-max-prefix-length 25)
  ;; No pair has any business being longer than 4 characters; if they must, set
  ;; it buffer-locally. It's less work for smartparens.
  (setq sp-max-pair-length 4)
  ;; This isn't always smart enough to determine when we're in a string or not.
  ;; See https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  (add-hook! 'minibuffer-setup-hook
    (defun doom-init-smartparens-in-minibuffer-maybe-h ()
      "Enable `smartparens-mode' in the minibuffer, during `eval-expression',
`pp-eval-expression' or `evil-ex'."
      (when (memq this-command '(eval-expression pp-eval-expression evil-ex))
        (smartparens-mode))))

  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)

  ;; Smartparens breaks evil-mode's replace state
  (defvar doom-buffer-smartparens-mode nil)
  (add-hook! 'evil-replace-state-exit-hook
    (defun doom-enable-smartparens-mode-maybe-h ()
      (when doom-buffer-smartparens-mode
        (turn-on-smartparens-mode)
        (kill-local-variable 'doom-buffer-smartparens-mode))))
  (add-hook! 'evil-replace-state-entry-hook
    (defun doom-disable-smartparens-mode-maybe-h ()
      (when smartparens-mode
        (setq-local doom-buffer-smartparens-mode t)
        (turn-off-smartparens-mode))))

  (smartparens-global-mode +1))


(use-package! so-long
  :after-call after-find-file
  :config
  (when doom-interactive-mode
    (global-so-long-mode +1))
  (setq so-long-threshold 400) ; reduce false positives w/ larger threshold
  ;; Don't disable syntax highlighting and line numbers, or make the buffer
  ;; read-only, in `so-long-minor-mode', so we can have a basic editing
  ;; experience in them, at least. It will remain off in `so-long-mode',
  ;; however, because long files have a far bigger impact on Emacs performance.
  (delq! 'font-lock-mode so-long-minor-modes)
  (delq! 'display-line-numbers-mode so-long-minor-modes)
  (delq! 'buffer-read-only so-long-variable-overrides 'assq)
  ;; ...but at least reduce the level of syntax highlighting
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  ;; ...and insist that save-place not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; Text files could possibly be too long too
  (add-to-list 'so-long-target-modes 'text-mode)
  ;; But disable everything else that may be unnecessary/expensive for large
  ;; or wide buffers.
  (appendq! so-long-minor-modes
            '(flycheck-mode
              flyspell-mode
              eldoc-mode
              smartparens-mode
              highlight-numbers-mode
              better-jumper-local-mode
              ws-butler-mode
              auto-composition-mode
              undo-tree-mode
              highlight-indent-guides-mode
              hl-fill-column-mode))
  (defun doom-buffer-has-long-lines-p ()
    ;; HACK Fix #2183: `so-long-detected-long-line-p' tries to parse comment
    ;;      syntax, but in some buffers comment state isn't initialized,
    ;;      leading to a wrong-type-argument: stringp error.
    (let ((so-long-skip-leading-comments (bound-and-true-p comment-use-syntax))
          ;; HACK If visual-line-mode is on, then false positives are more
          ;;      likely, so up the threshold. More so in text-mode, since long
          ;;      paragraphs are the norm.
          (so-long-threshold
           (if visual-line-mode
               (* so-long-threshold
                  (if (derived-mode-p 'text-mode)
                      3
                    2))
             so-long-threshold)))
      (so-long-detected-long-line-p)))
  (setq so-long-predicate #'doom-buffer-has-long-lines-p))


(use-package! undo-tree
  ;; Branching & persistent undo
  :after-call doom-switch-buffer-hook after-find-file
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        ;; Increase undo-limits by a factor of ten to avoid emacs prematurely
        ;; truncating the undo history and corrupting the tree. See
        ;; https://github.com/syl20bnr/spacemacs/issues/12110
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000
        undo-tree-history-directory-alist
        `(("." . ,(concat doom-cache-dir "undo-tree-hist/"))))

  ;; Compress undo-tree history files with zstd, if available. File size isn't
  ;; the (only) concern here: the file IO barrier is slow for Emacs to cross;
  ;; reading a tiny file and piping it in-memory through zstd is *slightly*
  ;; faster than Emacs reading the entire undo-tree file from the get go (on
  ;; SSDs). Whether or not that's true in practice, we still enjoy zstd's ~80%
  ;; file savings (these files add up over time and zstd is so incredibly fast).
  (when (executable-find "zstd")
    (defadvice! doom--undo-tree-make-history-save-file-name-a (file)
      :filter-return #'undo-tree-make-history-save-file-name
      (concat file ".zst")))

  ;; Strip text properties from undo-tree data to stave off bloat. File size
  ;; isn't the concern here; undo cache files bloat easily, which can cause
  ;; freezing, crashes, GC-induced stuttering or delays when opening files.
  (defadvice! doom--undo-tree-strip-text-properties-a (&rest _)
    :before #'undo-list-transfer-to-tree
    (dolist (item buffer-undo-list)
      (and (consp item)
           (stringp (car item))
           (setcar item (substring-no-properties (car item))))))

  ;; Undo-tree is too chatty about saving its history files. This doesn't
  ;; totally suppress it logging to *Messages*, it only stops it from appearing
  ;; in the echo-area.
  (advice-add #'undo-tree-save-history :around #'doom-shut-up-a)

  (global-undo-tree-mode +1))


(use-package! ws-butler
  ;; a less intrusive `delete-trailing-whitespaces' on save
  :after-call after-find-file
  :config (ws-butler-global-mode +1))

(provide 'core-editor)
;;; core-editor.el ends here
