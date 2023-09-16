;;; emacs/undo/config.el -*- lexical-binding: t; -*-

(use-package! undo-fu
  :unless (modulep! +tree)
  :hook (doom-first-buffer . undo-fu-mode)
  :config
  ;; Increase undo history limits to reduce likelihood of data loss
  (setq undo-limit 400000           ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)

  (define-minor-mode undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo)
              (define-key map (kbd "C-_")     #'undo-fu-only-undo)
              (define-key map (kbd "M-_")     #'undo-fu-only-redo)
              (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
              (define-key map (kbd "C-x r u") #'undo-fu-session-save)
              (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t))


(use-package! undo-fu-session
  :unless (modulep! +tree)
  :hook (undo-fu-mode . global-undo-fu-session-mode)
  :custom (undo-fu-session-directory (concat doom-cache-dir "undo-fu-session/"))
  :config
  (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))

  (when (executable-find "zstd")
    ;; There are other algorithms available, but zstd is the fastest, and speed
    ;; is our priority within Emacs
    (setq undo-fu-session-compression 'zst))

  ;; HACK Fix #4993: we've advised `make-backup-file-name-1' to produced SHA1'ed
  ;;      filenames to prevent file paths that are too long, so we force
  ;;      `undo-fu-session--make-file-name' to use it instead of its own
  ;;      home-grown overly-long-filename generator.
  ;; TODO PR this upstream; should be a universal issue
  (defadvice! +undo-fu-make-hashed-session-file-name-a (file)
    :override #'undo-fu-session--make-file-name
    (concat (let ((backup-directory-alist `(("." . ,undo-fu-session-directory))))
              (make-backup-file-name-1 file))
            (undo-fu-session--file-name-ext))))


(use-package! vundo
  :unless (modulep! +tree)
  :when (> emacs-major-version 27)
  :defer t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t)
  (define-key vundo-mode-map [remap doom/escape] #'vundo-quit))


(use-package! undo-tree
  :when (modulep! +tree)
  ;; Branching & persistent undo
  :hook (doom-first-buffer . global-undo-tree-mode)
  :custom (undo-tree-history-directory-alist `(("." . ,(concat doom-cache-dir "undo-tree-hist/"))))
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        ;; Increase undo limits to avoid emacs prematurely truncating the undo
        ;; history and corrupting the tree. This is larger than the undo-fu
        ;; defaults because undo-tree trees consume exponentially more space,
        ;; and then some when `undo-tree-enable-undo-in-region' is involved. See
        ;; syl20bnr/spacemacs#12110
        undo-limit 800000           ; 800kb (default is 160kb)
        undo-strong-limit 12000000  ; 12mb  (default is 240kb)
        undo-outer-limit 128000000) ; 128mb (default is 24mb)

  ;; Compress undo-tree history files with zstd, if available. File size isn't
  ;; the (only) concern here: the file IO barrier is slow for Emacs to cross;
  ;; reading a tiny file and piping it in-memory through zstd is *slightly*
  ;; faster than Emacs reading the entire undo-tree file from the get go (on
  ;; SSDs). Whether or not that's true in practice, we still enjoy zstd's ~80%
  ;; file savings (these files add up over time and zstd is so incredibly fast).
  (when (executable-find "zstd")
    (defadvice! +undo--append-zst-extension-to-file-name-a (file)
      :filter-return #'undo-tree-make-history-save-file-name
      (concat file ".zst")))

  ;; Strip text properties from undo-tree data to stave off bloat. File size
  ;; isn't the concern here; undo cache files bloat easily, which can cause
  ;; freezing, crashes, GC-induced stuttering or delays when opening files.
  (defadvice! +undo--strip-text-properties-a (&rest _)
    :before #'undo-list-transfer-to-tree
    (dolist (item buffer-undo-list)
      (and (consp item)
           (stringp (car item))
           (setcar item (substring-no-properties (car item))))))

  ;; Undo-tree is too chatty about saving its history files. This doesn't
  ;; totally suppress it logging to *Messages*, it only stops it from appearing
  ;; in the echo-area.
  (advice-add #'undo-tree-save-history :around #'doom-shut-up-a))
