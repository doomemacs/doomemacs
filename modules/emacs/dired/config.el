;;; emacs/dired/config.el -*- lexical-binding: t; -*-

(use-package! dired
  :commands dired-jump
  :init
  (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
        ;; don't prompt to revert, just do it
        dired-auto-revert-buffer #'dired-buffer-stale-p
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask
        ;; Where to store image caches
        image-dired-dir (concat doom-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        ;; Screens are larger nowadays, we can afford slightly larger thumbnails
        image-dired-thumb-size 150)
  :config
  (set-popup-rule! "^\\*image-dired"
    :slot 20 :size 0.8 :select t :quit nil :ttl 0)
  (set-evil-initial-state! 'image-dired-display-image-mode 'emacs)

  (let* ((ls (executable-find "ls"))
         (gls (executable-find "gls"))
         (idp (executable-find insert-directory-program))
         (ls-is-gnu? (and ls (= 0 (process-file ls nil nil nil "--version"))))
         (idp-is-gnu-ls?
          (and idp (= 0 (process-file idp nil nil nil "--version")))))
    (setq dired-listing-switches
          "-l --almost-all --human-readable --group-directories-first")
    (setq insert-directory-program
          (cond
           ;; just use GNU ls if found
           (ls-is-gnu? ls)
           ;; use insert-directory-program if it points to GNU ls
           (idp-is-gnu-ls? insert-directory-program)
           ;; heuristic: GNU ls is often installed as gls by Homebrew on Mac
           ((and (eq system-type 'darwin) gls) gls)
           (t
            ;; Emacs 28+ sanitizes unknown switches silently
            (when (< emacs-major-version 28)
              (setq dired-listing-switches "-alh"))
            ;; fallback to insert-directory-program
            insert-directory-program))))

  (defadvice! +dired--no-revert-in-virtual-buffers-a (&rest args)
    "Don't auto-revert in dired-virtual buffers (see `dired-virtual-revert')."
    :before-while #'dired-buffer-stale-p
    (not (eq revert-buffer-function #'dired-virtual-revert)))

  (map! :map dired-mode-map
        ;; To be consistent with ivy/helm+wgrep integration
        "C-c C-e" #'wdired-change-to-wdired-mode))


(use-package! dired-x
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  ;; Disable the prompt about whether I want to kill the Dired buffer for a
  ;; deleted directory. Of course I do!
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  ;; Let OS decide how to open certain files
  (when-let (cmd (cond (IS-MAC "open")
                       (IS-LINUX "xdg-open")
                       (IS-WINDOWS "start")))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))
  (map! :map dired-mode-map
        :localleader
        "h" #'dired-omit-mode))


(use-package! dired-aux
  :defer t
  :config
  (setq dired-create-destination-dirs 'ask
        dired-vc-rename-file t))


(use-package! diredfl
  :hook (dired-mode . diredfl-mode)
  :hook (dirvish-directory-view-mode . diredfl-mode))


(use-package! dirvish
  :defer t
  :general (dired-mode-map "C-c C-r" #'dirvish-rsync)
  :after-call dired-noselect dired dired-jump
  :config
  (dirvish-override-dired-mode)
  (setq dirvish-cache-dir (concat doom-cache-dir "dirvish/"))
  (set-popup-rule! "^ ?\\*Dirvish.*" :ignore t)
  (map! :map dirvish-mode-map
        :n  "?"   #'dirvish-dispatch
        :n  "q"   #'dirvish-quit
        :ng "a"   #'dirvish-quick-access
        :ng "f"   #'dirvish-file-info-menu
        :ng "y"   #'dirvish-yank-menu
        :ng "s"   #'dirvish-quicksort
        :ng "TAB" #'dirvish-subtree-toggle
        :ng "M-t" #'dirvish-layout-toggle
        :ng "M-b" #'dirvish-history-go-backward
        :ng "M-f" #'dirvish-history-go-forward
        :ng "M-n" #'dirvish-narrow
        :ng "M-m" #'dirvish-mark-menu
        :ng "M-s" #'dirvish-setup-menu
        :ng "M-e" #'dirvish-emerge-menu)
  (if (modulep! +dirvish)
      (setq dirvish-attributes '(file-size collapse)
            dirvish-mode-line-format
            '(:left (sort file-time symlink) :right (omit yank index)))
    (setq dirvish-attributes nil
          dirvish-use-header-line nil
          dirvish-mode-line-format nil
          dirvish-hide-details nil
          dirvish-hide-cursor nil))
  (when (modulep! :ui tabs)
    (after! centaur-tabs
      (add-hook! 'dired-mode-hook 'centaur-tabs-local-mode)
      (add-hook! 'dirvish-directory-view-mode-hook 'centaur-tabs-local-mode)))
  (when (modulep! :ui vc-gutter)
    (push 'vc-state dirvish-attributes))
  (when (modulep! +icons)
    (appendq! dirvish-attributes '(all-the-icons subtree-state))))
