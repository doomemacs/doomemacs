;;; tools/dired/config.el -*- lexical-binding: t; -*-

(use-package! dired
  :commands dired-jump
  :init
  (setq dired-auto-revert-buffer t  ; don't prompt to revert; just do it
        dired-dwim-target t  ; suggest a target for moving/copying intelligently
        dired-hide-details-hide-symlink-targets nil
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
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

  (let ((args (list "-aBhl" "--group-directories-first")))
    (when IS-BSD
      ;; Use GNU ls as `gls' from `coreutils' if available. Add `(setq
      ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning
      ;; when not using GNU ls.
      (if-let (gls (executable-find "gls"))
          (setq insert-directory-program gls)
        ;; BSD ls doesn't support --group-directories-first
        (setq args (delete "--group-directories-first" args))))
    (setq dired-listing-switches (string-join args " ")))

  (add-hook! 'dired-mode-hook
    (defun +dired-disable-gnu-ls-flags-in-tramp-buffers-h ()
      "Fix #1703: dired over TRAMP displays a blank screen.

This is because there's no guarantee the remote system has GNU ls, which is the
only variant that supports --group-directories-first."
      (when (file-remote-p default-directory)
        (setq-local dired-listing-switches
                    (string-join
                     (split-string dired-listing-switches
                                   "--group-directories-first")
                     " ")))))

  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil)

  (map! :map dired-mode-map
        ;; Kill all dired buffers on q
        :ng "q" #'+dired/quit-all
        ;; To be consistent with ivy/helm+wgrep integration
        "C-c C-e" #'wdired-change-to-wdired-mode))


(use-package! dired-rsync
  :general (dired-mode-map "C-c C-r" #'dired-rsync))


(use-package! diredfl
  :hook (dired-mode . diredfl-mode))


(use-package! diff-hl
  :hook (dired-mode . diff-hl-dired-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; use margin instead of fringe
  (diff-hl-margin-mode))


(use-package! ranger
  :when (featurep! +ranger)
  :after dired
  :init (setq ranger-override-dired t)
  :config
  (unless (file-directory-p image-dired-dir)
    (make-directory image-dired-dir))

  (set-popup-rule! "^\\*ranger" :ignore t)

  (defadvice! +dired--cleanup-header-line-a ()
    "Ranger fails to clean up `header-line-format' when it is closed, so..."
    :before #'ranger-revert
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (equal header-line-format '(:eval (ranger-header-line)))
            (setq header-line-format nil))))))

  (defadvice! +dired--cleanup-mouse1-bind-a ()
    "Ranger binds an anonymous function to mouse-1 after previewing a buffer
that prevents the user from escaping the window with the mouse. This command is
never cleaned up if the buffer already existed before ranger was initialized, so
we have to clean it up ourselves."
    :after #'ranger-setup-preview
    (when (window-live-p ranger-preview-window)
      (with-current-buffer (window-buffer ranger-preview-window)
        (local-unset-key [mouse-1]))))

  (setq ranger-cleanup-on-disable t
        ranger-excluded-extensions '("mkv" "iso" "mp4")
        ranger-deer-show-details t
        ranger-max-preview-size 10
        ranger-show-literal nil
        ranger-hide-cursor nil))


(use-package! all-the-icons-dired
  :when (featurep! +icons)
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  ;; HACK Fixes #1929: icons break file renaming in Emacs 27+, because the icon
  ;;      is considered part of the filename, so we disable icons while we're in
  ;;      wdired-mode.
  (when EMACS27+
    (defvar +wdired-icons-enabled -1)

    (defadvice! +dired-disable-icons-in-wdired-mode-a (&rest _)
      :before #'+wdired-before-start-advice
      (setq-local +wdired-icons-enabled (if all-the-icons-dired-mode 1 -1))
      (when all-the-icons-dired-mode
        (all-the-icons-dired-mode -1)))

    (defadvice! +dired-restore-icons-after-wdired-mode-a (&rest _)
      :after #'+wdired-after-finish-advice
      (all-the-icons-dired-mode +wdired-icons-enabled))))


(use-package! dired-x
  :unless (featurep! +ranger)
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store\\'"
                "\\|^.project\\(?:ile\\)?\\'"
                "\\|^.\\(svn\\|git\\)\\'"
                "\\|^.ccls-cache\\'"
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


(use-package! fd-dired
  :when (executable-find doom-projectile-fd-binary)
  :defer t
  :init
  (global-set-key [remap find-dired] #'fd-dired)
  (set-popup-rule! "^\\*F\\(?:d\\|ind\\)\\*$" :ignore t))


;;;###package dired-git-info
(map! :after dired
      :map (dired-mode-map ranger-mode-map)
      :ng ")" #'dired-git-info-mode)
(after! wdired
  ;; Temporarily disable `dired-git-info-mode' when entering wdired, due to
  ;; reported incompatibilities.
  (defvar +dired--git-info-p nil)
  (defadvice! +dired--disable-git-info-a (&rest _)
    :before #'wdired-change-to-wdired-mode
    (setq +dired--git-info-p (bound-and-true-p dired-git-info-mode))
    (when +dired--git-info-p
      (dired-git-info-mode -1)))
  (defadvice! +dired--reactivate-git-info-a (&rest _)
    :after '(wdired-exit
             wdired-abort-changes
             wdired-finish-edit)
    (when +dired--git-info-p
      (dired-git-info-mode +1))))
