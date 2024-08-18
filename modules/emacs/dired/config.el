;;; emacs/dired/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

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

  (let ((args (list "-ahl" "-v" "--group-directories-first")))
    (when (featurep :system 'bsd)
      ;; Use GNU ls as `gls' from `coreutils' if available. Add `(setq
      ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning
      ;; when not using GNU ls.
      (if-let (gls (executable-find "gls"))
          (setq insert-directory-program gls)
        ;; BSD ls doesn't support -v or --group-directories-first
        (setq args (list (car args)))))
    (setq dired-listing-switches (string-join args " "))

    (add-hook! 'dired-mode-hook
      (defun +dired-disable-gnu-ls-flags-maybe-h ()
        "Remove extraneous switches from `dired-actual-switches' when it's
uncertain that they are supported (e.g. over TRAMP or on Windows).

Fixes #1703: dired over TRAMP displays a blank screen.
Fixes #3939: unsortable dired entries on Windows."
        (when (or (file-remote-p default-directory)
                  (and (boundp 'ls-lisp-use-insert-directory-program)
                       (not ls-lisp-use-insert-directory-program)))
          (setq-local dired-actual-switches (car args))))))

  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil)

  (defadvice! +dired--no-revert-in-virtual-buffers-a (&rest args)
    "Don't auto-revert in dired-virtual buffers (see `dired-virtual-revert')."
    :before-while #'dired-buffer-stale-p
    (not (eq revert-buffer-function #'dired-virtual-revert)))

  ;; To be consistent with vertico/ivy/helm+wgrep integration
  (define-key dired-mode-map (kbd "C-c C-e") #'wdired-change-to-wdired-mode))


(use-package! dirvish
  :commands dirvish-find-entry-a dirvish-dired-noselect-a
  :general (dired-mode-map "C-c C-r" #'dirvish-rsync)
  :init
  (setq dirvish-cache-dir (file-name-concat doom-cache-dir "dirvish/"))
  ;; HACK: ...
  (advice-add #'dired-find-file :override #'dirvish-find-entry-a)
  (advice-add #'dired-noselect :around #'dirvish-dired-noselect-a)
  :config
  (dirvish-override-dired-mode)
  (set-popup-rule! "^ ?\\*Dirvish.*" :ignore t)

  ;; Don't recycle sessions. We don't want leftover buffers lying around,
  ;; especially if users are reconfiguring Dirvish or trying to recover from an
  ;; error. It's too easy to accidentally break Dirvish (e.g. by focusing the
  ;; header window) at the moment, or get stuck in a focus loop with the buried
  ;; buffers. Starting from scratch isn't even that expensive, anyway.
  (setq dirvish-reuse-session nil)

  ;; A more reserved mode-line height that should match doom-modeline's (or the
  ;; vanilla mode-line's) height.
  (add-hook! 'after-setting-font-hook
    (defun +dired-update-mode-line-heigth-h ()
      ;; REVIEW: Too hardcoded.
      (setq dirvish-mode-line-height (+ (frame-char-height) 4)
            dirvish-header-line-height (+ (frame-char-height) 8))))
  (+dired-update-mode-line-heigth-h)

  (if (modulep! +dirvish)
      (setq dirvish-attributes '(file-size)
            dirvish-mode-line-format
            '(:left (sort file-time symlink) :right (omit yank index)))
    (setq dirvish-attributes nil
          dirvish-use-header-line nil
          dirvish-mode-line-format nil))

  (when (modulep! :ui vc-gutter)
    (push 'vc-state dirvish-attributes))
  (when (modulep! +icons)
    (setq dirvish-subtree-always-show-state t)
    (appendq! dirvish-attributes '(nerd-icons subtree-state)))

  ;; HACK: Doom will treat an integer value for `dirvish-hide-details' to mean
  ;;   hide file/dir details if window is less than N characters wide (e.g. for
  ;;   side windows or small full-window layouts).
  (setq dirvish-hide-details 50)
  ;; TODO: Proc this hook sooner. The delay on `dirvish-setup-hook' is jarring.
  (add-hook! 'dirvish-setup-hook
    (defun +dired-hide-details-in-side-mode-h ()
      (when (integerp dirvish-hide-details)
        (dired-hide-details-mode
         (if (< (window-width dirvish--selected-window) dirvish-hide-details)
             +1 -1)))))

  (when (modulep! :ui tabs)
    (after! centaur-tabs
      (add-hook 'dired-mode-hook #'centaur-tabs-local-mode)
      (add-hook 'dirvish-directory-view-mode-hook #'centaur-tabs-local-mode)))

  ;; TODO: Needs more polished keybinds for non-Evil users
  (map! :map dirvish-mode-map
        :n  "?"   #'dirvish-dispatch
        :n  "q"   #'dirvish-quit
        :n  "b"   #'dirvish-quick-access
        :ng "f"   #'dirvish-file-info-menu
        :n  "p"   #'dirvish-yank
        :ng "S"   #'dirvish-quicksort
        :n  "F"   #'dirvish-layout-toggle
        :n  "z"   #'dirvish-history-jump
        :n  "gh"  #'dirvish-subtree-up
        :n  "gl"  #'dirvish-subtree-down
        :m  "[h"  #'dirvish-history-go-backward
        :m  "]h"  #'dirvish-history-go-forward
        :m  "[e"  #'dirvish-emerge-next-group
        :m  "]e"  #'dirvish-emerge-previous-group
        :n  "TAB" #'dirvish-subtree-toggle
        :ng "M-b" #'dirvish-history-go-backward
        :ng "M-f" #'dirvish-history-go-forward
        :ng "M-n" #'dirvish-narrow
        :ng "M-m" #'dirvish-mark-menu
        :ng "M-s" #'dirvish-setup-menu
        :ng "M-e" #'dirvish-emerge-menu
        (:prefix ("y" . "yank")
         :n "l"   #'dirvish-copy-file-true-path
         :n "n"   #'dirvish-copy-file-name
         :n "p"   #'dirvish-copy-file-path
         :n "r"   #'dirvish-copy-remote-path
         :n "y"   #'dired-do-copy)
        (:prefix ("s" . "symlinks")
         :n "s"   #'dirvish-symlink
         :n "S"   #'dirvish-relative-symlink
         :n "h"   #'dirvish-hardlink))

  ;; HACK: Kill Dirvish session before switching projects/workspaces, otherwise
  ;;   it errors out on trying to delete/change dedicated windows.
  (add-hook! '(persp-before-kill-functions projectile-before-switch-project-hook)
    (defun +dired--cleanup-dirvish-h (&rest _)
      (when-let ((win
                  (or (and (featurep 'dirvish-side)
                           (dirvish-side--session-visible-p))
                      (and dirvish--this (selected-window)))))
        (delete-window win))))


  ;; HACK: If a directory has a .dir-locals.el, its settings could
  ;;   interfere/crash Dirvish trying to preview it.
  ;; REVIEW: Upstream this later.
  (defadvice! +dired--ignore-local-vars-for-dir-previews-a (fn &rest args)
    :around #'dirvish-default-dp
    (let ((result (apply fn args)))
      (if (and (file-directory-p (car args))
               (eq (car-safe result) 'dired))
          `(dired . (,@(butlast (cdr result))
                     ,(format "(let ((enable-local-variables nil)) %s)"
                              (car (last (cdr result))))))
        result)))

  ;; HACK: Dirvish will complain that pdf-tools is required to preview PDFs,
  ;;   even if the package is installed, so I advise it to try autoloading it
  ;;   before complaining, otherwise complain if epdfinfo hasn't been built yet.
  ;; REVIEW: Upstream this later.
  (defadvice! +dired--autoload-pdf-tools-a (fn &rest args)
    :around #'dirvish-pdf-dp
    (when (equal (nth 1 args) "pdf")
      (require 'pdf-tools nil t)
      (if (file-exists-p pdf-info-epdfinfo-program)
          (apply fn args)
        '(info . "`epdfinfo' program required to preview pdfs; run `M-x pdf-tools-install'")))))


(use-package! diredfl
  :hook (dired-mode . diredfl-mode)
  :hook (dirvish-directory-view-mode . diredfl-mode))


(use-package! dired-x
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^\\.DS_Store\\'"
                "\\|^flycheck_.*"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  ;; Disable the prompt about whether I want to kill the Dired buffer for a
  ;; deleted directory. Of course I do!
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  ;; Let OS decide how to open certain files
  (when-let (cmd (cond ((featurep :system 'macos) "open")
                       ((featurep :system 'linux) "xdg-open")
                       ((featurep :system 'windows) "start")))
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
