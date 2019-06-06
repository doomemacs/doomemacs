;;; tools/dired/config.el -*- lexical-binding: t; -*-

(def-package! dired
  :commands dired-jump
  :init
  (setq ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Auto refresh dired, but be quiet about it
        dired-hide-details-hide-symlink-targets nil
        ;; files
        image-dired-dir (concat doom-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image"))
  :config
  (let ((args (list "-aBhl" "--group-directories-first")))
    (when IS-BSD
      ;; Use GNU ls as `gls' from `coreutils' if available. Add `(setq
      ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning
      ;; when not using GNU ls.
      (if-let* ((gls (executable-find "gls")))
          (setq insert-directory-program gls)
        ;; BSD ls doesn't support --group-directories-first
        (setq args (delete "--group-directories-first" args))))
    (setq dired-listing-switches (string-join args " ")))

  (define-key! dired-mode-map
    ;; Kill buffer when quitting dired buffers
    [remap quit-window] (λ! (quit-window t))
    ;; To be consistent with ivy/helm+wgrep integration
    "C-c C-e" #'wdired-change-to-wdired-mode))


(def-package! dired-rsync
  :general (dired-mode-map "C-c C-r" #'dired-rsync))


(def-package! diredfl
  :hook (dired-mode . diredfl-mode))


(def-package! dired-k
  :hook (dired-initial-position . dired-k)
  :hook (dired-after-readin . dired-k-no-revert)
  :config
  (setq dired-k-style 'git
        dired-k-padding 1)

  ;; Don't highlight based on mtime, this interferes with diredfl and is more
  ;; confusing than helpful.
  (advice-add #'dired-k--highlight-by-file-attribyte :override #'ignore)

  (defun +dired*interrupt-process (orig-fn &rest args)
    "Fixes dired-k killing git processes too abruptly, leaving behind disruptive
.git/index.lock files."
    (cl-letf (((symbol-function #'kill-process)
               (symbol-function #'interrupt-process)))
      (apply orig-fn args)))
  (advice-add #'dired-k--start-git-status :around #'+dired*interrupt-process)

  (defun +dired*dired-k-highlight (orig-fn &rest args)
    "Butt out if the requested directory is remote (i.e. through tramp)."
    (unless (file-remote-p default-directory)
      (apply orig-fn args)))
  (advice-add #'dired-k--highlight :around #'+dired*dired-k-highlight))


(def-package! ranger
  :when (featurep! +ranger)
  :after dired
  :init
  ;; set up image-dired to allow picture resize
  (setq image-dired-dir (concat doom-cache-dir "image-dir")
        ranger-override-dired t)
  :config
  (unless (file-directory-p image-dired-dir)
    (make-directory image-dired-dir))

  (set-popup-rule! "^\\*ranger" :ignore t)

  (defun +dired*cleanup-header-line ()
    "Ranger fails to clean up `header-line-format' when it is closed, so..."
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (equal header-line-format '(:eval (ranger-header-line)))
            (setq header-line-format nil))))))
  (advice-add #'ranger-revert :before #'+dired*cleanup-header-line)

  (defun +dired*cleanup-mouse1-bind ()
    "Ranger binds an anonymous function to mouse-1 after previewing a buffer
that prevents the user from escaping the window with the mouse. This command is
never cleaned up if the buffer already existed before ranger was initialized, so
we have to clean it up ourselves."
    (when (window-live-p ranger-preview-window)
      (with-current-buffer (window-buffer ranger-preview-window)
        (local-unset-key [mouse-1]))))
  (advice-add #'ranger-setup-preview :after #'+dired*cleanup-mouse1-bind)

  (setq ranger-cleanup-on-disable t
        ranger-excluded-extensions '("mkv" "iso" "mp4")
        ranger-deer-show-details t
        ranger-max-preview-size 10
        ranger-show-literal nil
        ranger-hide-cursor nil))


(def-package! all-the-icons-dired
  :when (featurep! +icons)
  :hook (dired-mode . all-the-icons-dired-mode))


(def-package! dired-x
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil))
