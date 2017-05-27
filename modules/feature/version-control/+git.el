;;; feature/version-control/+git.el

(def-package! gitconfig-mode
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$")


(def-package! gitignore-mode
  :mode "/\\.gitignore$")


(def-package! git-gutter-fringe
  :commands git-gutter-mode
  :init
  (defun +version-control|git-gutter-maybe ()
    "Enable `git-gutter-mode' in non-remote buffers."
    (when (and (buffer-file-name)
               (not (file-remote-p (buffer-file-name))))
      (git-gutter-mode +1)))
  (add-hook! (text-mode prog-mode conf-mode) #'+version-control|git-gutter-maybe)
  :config
  (set! :popup "^\\*git-gutter.+\\*$" :regexp t :size 15 :noselect t)

  ;; Update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  (after! evil
    (defun +version-control|update-git-gutter ()
      "Refresh git-gutter on ESC. Return nil to prevent shadowing other
`+evil-esc-hook' hooks."
      (ignore (git-gutter)))
    (add-hook '+evil-esc-hook #'+version-control|update-git-gutter t)))


(def-package! git-timemachine
  :commands (git-timemachine git-timemachine-toggle)
  :config
  (require 'magit-blame)

  ;; Sometimes I forget `git-timemachine' is enabled in a buffer, so instead of
  ;; showing revision details in the minibuffer, show them in
  ;; `header-line-format', which has better visibility.
  (setq git-timemachine-show-minibuffer-details nil)

  (defun +vcs|toggle-header-line ()
    (if git-timemachine-mode
        (+vcs*update-header-line)
      (setq-local header-line-format nil)))

  (defun +vcs*update-header-line (&rest _)
    (when (and git-timemachine-mode git-timemachine-revision)
      (let* ((revision git-timemachine-revision)
             (date-relative (nth 3 revision))
             (date-full (nth 4 revision))
             (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
             (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
        (setq-local
         header-line-format
         (format "%s%s [%s (%s)]"
                 (propertize author 'face 'git-timemachine-minibuffer-author-face)
                 (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face)
                 date-full date-relative)))))

  (add-hook 'git-timemachine-mode-hook #'+vcs|toggle-header-line)
  (advice-add #'git-timemachine-show-revision :after #'+vcs*update-header-line)

  ;; Force evil to rehash keybindings for the current state
  (add-hook 'git-timemachine-mode-hook #'evil-force-normal-state))


(def-package! magit
  :commands (magit-status magit-blame))


(def-package! git-link
  :commands (git-link git-link-commit git-link-homepage))


(def-package! evil-magit
  :when (featurep! :feature evil)
  :after magit
  :init (setq evil-magit-want-horizontal-movement t))

