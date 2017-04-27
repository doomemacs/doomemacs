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
    (when (and (buffer-file-name)
               (not (file-remote-p (buffer-file-name))))
      (git-gutter-mode +1)))
  (add-hook! (text-mode prog-mode conf-mode) #'+version-control|git-gutter-maybe)
  :config
  (set! :popup "^\\*git-gutter.+\\*$" :regexp t :size 15 :noselect t)

  ;; Update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  (after! evil
    ;; Refreshing git-gutter on ESC
    (advice-add #'evil-force-normal-state :after #'git-gutter)))


(def-package! browse-at-remote
  :commands (browse-at-remote browse-at-remote-get-url))


(def-package! git-timemachine
  :commands (git-timemachine git-timemachine-toggle)
  :config
  ;; Sometimes I forget `git-timemachine' is enabled in a buffer, so instead of
  ;; showing revision details in the minibuffer, show them in
  ;; `header-line-format', which is always visible.
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
  (add-hook 'git-timemachine-mode-hook #'evil-force-normal-state)
  (map! :map git-timemachine-mode-map
        :nv "p" #'git-timemachine-show-previous-revision
        :nv "n" #'git-timemachine-show-next-revision
        :nv "g" #'git-timemachine-show-nth-revision
        :nv "q" #'git-timemachine-quit
        :nv "w" #'git-timemachine-kill-abbreviated-revision
        :nv "W" #'git-timemachine-kill-revision
        :nv "b" #'git-timemachine-blame))


(def-package! magit
  :commands magit-status
  :config
  (set! :popup "^\\*magit" :regexp t)
  (map! :map magit-mode-map
        ;; Don't interfere with window movement keys
        :nv "C-j" nil
        :nv "C-k" nil))


(def-package! evil-magit
  :when (featurep! :feature evil)
  :after magit)

