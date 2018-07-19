;;; emacs/vc/config.el -*- lexical-binding: t; -*-

;; `git-timemachine'
(after! git-timemachine
  ;; Sometimes I forget `git-timemachine' is enabled in a buffer, so instead of
  ;; showing revision details in the minibuffer, show them in
  ;; `header-line-format', which has better visibility.
  (setq git-timemachine-show-minibuffer-details t)
  (advice-add #'git-timemachine--show-minibuffer-details :override #'+vc*update-header-line)

  (after! evil
    ;; Force evil to rehash keybindings for the current state
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))


;; `git-commit-mode'
(defun +vc|enforce-git-commit-conventions ()
  "See https://chris.beams.io/posts/git-commit/"
  (setq fill-column 72
        git-commit-summary-max-length 50
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)))
(add-hook 'git-commit-mode-hook #'+vc|enforce-git-commit-conventions)
(when (featurep! :feature evil)
  (add-hook 'git-commit-mode-hook #'evil-insert-state))


;;
;; `vc' (built-in)
;;

;; `vc-hooks'
(setq vc-make-backup-files nil)

;; `vc-annotate' (built-in)
(after! vc-annotate
  (set-popup-rules!
    '(("^\\vc-d" :select nil) ; *vc-diff*
      ("^\\vc-c" :select t))) ; *vc-change-log*
  (set-evil-initial-state!
    '(vc-annotate-mode vc-git-log-view-mode)
    'normal))

;; `smerge-mode' (built-in)
(after! smerge-mode
  (unless EMACS26+
    (with-no-warnings
      (defalias #'smerge-keep-upper #'smerge-keep-mine)
      (defalias #'smerge-keep-lower #'smerge-keep-other)
      (defalias #'smerge-diff-base-upper #'smerge-diff-base-mine)
      (defalias #'smerge-diff-upper-lower #'smerge-diff-mine-other)
      (defalias #'smerge-diff-base-lower #'smerge-diff-base-other))))

