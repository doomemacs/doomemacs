;;; feature/version-control/+git.el -*- lexical-binding: t; -*-

(setq git-commit-fill-column 72)

(when (featurep! :feature evil)
  (add-hook 'git-commit-mode-hook #'evil-insert-state))


(def-package! git-gutter-fringe
  :defer t
  :init
  (defun +version-control|git-gutter-maybe ()
    "Enable `git-gutter-mode' in non-remote buffers."
    (when (and (buffer-file-name)
               (not (file-remote-p (buffer-file-name))))
      (require 'git-gutter-fringe)
      (git-gutter-mode +1)))
  (add-hook! (text-mode prog-mode conf-mode) #'+version-control|git-gutter-maybe)
  :config
  (set! :popup "^\\*git-gutter" nil '((select)))

  ;; Update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  (defun +version-control|update-git-gutter (&rest _)
    "Refresh git-gutter on ESC. Return nil to prevent shadowing other
`doom-escape-hook' hooks."
    (when git-gutter-mode
      (ignore (git-gutter))))

  (add-hook 'doom-escape-hook #'+version-control|update-git-gutter t)

  (defhydra +version-control@git-gutter
    (:body-pre (git-gutter-mode 1) :hint nil)
    "
                                     ╭─────────────────┐
  Movement   Hunk Actions     Misc.  │ gg: +%-4s(car (git-gutter:statistic))/ -%-3s(cdr (git-gutter:statistic)) │
  ╭──────────────────────────────────┴─────────────────╯
     ^_g_^       [_s_] stage        [_R_] set start Rev
     ^_k_^       [_r_] revert
     ^↑ ^      [_m_] mark
     ^↓ ^      [_p_] popup          ╭──────────────────────
     ^_j_^                          │[_q_] quit
     ^_G_^                          │[_Q_] Quit and disable"
    ("j" (progn (git-gutter:next-hunk 1) (recenter)))
    ("k" (progn (git-gutter:previous-hunk 1) (recenter)))
    ("g" (progn (goto-char (point-min)) (git-gutter:next-hunk 1)))
    ("G" (progn (goto-char (point-min)) (git-gutter:previous-hunk 1)))
    ("s" git-gutter:stage-hunk)
    ("r" git-gutter:revert-hunk)
    ("m" git-gutter:mark-hunk)
    ("p" git-gutter:popup-hunk)
    ("R" git-gutter:set-start-revision)
    ("q" nil :color blue)
    ("Q" (git-gutter-mode -1) :color blue)))


(def-package! git-timemachine
  :defer t
  :config
  ;; Sometimes I forget `git-timemachine' is enabled in a buffer, so instead of
  ;; showing revision details in the minibuffer, show them in
  ;; `header-line-format', which has better visibility.
  (setq git-timemachine-show-minibuffer-details t)
  (advice-add #'git-timemachine--show-minibuffer-details :override #'+vcs*update-header-line)

  (after! evil
    ;; Force evil to rehash keybindings for the current state
    (add-hook 'git-timemachine-mode-hook #'evil-force-normal-state)))
