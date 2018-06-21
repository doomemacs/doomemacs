;;; emacs/vc/config.el -*- lexical-binding: t; -*-

(defvar +vc-auto-hydra-smerge t
  "When entering `smerge-mode' automatically open associated hydra.")


;;
;; Plugins
;;

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
;; see https://chris.beams.io/posts/git-commit/
(setq git-commit-fill-column 72
      git-commit-summary-max-length 50
      git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
(when (featurep! :feature evil)
  (add-hook 'git-commit-mode-hook #'evil-insert-state))


;;
;; `vc'
;;

;; `vc-hooks'
(setq vc-make-backup-files nil)

;; `vc-annotate'
(after! vc-annotate
  (set-popup-rules!
    '(("^\\vc-d" :select nil)     ; *vc-diff*
      ("^\\vc-c" :select t))) ; *vc-change-log*
  (set-evil-initial-state!
    '(vc-annotate-mode vc-git-log-view-mode)
    'normal))

;; `smerge-mode'
(defun +vcs|enable-smerge-mode-maybe ()
  "Auto-enable `smerge-mode' when merge conflict is detected."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil :noerror)
      (smerge-mode 1)
      (when (and (featurep 'hydra) +vc-auto-hydra-smerge)
        (+hydra-smerge/body)))))
(add-hook 'find-file-hook #'+vcs|enable-smerge-mode-maybe)

(after! smerge-mode ; built-in
  (unless EMACS26+
    (with-no-warnings
      (defalias #'smerge-keep-upper #'smerge-keep-mine)
      (defalias #'smerge-keep-lower #'smerge-keep-other)
      (defalias #'smerge-diff-base-upper #'smerge-diff-base-mine)
      (defalias #'smerge-diff-upper-lower #'smerge-diff-mine-other)
      (defalias #'smerge-diff-base-lower #'smerge-diff-base-other)))

  (defhydra +hydra-smerge (:hint nil
                           :pre (smerge-mode 1)
                           ;; Disable `smerge-mode' when quitting hydra if
                           ;; no merge conflicts remain.
                           :post (smerge-auto-leave))
    "
                                                    ╭────────┐
  Movement   Keep           Diff              Other │ smerge │
  ╭─────────────────────────────────────────────────┴────────╯
     ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
     ^_C-k_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
     ^_k_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
     ^_j_ ↓^     [_a_] all        [_H_] hightlight
     ^_C-j_^     [_RET_] current  [_E_] ediff             ╭──────────
     ^_G_^                                            │ [_q_] quit"
    ("g" (progn (goto-char (point-min)) (smerge-next)))
    ("G" (progn (goto-char (point-max)) (smerge-prev)))
    ("C-j" smerge-next)
    ("C-k" smerge-prev)
    ("j" next-line)
    ("k" previous-line)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("H" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("R" smerge-kill-current)
    ("q" nil :color blue)))

