;;; feature/version-control/config.el -*- lexical-binding: t; -*-

(setq vc-make-backup-files nil)

(defvar +vcs-auto-hydra-smerge t
  "When entering `smerge-mode' automatically open associated hydra.")

(load! +git)
;; (load! +hg)

(after! vc-annotate
  (set! :popup
    '("*vc-diff*" :size 15 :noselect t)
    '("*vc-change-log*" :size 15)
    '(vc-annotate-mode :same t))

  (set! :evil-state 'vc-annotate-mode 'normal)
  (set! :evil-state 'vc-git-log-view-mode 'normal))

(def-package! smerge-mode
  :init
  (add-hook 'find-file-hook #'+vcs|enable-smerge-mode-maybe)
  :config
  (when (featurep! :feature hydra)
    (require 'hydra)

    (when (version< emacs-version "26")
      (defalias 'smerge-keep-upper 'smerge-keep-mine)
      (defalias 'smerge-keep-lower 'smerge-keep-other)
      (defalias 'smerge-diff-base-upper 'smerge-diff-base-mine)
      (defalias 'smerge-diff-upper-lower 'smerge-diff-mine-other)
      (defalias 'smerge-diff-base-lower 'smerge-diff-base-other))

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
      ("q" nil :color blue))))
