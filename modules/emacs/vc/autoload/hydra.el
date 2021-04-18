;;; emacs/vc/autoload/hydra.el -*- lexical-binding: t; -*-
;;;###if (featurep! :ui hydra)

;;;###autoload (autoload '+vc/smerge-hydra/body "emacs/vc/autoload/hydra" nil t)
(defhydra +vc/smerge-hydra (:hint nil
                            :pre (if (not smerge-mode) (smerge-mode 1))
                            ;; Disable `smerge-mode' when quitting hydra if
                            ;; no merge conflicts remain.
                            :post (smerge-auto-leave))
    "
                                                         [smerge]
  Movement   Keep           Diff              Other
  ╭─────────────────────────────────────────────────────────╯
     ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
     ^_C-k_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
     ^_k_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
     ^_j_ ↓^     [_a_] all        [_H_] hightlight
     ^_C-j_^     [_RET_] current  [_E_] ediff                 ╭──────────
     ^_G_^                                                │ [_q_] quit
"
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
    ("q" nil :color blue))
