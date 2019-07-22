;;; ui/vc-gutter/autoload.el -*- lexical-binding: t; -*-
;;;###if (featurep! :ui hydra)

;;;###autoload (autoload '+vc/gutter-hydra/body "ui/vc-gutter/autoload" nil t)
(defhydra +vc/gutter-hydra
  (:body-pre (git-gutter-mode 1) :hint nil)
  "
                                               [git gutter]
   Movement  Hunk Actions     Misc.         +%-4s(car (git-gutter:statistic))/ -%-4s(cdr (git-gutter:statistic))
  ╭──────────────────────────────────┴────────────────╯
     ^_g_^       [_s_] stage        [_R_] set start Rev
     ^_k_^       [_r_] revert
     ^↑ ^      [_m_] mark
     ^↓ ^      [_p_] popup             ╭─────────────────────
     ^_j_^                             │[_q_] quit
     ^_G_^                             │[_Q_] Quit and disable"
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
  ("Q" (git-gutter-mode -1) :color blue))
