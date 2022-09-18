;;; completion/ivy/autoload/hydras.el -*- lexical-binding: t; -*-
;;;###if (modulep! :ui hydra)

;;;###autoload
(after! ivy-hydra
  (defhydra+ hydra-ivy (:hint nil :color pink)
    "
 Move     ^^^^^^^^^^ | Call         ^^^^ | Cancel^^ | Options^^ | Action _w_/_s_/_a_: %s(ivy-action-name)
----------^^^^^^^^^^-+--------------^^^^-+-------^^-+--------^^-+---------------------------------
 _g_ ^ ^ _k_ ^ ^ _u_ | _f_orward _o_ccur | _i_nsert | _c_alling: %-7s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
 ^↨^ _h_ ^+^ _l_ ^↕^ | _RET_ done     ^^ | _q_uit   | _m_atcher: %-7s(ivy--matcher-desc) _t_runcate: %-11`truncate-lines
 _G_ ^ ^ _j_ ^ ^ _d_ | _TAB_ alt-done ^^ | ^ ^      | _<_/_>_: shrink/grow
"
    ;; arrows
    ("l" ivy-alt-done)
    ("h" ivy-backward-delete-char)
    ("g" ivy-beginning-of-buffer)
    ("G" ivy-end-of-buffer)
    ("d" ivy-scroll-up-command)
    ("u" ivy-scroll-down-command)
    ("e" ivy-scroll-down-command)
    ;; actions
    ("q" keyboard-escape-quit :exit t)
    ("<escape>" keyboard-escape-quit :exit t)
    ("TAB" ivy-alt-done :exit nil)
    ("RET" ivy-done :exit t)
    ("C-SPC" ivy-call-and-recenter :exit nil)
    ("f" ivy-call)
    ("c" ivy-toggle-calling)
    ("m" ivy-toggle-fuzzy)
    ("t" (setq truncate-lines (not truncate-lines)))
    ("o" ivy-occur :exit t)))
