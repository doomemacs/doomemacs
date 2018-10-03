;;; lang/common-lisp/autoload/hydras.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+common-lisp/navigation/body "lang/common-lisp/autoload/hydras" nil nil)
(defhydra +common-lisp/navigation (:exit nil :hint nil :foreign-keys run)
  "
^^Definitions                           ^^Compiler Notes             ^^Stickers
^^^^^^─────────────────────────────────────────────────────────────────────────────────────
[_g_] Jump to definition                [_n_] Next compiler note     [_s_] Next sticker
[_G_] Jump to definition (other window) [_N_] Previous compiler note [_S_] Previous sticker
[_b_] Pop from definition
[_q_] Exit
"
  ("g" sly-edit-definition)
  ("G" sly-edit-definition-other-window)
  ("b" sly-pop-find-definition-stack)
  ("n" sly-next-note)
  ("N" sly-previous-note)
  ("s" sly-stickers-next-sticker)
  ("S" sly-stickers-prev-sticker)
  ("q" nil :exit t))

;;;###autoload (autoload '+common-lisp/macrostep/body "lang/common-lisp/autoload/hydras" nil nil)
(defhydra +common-lisp/macrostep (:exit nil :hint nil :foreign-keys run)
  "
Macro Expansion
^^Definitions                           ^^Compiler Notes             ^^Stickers
^^^^^^─────────────────────────────────────────────────────────────────────────────────────
[_e_] Expand
[_c_] Collapse
[_n_] Next level
[_N_] Previous level
[_q_] Exit
"
  ("e" macrostep-expand)
  ("c" macrostep-collapse)
  ("n" macrostep-next-macro)
  ("N" macrostep-prev-macro)
  ("q" macrostep-collapse-all :exit t))
