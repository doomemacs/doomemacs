;;; lang/common-lisp/autoload/hydras.el -*- lexical-binding: t; -*-

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
