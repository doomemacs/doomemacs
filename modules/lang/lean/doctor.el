;;; lang/lean/doctor.el -*- lexical-binding: t; -*-

(assert! (or (not (modulep! +lsp))
             (modulep! :tools lsp))
         "This module requires (:tools lsp)")

(unless (executable-find "lake")
  (error! "lake executable is missing. Lean 4 support will be limited"))

(when (modulep! +v3)
  (unless (executable-find "lean")
    (error! "lean executable is missing. Most lean-* commands won't work")))
