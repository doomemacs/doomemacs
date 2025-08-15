;;; emacs/undo/doctor.el -*- lexical-binding: t; -*-

(let* ((unwanted (if (modulep! +tree) 'undo-fu 'undo-tree))
       (wanted   (if (modulep! +tree) 'undo-tree 'undo-fu)))
  (when (doom-package-in-module-p unwanted :user)
    (error! "User has installed %S, which is incompatible with %S" unwanted wanted)
    (explain! "Set (or unset) this module's +tree flag to set up one or the other.")))
