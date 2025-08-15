;;; tools/magit/doctor.el -*- lexical-binding: t; -*-

(when (and (modulep! +forge)
           (version< emacs-version "29.1"))
  (error! "+forge requires Emacs 29.1 or newer"))
