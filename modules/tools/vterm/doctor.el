;;; tools/vterm/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "vterm-ctrl")
  (warn! "Couldn't find libvterm library. Please install it on your system"))

(unless (executable-find "make")
  (warn! "Couldn't find make command. Please install it on your system"))

(unless (executable-find "cmake")
  (warn! "Couldn't find cmake command. Please install it on your system"))

(unless (string-match-p "MODULES" system-configuration-features)
  (error! "You have to compile emacs with MODULES support"))
