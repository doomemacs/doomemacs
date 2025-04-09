;;; ui/smooth-scroll/doctor.el -*- lexical-binding: t; -*-

(unless (fboundp 'pixel-scroll-precision-mode)
  (error! "Emacs <= 29 detected. The :ui smooth-scroll module will not function"))
