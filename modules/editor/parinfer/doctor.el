;;; editor/parinfer/doctor.el -*- lexical-binding: t; -*-

(unless (fboundp 'module-load)
  (warn! "Your emacs wasn't built with dynamic modules support. `parinfer-rust-mode' won't work"))
(when (and (eq system-type 'berkeley-unix)
           (not (file-readable-p parinfer-rust-library)))
  (warn! (concat "Could not read " parinfer-rust-library ". `parinfer-rust-mode' won't work")))
