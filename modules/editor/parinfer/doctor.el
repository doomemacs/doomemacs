;;; editor/parinfer/doctor.el -*- lexical-binding: t; -*-

(when (featurep! +rust)
  (unless (fboundp 'module-load)
    (warn! "Your emacs wasn't built with dynamic modules support. `parinfer-rust-mode' won't work"))
  (when (and (eq system-type 'berkeley-unix)
             (not (file-readable-p (concat user-emacs-directory
                                           ".local/etc/parinfer-rust/libparinfer_rust.so"))))
    (warn! (concat "Could not read " user-emacs-directory
                   ".local/etc/parinfer-rust/libparinfer_rust.so. "
                   "`parinfer-rust-mode' won't work"))))
