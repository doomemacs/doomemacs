;;; app/everywhere/doctor.el -*- lexical-binding: t; -*-

(when (featurep :system 'windows)
  (error! "emacs-everywhere package does not support windows."))

(when (featurep :system 'linux)
  (let (unmet-deps)
    (dolist (dep '("xclip" "xdotool" "xprop" "xwininfo"))
      (unless (executable-find dep)
        (push dep unmet-deps)))
    (when unmet-deps
      (error! "Unmet dependencies: %s" (string-join unmet-deps ", ")))))
