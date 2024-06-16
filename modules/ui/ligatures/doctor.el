;; This cond expression mimics the activation conditional of ligatures,
;; with a fallback that triggers a warning.
(cond
 ((if (featurep :system 'macos)
      (fboundp 'mac-auto-operator-composition-mode))
  (ignore))

 ((and (> emacs-major-version 27)
       (or (featurep 'ns)
           (string-match-p "HARFBUZZ" system-configuration-features))
       (featurep 'composite))           ; Emacs loads `composite' at startup
  (ignore))

 ((if (featurep :system 'macos)
      (warn! "The (:ui ligatures) module does not support your version of Emacs. Install emacs-plus with at least Emacs 28, or emacs-mac.")
    (warn! "The (:ui ligatures) module does not support your version of Emacs. Make sure to have at least Emacs 28 with Harfbuzz configured (should be the default)."))))
