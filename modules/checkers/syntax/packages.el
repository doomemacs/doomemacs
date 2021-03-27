;; -*- no-byte-compile: t; -*-
;;; checkers/syntax/packages.el

(package! flycheck :pin "f8c679fff349850c80541a31de50009c3c15d4c9")
(package! flycheck-popup-tip :pin "ef86aad907f27ca076859d8d9416f4f7727619c6")
(when (featurep! +childframe)
  (package! flycheck-posframe :pin "8f60c9bf124ab9597d681504a73fdf116a0bde12"))

;; TODO flymake?
