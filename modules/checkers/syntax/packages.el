;; -*- no-byte-compile: t; -*-
;;; checkers/syntax/packages.el

(package! flycheck :pin "21d52264aa80bfa4ede94c59e37a20fb6d033b0c")
(package! flycheck-popup-tip :pin "ef86aad907f27ca076859d8d9416f4f7727619c6")
(when (featurep! +childframe)
  (package! flycheck-posframe :pin "8f60c9bf124ab9597d681504a73fdf116a0bde12"))

;; TODO flymake?
