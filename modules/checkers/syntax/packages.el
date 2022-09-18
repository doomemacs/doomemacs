;; -*- no-byte-compile: t; -*-
;;; checkers/syntax/packages.el

(package! flycheck :pin "784f184cdd9f9cb4e3dbb997c09d93e954142842")
(package! flycheck-popup-tip :pin "ef86aad907f27ca076859d8d9416f4f7727619c6")
(when (modulep! +childframe)
  (package! flycheck-posframe :pin "8f60c9bf124ab9597d681504a73fdf116a0bde12"))

;; TODO flymake?
