;; -*- no-byte-compile: t; -*-
;;; checkers/syntax/packages.el

(package! flycheck :pin "f19a51c0f1")
(package! flycheck-popup-tip :pin "ef86aad907")
(when (featurep! +childframe)
  (package! flycheck-posframe :pin "2b3e94c2e4"))

;; TODO flymake?
