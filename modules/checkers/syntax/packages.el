;; -*- no-byte-compile: t; -*-
;;; checkers/syntax/packages.el

(package! flycheck :pin "c02cd773dded0215f9417ec04dfe8dabda63ef43")
(package! flycheck-popup-tip :pin "ef86aad907f27ca076859d8d9416f4f7727619c6")
(when (featurep! +childframe)
  (package! flycheck-posframe :pin "c928b5b5424fe84a0b346e28bd7d461c80b27482"))

;; TODO flymake?
