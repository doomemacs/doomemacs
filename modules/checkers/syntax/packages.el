;; -*- no-byte-compile: t; -*-
;;; checkers/syntax/packages.el

(package! flycheck :pin "cd8e0a280c9980c8c7ce31fd2458df7fd81a0acf")
(package! flycheck-popup-tip :pin "ef86aad907f27ca076859d8d9416f4f7727619c6")
(when (featurep! +childframe)
  (package! flycheck-posframe :pin "c928b5b5424fe84a0b346e28bd7d461c80b27482"))

;; TODO flymake?
