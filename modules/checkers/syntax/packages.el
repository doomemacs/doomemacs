;; -*- no-byte-compile: t; -*-
;;; checkers/syntax/packages.el

(unless (modulep! +flymake)
  (package! flycheck :pin "e8d1472aeab6ac4e19c8339e6be93e91e878f819")
  (package! flycheck-popup-tip :pin "ef86aad907f27ca076859d8d9416f4f7727619c6")
  (when (modulep! +childframe)
    (package! flycheck-posframe :pin "19896b922c76a0f460bf3fe8d8ebc2f9ac9028d8")))

(when (modulep! +flymake)
  (package! flymake-popon
    :recipe (:host github :repo "doomelpa/flymake-popon")
    :pin "99ea813346f3edef7220d8f4faeed2ec69af6060"))
