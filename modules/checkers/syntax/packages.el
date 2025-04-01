;; -*- no-byte-compile: t; -*-
;;; checkers/syntax/packages.el

(unless (modulep! +flymake)
  (package! flycheck :pin "b9db1379dcc3e59238dc1fdd7db368c66e8734ba")
  (package! flycheck-popup-tip :pin "ef86aad907f27ca076859d8d9416f4f7727619c6")
  ;; REVIEW: Remove when purcell/package-lint#285 is dealt with.
  (package! package-lint :pin "43012b41ac8d1a0ce6118c432c9b822c0f1a1981")
  (when (modulep! +childframe)
    (package! flycheck-posframe :pin "19896b922c76a0f460bf3fe8d8ebc2f9ac9028d8")))

(when (modulep! +flymake)
  (package! flymake-popon
    :recipe (:host github :repo "doomelpa/flymake-popon")
    :pin "99ea813346f3edef7220d8f4faeed2ec69af6060"))
