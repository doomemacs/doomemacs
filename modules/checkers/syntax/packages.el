;; -*- no-byte-compile: t; -*-
;;; checkers/syntax/packages.el

(unless (modulep! +flymake)
  (package! flycheck :pin "16b536b031cbfb5e95a3914ea1e6c1bcadb4d0ad")
  (package! flycheck-popup-tip :pin "ef86aad907f27ca076859d8d9416f4f7727619c6")
  ;; REVIEW: Remove when purcell/package-lint#285 is dealt with.
  (package! package-lint :pin "2dc48e5fb9c37390d9290d4f5ab371c39b7a3829")
  (when (modulep! +childframe)
    (package! flycheck-posframe :pin "19896b922c76a0f460bf3fe8d8ebc2f9ac9028d8")))

(when (modulep! +flymake)
  (package! flymake-popon
    :recipe (:host github :repo "doomelpa/flymake-popon")
    :pin "99ea813346f3edef7220d8f4faeed2ec69af6060"))
