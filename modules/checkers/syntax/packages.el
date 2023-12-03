;; -*- no-byte-compile: t; -*-
;;; checkers/syntax/packages.el

(unless (modulep! +flymake)
  (package! flycheck :pin "e56e30d8c66ffc9776d07740658d3b542c1a8e21")
  (package! flycheck-popup-tip :pin "ef86aad907f27ca076859d8d9416f4f7727619c6")
  (when (modulep! +childframe)
    (package! flycheck-posframe :pin "19896b922c76a0f460bf3fe8d8ebc2f9ac9028d8")))

;; Flymake
(when (modulep! +flymake)
  ;; NOTE: remove when straight bumped to support nonGnuELPA
  (package! popon :recipe (:repo "https://codeberg.org/akib/emacs-popon"))
  (package! flymake-popon :recipe (:repo "https://codeberg.org/akib/emacs-flymake-popon")))
