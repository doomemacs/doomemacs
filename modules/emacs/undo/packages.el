;; -*- no-byte-compile: t; -*-
;;; emacs/undo/packages.el

(if (modulep! +tree)
    (package! undo-tree :pin "d8f72bbe7d3c3a2808986febd3bb1a46d4da7f51")
  (package! undo-fu
    :recipe (:host github :repo "emacsmirror/undo-fu")
    :pin "545e29459e71a9aca81c96c1385d43a5696e27e9")
  (package! undo-fu-session
    :recipe (:host github :repo "emacsmirror/undo-fu-session")
    :pin "366717d88f77182f780829f2041f1a16ca2970c7")
  (when (> emacs-major-version 27)  ; unsupported in 27
    (package! vundo :pin "caad94b3cd11cb7fe4179b9d9e9811686e8a716b")))
