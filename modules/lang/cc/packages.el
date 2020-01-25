;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! cmake-mode
  :recipe (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*"))
  :pin "bfe85bc009")
(package! cuda-mode :pin "9ae9eacfdb")
(package! demangle-mode :pin "06903d731d")
(package! disaster :pin "10a785facc")
(package! modern-cpp-font-lock :pin "02f104701b")
(package! opencl-mode :pin "55cb49c824")

(when (package! glsl-mode :pin "43d906688a")
  (when (featurep! :completion company)
    (package! company-glsl
      :recipe (:host github :repo "Kaali/company-glsl")
      :pin "404cd0694a")))

(if (featurep! +lsp)
    (package! ccls :pin "aab3e31fd7")
  (when (package! irony :pin "8387098286")
    (package! irony-eldoc :pin "0df5831eaa")
    (when (featurep! :checkers syntax)
      (package! flycheck-irony :pin "42dbecd4a8"))
    (when (featurep! :completion company)
      (package! company-irony :pin "b44711dfce")
      (package! company-irony-c-headers :pin "72c386aeb0")))
  (when (package! rtags :pin "92c5126e98")
    (when (featurep! :completion ivy)
      (package! ivy-rtags :pin "92c5126e98"))
    (when (featurep! :completion helm)
      (package! helm-rtags :pin "92c5126e98"))))
