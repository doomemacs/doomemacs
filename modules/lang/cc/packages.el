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
    (package! ccls :pin "17ec7bb4cf")
  (when (package! irony :pin "5f75fc0c92")
    (package! irony-eldoc :pin "0df5831eaa")
    (when (featurep! :checkers syntax)
      (package! flycheck-irony :pin "42dbecd4a8"))
    (when (featurep! :completion company)
      (package! company-irony :pin "b44711dfce")
      (package! company-irony-c-headers :pin "72c386aeb0")))
  (when (package! rtags :pin "d370c09007")
    (when (featurep! :completion ivy)
      (package! ivy-rtags))
    (when (featurep! :completion helm)
      (package! helm-rtags))))

(when (featurep! +google-c-style)
  (package! google-c-style
    :pin "35fd3f01678c63593b80c4abbf1691a79fed2dfe"
    :recipe (:host github :repo "google/styleguide" :files ("google-c-style.el"))))
