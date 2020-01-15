;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! cmake-mode
  :recipe (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*"))
  :pin "bfe85bc009c4778b44e246d5c27d0f888f0bfc0c")
(package! cuda-mode :pin "9ae9eacfdba3559b5456342d0d03296290df8ff5")
(package! demangle-mode :pin "06903d731dfde110e10b979dcc7624ef6dbb5ac8")
(package! disaster :pin "10a785facc60d89d78e0d5177985ab1af1741bb4")
(package! modern-cpp-font-lock :pin "02f104701bc34c146d22e3143ae59ef362999098")
(package! opencl-mode :pin "55cb49c8243e6420961d719faced035bc547c1ef")

(when (package! glsl-mode :pin "43d906688a8e2fe650005806eb69bea131d9321a")
  (when (featurep! :completion company)
    (package! company-glsl
      :recipe (:host github :repo "Kaali/company-glsl")
      :pin "404cd0694ab34971f9c01eb22126cd2e7d3f9dc4")))

(if (featurep! +lsp)
    (package! ccls :pin "aab3e31fd716daf59f9794e62d473357263e8cc0")
  (when (package! irony :pin "8387098286132abd6472a8f49750e38ddb8096b6")
    (package! irony-eldoc :pin "0df5831eaae264a25422b061eb2792aadde8b3f2")
    (when (featurep! :checkers syntax)
      (package! flycheck-irony :pin "42dbecd4a865cabeb301193bb4d660e26ae3befe"))
    (when (featurep! :completion company)
      (package! company-irony :pin "b44711dfce445610c1ffaec4951c6ff3882b216a")
      (package! company-irony-c-headers :pin "72c386aeb079fb261d9ec02e39211272f76bbd97")))
  (when (package! rtags :pin "5f1eaf4355e2093afb2b7828f3ebddfcad1234be")
    (when (featurep! :completion ivy)
      (package! ivy-rtags :pin "5f1eaf4355e2093afb2b7828f3ebddfcad1234be"))
    (when (featurep! :completion helm)
      (package! helm-rtags :pin "5f1eaf4355e2093afb2b7828f3ebddfcad1234be"))))
