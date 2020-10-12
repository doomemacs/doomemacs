;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! cmake-mode
  :recipe (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*"))
  :pin "e58c41145a879f0314b2821eada7fd0dc898b6b6")
(package! cuda-mode :pin "9ae9eacfdba3559b5456342d0d03296290df8ff5")
(package! demangle-mode :pin "aaef0bd77a3ea9ce9132e9a53ac021b0f5d33e12")
(package! disaster :pin "10a785facc60d89d78e0d5177985ab1af1741bb4")
(package! modern-cpp-font-lock :pin "865955d0035382a17a7f03add0d00d0bd812b103")
(package! opencl-mode :pin "55cb49c8243e6420961d719faced035bc547c1ef")

(when (package! glsl-mode :pin "b07112016436d9634cd4ef747f9af6b01366d136")
  (when (featurep! :completion company)
    (package! company-glsl
      :recipe (:host github :repo "Kaali/company-glsl")
      :pin "404cd0694ab34971f9c01eb22126cd2e7d3f9dc4")))

(if (featurep! +lsp)
    (unless (featurep! :tools lsp +eglot)
      ;; ccls package is necessary only for lsp-mode.
      (package! ccls :pin "675a5704c14a27931e835a431beea3631d92e8e6"))
  (when (package! irony :pin "1e1aabaa686a08767ab33e5cd43ce8f0ebf8d020")
    (package! irony-eldoc :pin "73e79a89fad982a2ba072f2fcc1b4e41f0aa2978")
    (when (featurep! :checkers syntax)
      (package! flycheck-irony :pin "42dbecd4a865cabeb301193bb4d660e26ae3befe"))
    (when (featurep! :completion company)
      (package! company-irony :pin "b44711dfce445610c1ffaec4951c6ff3882b216a")
      (package! company-irony-c-headers :pin "72c386aeb079fb261d9ec02e39211272f76bbd97")))
  (when (package! rtags :pin "e6c7005bfad5df335ce86378b642ebd33aa66f26")
    (when (featurep! :completion ivy)
      (package! ivy-rtags))
    (when (featurep! :completion helm)
      (package! helm-rtags))))
