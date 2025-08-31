;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! cmake-mode
  :recipe (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*"))
  :pin "b08b5d9045308362a623a4f576896d55ffecfd52")
(package! demangle-mode :pin "04f545adab066708d6151f13da65aaf519f8ac4e")
(package! disaster :pin "8b445913221feb0c196e943106643040118bcd77")
(package! opencl-mode :pin "204d5d9e0f5cb2cbe810f2933230eb08fe2c7695")

(when (package! cuda-mode :pin "c3dae31b3d1abedf4d0b98840127e2cac73d6ad8")
  (when (modulep! +tree-sitter)
    (package! cuda-ts-mode
      :recipe (:host github :repo "Ergus/cuda-ts-mode")
      :pin "807f15150deb3a3060bc36a0e135a27876d7e239")))

(when (package! glsl-mode :pin "86e6bb6cf28d1053366039683a4498401bab9c47")
  (when (modulep! :completion company)
    (package! company-glsl
      :recipe (:host github :repo "Kaali/company-glsl")
      :pin "404cd0694ab34971f9c01eb22126cd2e7d3f9dc4")))

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! ccls :pin "5636ee6c501355eb0acc75f71569688857f8c10d"))
