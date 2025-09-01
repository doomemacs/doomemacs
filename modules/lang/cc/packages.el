;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! cmake-mode
  :recipe (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*"))
  :pin "b08b5d9045308362a623a4f576896d55ffecfd52")
(package! demangle-mode :pin "04f545adab066708d6151f13da65aaf519f8ac4e")
(package! disaster :pin "0299c129d4153e3a794358159737c3ff9d155654")
(package! opencl-mode :pin "6464abf969d916aba83e393b5206b147ac416da3")

(when (package! cuda-mode :pin "c3dae31b3d1abedf4d0b98840127e2cac73d6ad8")
  (when (and (modulep! +tree-sitter) (treesit-available-p))
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
  (package! ccls :pin "34251b799eba70739664c5022c37ddc0a14de872"))
