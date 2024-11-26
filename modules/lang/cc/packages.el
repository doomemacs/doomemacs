;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! cmake-mode
  :recipe (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*"))
  :pin "b08b5d9045308362a623a4f576896d55ffecfd52")
(package! cuda-mode :pin "c3dae31b3d1abedf4d0b98840127e2cac73d6ad8")
(package! demangle-mode :pin "04f545adab066708d6151f13da65aaf519f8ac4e")
(package! disaster :pin "b20f8e1ef96167a7beed5eb4fc6ef72488bd3662")
(package! opencl-mode :pin "204d5d9e0f5cb2cbe810f2933230eb08fe2c7695")

(unless (modulep! +tree-sitter)
  (package! modern-cpp-font-lock :pin "43c6b68ff58fccdf9deef11674a172e4eaa8455c"))

(when (package! glsl-mode :pin "c5f2c2e7edf8a647eda74abe2cdf73fa6f62ebd2")
  (when (modulep! :completion company)
    (package! company-glsl
      :recipe (:host github :repo "Kaali/company-glsl")
      :pin "404cd0694ab34971f9c01eb22126cd2e7d3f9dc4")))

(when (and (modulep! +lsp)
           (modulep! :tools lsp -eglot))
  (package! ccls :pin "41399b0eba03f9b80769ced71501ba702db4cd62"))
