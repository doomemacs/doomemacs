;; -*- no-byte-compile: t; -*-
;;; tools/docker/packages.el

(package! docker :pin "44f0bbec9b3deb027d17f4c10d8ec4106ed89dfb")
(unless EMACS29+
  (package! docker-tramp :pin "930d7b46c180d8a13240a028c1b40af84f2a3219"))
(package! dockerfile-mode :pin "b63a3d12b7dea0cb9efc7f78d7ad5672ceab2a3f")
