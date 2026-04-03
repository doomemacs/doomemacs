;; -*- no-byte-compile: t; -*-
;;; lang/erlang/packages.el

(package! erlang :pin "92c53535cd6ba9b73a24f631e7ff200fdd381570")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! erlang-ts :pin "7680c827adf17ad8b17ac94d56602c2b8645440a"))
