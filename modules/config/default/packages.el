;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! avy :pin "ba5f035be33693d1a136a5cbeedb24327f551a92")
(package! drag-stuff :pin "6d06d846cd37c052d79acd0f372c13006aa7e7c8")
(package! link-hint :pin "676dac6621e321b33a8d396fa27dd0ea619d21e3")

(unless (featurep! :editor evil)
  (package! expand-region :pin "7e5bbe2763c12bae3e77fe0c49bcad05ff91dbfe"))
