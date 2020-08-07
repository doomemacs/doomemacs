;; -*- no-byte-compile: t; -*-
;;; editor/multiple-cursors/packages.el

(cond
 ((featurep! :editor evil)
  ;; REVIEW Broken in 8abf2c1f4f0ade64cbb06c8f47055f04ab83e8d6 (latest commit at
  ;;        time of writing). Revisit later.
  (package! iedit :pin "77eb0a1e2e44b453e4ebf4c38409affa353f5139")
  (package! evil-multiedit :pin "9f271e0e6048297692f80ed6c5ae8994ac523abc")
  (package! evil-mc :pin "4d4c0172e4c7f80acc1d0e73d5fb3e536929b262"))

 ((package! multiple-cursors :pin "b880554d04b8f61165afba7d4de19ac9e39bb7ab")))
