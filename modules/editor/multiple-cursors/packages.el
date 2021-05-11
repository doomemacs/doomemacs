;; -*- no-byte-compile: t; -*-
;;; editor/multiple-cursors/packages.el

(cond
 ((featurep! :editor evil)
  ;; REVIEW Broken in 8abf2c1f4f0ade64cbb06c8f47055f04ab83e8d6 (latest commit at
  ;;        time of writing). Revisit later.
  (package! iedit :pin "77eb0a1e2e44b453e4ebf4c38409affa353f5139")
  (package! evil-multiedit :pin "9f271e0e6048297692f80ed6c5ae8994ac523abc")
  (package! evil-mc :pin "f04fb17f35f2722f2ac93c862b4450bb8e5b739a"))

 ((package! multiple-cursors :pin "7b13b03c995e13ad86e499d40ec49c4dc281f889")))
