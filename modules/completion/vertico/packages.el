;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "b6b8420d2943e42b88e2a143da29edf76bc223b5")

(package! orderless :pin "e6784026717a8a6a7dcd0bf31fd3414f148c542e")

(package! consult :pin "052399ed05372464b8ed6261b3196de143a8a834")
(package! consult-dir :pin "ed8f0874d26f10f5c5b181ab9f2cf4107df8a0eb")
(when (modulep! :checkers syntax)
  (package! consult-flycheck :pin "c371996c571b7139ef4d9a8db142bf37a7ee826b"))
(package! embark :pin "3ffb27a833d326ccf7e375bb9d94ebf4dc37fa77")
(package! embark-consult :pin "3ffb27a833d326ccf7e375bb9d94ebf4dc37fa77")

(package! marginalia :pin "2633b2dee22261531f960e49106771e679102a98")

(package! wgrep :pin "edf768732a56840db6879706b64c5773c316d619")

(when (modulep! +icons)
  (package! all-the-icons-completion :pin "b08f053cee444546ab44a05fd541f59e8bc8983b"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "7da6d648ff4202a48eb6647ee7dce8d65de48779"))
