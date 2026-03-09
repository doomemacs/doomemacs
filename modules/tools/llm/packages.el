;; -*- no-byte-compile: t; -*-
;;; tools/llm/packages.el

(package! gptel
  :recipe (:nonrecursive t)
  :pin "ecb6276a94267f1373dd258ec6e9334ee40f4e75")

(package! gptel-quick
  :recipe (:host github :repo "karthink/gptel-quick")
  :pin "018ff2be8f860a1e8fe3966eec418ad635620c38")

(when (modulep! :tools magit)
  (package! gptel-magit
    ;; REVIEW: Revert to upstream if ragnard/gptel-magit#7 is merged.
    :recipe (:host github
             :repo "ArthurHeymans/gptel-magit")
    :pin "4a40c3fc201d60d2f0589c2e1a6693fd94bb4c98"))

(when (modulep! :lang org)
  (package! ob-gptel
    :recipe (:host github :repo "jwiegley/ob-gptel")
    :pin "60e704a390d767a7d06c8d3845ba8786b75f7da3"))
