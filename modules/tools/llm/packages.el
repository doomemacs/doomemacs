;; -*- no-byte-compile: t; -*-
;;; tools/llm/packages.el

(package! gptel :pin "73144b7345693b046174364edb68e1a5f5a3c7ed")

(package! gptel-quick
  :recipe (:host github :repo "karthink/gptel-quick")
  :pin "018ff2be8f860a1e8fe3966eec418ad635620c38")

(when (modulep! :tools magit)
  (package! gptel-magit
    ;; REVIEW: Revert to upstream if ragnard/gptel-magit#7 is merged.
    :recipe (:host github
             :repo "ArthurHeymans/gptel-magit")
    :pin "4a40c3fc201d60d2f0589c2e1a6693fd94bb4c98"))
