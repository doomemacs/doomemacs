;; -*- no-byte-compile: t; -*-
;;; tools/llm/packages.el

(package! gptel :pin "af821efe63006cb571a071a8cda03fd2a57c18f5")

(package! gptel-quick
  :recipe (:host github :repo "karthink/gptel-quick")
  :pin "495b5e0b5348dbced1448bd12cbf8847e30b5175")

(when (modulep! :tools magit)
  (package! gptel-magit
    ;; REVIEW: Revert to upstream if ragnard/gptel-magit#7 is merged.
    :recipe (:host github
             :repo "ArthurHeymans/gptel-magit")
    :pin "4a40c3fc201d60d2f0589c2e1a6693fd94bb4c98"))
