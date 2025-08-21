;; -*- no-byte-compile: t; -*-
;;; tools/llm/packages.el

(package! gptel :pin "af821efe63006cb571a071a8cda03fd2a57c18f5")

(package! gptel-quick
  :recipe (:host github :repo "karthink/gptel-quick")
  :pin "495b5e0b5348dbced1448bd12cbf8847e30b5175")

(when (modulep! :tools magit)
  (package! gptel-magit :pin "f27c01821b67ed99ddf705c2b995f78b71394d8b"))

(when (and (modulep! +mcp)
           (>= emacs-major-version 30))
  (package! mcp :pin "4708c5849ce4ddb632016eca662a7405bfa642d4"))
