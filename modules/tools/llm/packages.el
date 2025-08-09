;; -*- no-byte-compile: t; -*-
;;; tools/llm/packages.el

(package! gptel :pin "9c9af2cdfbec0f55a14591f0b1ffab0ebcd25e5e")

(package! gptel-quick
  :recipe (:host github :repo "karthink/gptel-quick")
  :pin "495b5e0b5348dbced1448bd12cbf8847e30b5175")

(when (modulep! :tools magit)
  (package! gptel-magit :pin "f27c01821b67ed99ddf705c2b995f78b71394d8b"))
