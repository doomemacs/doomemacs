;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! cmake-mode)
(package! cuda-mode)
(package! demangle-mode)
(package! disaster)
(package! glsl-mode)
(package! irony)
(package! irony-eldoc)
(package! modern-cpp-font-lock)
(package! opencl-mode)

(when (featurep! :feature syntax-checker)
  (package! flycheck-irony))

(when (featurep! :completion company)
  (package! company-glsl :recipe (:fetcher github :repo "Kaali/company-glsl"))
  (package! company-irony)
  (package! company-irony-c-headers))

(package! rtags)
(when (featurep! :completion ivy)
  (package! ivy-rtags))
(when (featurep! :completion helm)
  (package! helm-rtags))
