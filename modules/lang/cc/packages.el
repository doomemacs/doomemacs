;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! cmake-mode)
(package! cuda-mode)
(package! demangle-mode)
(package! disaster)
(package! modern-cpp-font-lock)
(package! opencl-mode)

(when (package! glsl-mode)
  (when (featurep! :completion company)
    (package! company-glsl :recipe (:fetcher github :repo "Kaali/company-glsl"))))

(when (featurep! +irony)
  (package! irony)
  (package! irony-eldoc)
  (when (featurep! :feature syntax-checker)
    (package! flycheck-irony))
  (when (featurep! :completion company)
    (package! company-irony)
    (package! company-irony-c-headers)))

(when (featurep! +rtags)
  (package! rtags)
  (when (featurep! :completion ivy)
    (package! ivy-rtags))
  (when (featurep! :completion helm)
    (package! helm-rtags)))
