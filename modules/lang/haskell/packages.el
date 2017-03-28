;; -*- no-byte-compile: t; -*-
;;; lang/haskell/packages.el

(package! haskell-mode)
(package! dante)

;;
(def-bootstrap! haskell
  (pcase (doom-system-os)
    ('arch
     (let ((pkgs (cl-remove-if 'executable-find '("ghc" "happy" "alex"))))
       (unless (executable-find "cabal")
         (push "cabal-install" pkgs))
       (when pkgs
         (sudo "pacman --noconfirm -S %s" (s-join " " pkgs)))))
    ('debian) ;; TODO
    ('macos
     (unless (executable-find "ghc")
       (sh "brew install ghc"))
     (unless (executable-find "cabal")
       (sh "brew install cabal-install")))))
