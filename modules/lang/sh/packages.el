;; -*- no-byte-compile: t; -*-
;;; lang/sh/packages.el

(when (featurep! :completion company)
  (package! company-shell))

;;
(def-bootstrap! sh
  (when-let (progs (cl-remove-if 'executable-find '("zshdb" "bashdb" "shellcheck")))
    (let ((prog-str (s-join " " progs)))
      (pcase (doom-system-os)
        ('arch
         (sudo "pacman --noconfirm -S %s" prog-str))
        ('debian
         (sudo "apt-get install -y %s" prog-str))
        ('macos
         (sh "brew install %s" prog-str))))
    t))
