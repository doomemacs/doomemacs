;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

(package! coffee-mode)
(package! js2-mode)
(package! js2-refactor)
(package! jsx-mode)
(package! nodejs-repl)
(package! tern)
(package! web-beautify)
(package! skewer-mode)

(when (featurep! :completion company)
  (package! company-tern))

(when (featurep! :feature jump)
  (package! xref-js2))

;;
(def-bootstrap! javascript
  (unless (cl-every 'executable-find '("node" "npm" "tern" "js-beautify"))
    (pcase (doom-system-os)
      ('arch
       (let (progs)
         (unless (executable-find "node") (push "nodejs" progs))
         (unless (executable-find "npm")  (push "npm" progs))
         (when progs
           (sudo "pacman --noconfirm -S %s" progs))))
      ('debian) ;; TODO
      ('macos
       (unless (executable-find "node")
         (sh "brew install node"))))
    (unless (executable-find "node")
      (error "Failed to install NodeJS"))
    (unless (executable-find "tern")
      (sh "npm -g install tern"))
    (unless (executable-find "js-beautify")
      (sh "npm -g install js-beautify"))
    t))
