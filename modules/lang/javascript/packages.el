;; -*- no-byte-compile: t; -*-
;; lang/javascript/packages.el

(package! js2-mode)
(package! jsx-mode)
(package! coffee-mode)
(package! tern)
(package! company-tern)
(package! js2-refactor)

(bootstrap! javascript
 "Sets up NodeJS, the TrepanJS debugger."

 :if-arch
 (unless (and (executable-find "node")
              (executable-find "npm"))
   (sudo "pacman --needed --noconfirm -S nodejs npm"))

 ;; :if-deb
 ;; TODO

 :if-macos
 (unless (executable-find "node")
   (sh "brew install node"))

 :after
 (unless (executable-find "tern")
   (sh "npm -g install trepanjs tern")))
