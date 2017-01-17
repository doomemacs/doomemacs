;;; lang/js/packages.el

(package! js2-mode :mode "\\.js$" :interpreter "node")

(package! jsx-mode :mode "\\.jsx$")

(package! coffee-mode :mode "\\.coffee$")

(package! tern :after js2-mode)

(package! company-tern :after js2-mode)

(package! js2-refactor :after js2-mode)
