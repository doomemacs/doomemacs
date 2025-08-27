;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico :pin "718f8584d09082451c29066c3c370bdaff02fcab")

(package! orderless :pin "31812d9252c6cfa7eae8fa04cd40c8b2081e9936")

(package! consult :pin "c8bbb3f1e2fbbdcca773498e2db168c0929c3434")
(package! consult-dir :pin "4532b8d215d16b0159691ce4dee693e72d71e0ff")
(when (modulep! :checkers syntax -flymake)
  (package! consult-flycheck :pin "398a85b5df71a4a57d74d0a7e1bdf25057cf5bdf"))
(package! embark :pin "1371a1e33e3a3d96557beb28dccf1fa762f6ae22")
(package! embark-consult :pin "1371a1e33e3a3d96557beb28dccf1fa762f6ae22")

(package! marginalia :pin "30e6813c8142ef8cb45e6f9bdd23ead1c80b9b2e")

(package! wgrep :pin "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f")

(when (modulep! +icons)
  (package! nerd-icons-completion :pin "5625ef374d428e69f96c2f95858c8bc4db6f7679"))

(when (modulep! +childframe)
  (package! vertico-posframe
    :recipe (:host github :repo "tumashu/vertico-posframe")
    :pin "c5a8b5f72a582e88a2a696a3bbc2df7af28bd229"))

(when (modulep! :editor snippets)
  (package! consult-yasnippet :pin "a3482dfbdcbe487ba5ff934a1bb6047066ff2194"))
