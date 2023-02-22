;; -*- no-byte-compile: t; -*-
;;; lang/latex/packages.el

(package! auctex
  :recipe (:files ("*.el" "*.info" "dir"
                   "doc" "etc" "images" "latex" "style"))
  :pin "3929d5408b1e0d68cadeef7536a26ce29b1d36ea")
(package! adaptive-wrap :pin "0d5b4a07de76d87dd64333a566a8a0a845f2b9f0")
(package! latex-preview-pane :pin "5297668a89996b50b2b62f99cba01cc544dbed2e")
(when (modulep! :editor evil +everywhere)
  (package! evil-tex :pin "3e0a26b91a1a56b0f35cbd450d01431057551750"))

;; Optional module features.

(when (modulep! +latexmk)
  (package! auctex-latexmk :pin "b00a95e6b34c94987fda5a57c20cfe2f064b1c7a"))

(when (modulep! +cdlatex)
  (package! cdlatex :pin "ac024ce29318cab812a743ad132a531c855c27a5"))

;; Features according to other user selected options.

(when (modulep! :completion company)
  (package! company-auctex :pin "9400a2ec7459dde8cbf1a5d50dfee4e300ed7e18")
  (package! company-reftex :pin "42eb98c6504e65989635d95ab81b65b9d5798e76")
  (package! company-math :pin "3eb006874e309ff4076d947fcbd61bb6806aa508"))
