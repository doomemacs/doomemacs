;; -*- no-byte-compile: t; -*-
;;; emacs/dired/packages.el

(package! diredfl :pin "4ca32658aebaf2335f0368a0fd08f52eb1aee960")
(package! dired-git-info :pin "9461476a28a5fec0784260f6e318237c662c3430")
(package! diff-hl :pin "992559f98a1f0367ae2b73e94f69503da73f3a83")
(package! dired-rsync :pin "fb0f161ac3cce1b224f52547f5bc7e1dcd283191")
(when (featurep! +ranger)
  (package! ranger :pin "2498519cb21dcd5791d240607a72a204d1761668"))
(when (featurep! +icons)
  (package! all-the-icons-dired :pin "a758766878b6e8b9eaaf41d68599a2df99e37f48"))
(package! fd-dired :pin "c223aee30af7dc7f52fb20045226ed9f49f4ec49")
