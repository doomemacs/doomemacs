;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "bb7b7a402025e26aabc0aee6e97c2ee852a806a9")
  (when (featurep! +forge)
    (package! forge :pin "8264234db61b8a7f427b972aaef6235f9f6a13fa"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "60152d5c4e4b73e72e15f23ca16e8cc7734906bc")
  (package! github-review :pin "4d91dd6c56be1ae2b93b6b9e50c73f657cc461b6"))
