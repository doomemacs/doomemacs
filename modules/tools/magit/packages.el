;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

;; NOTE: Always bump magit and forge to HEAD~1, not HEAD, because the latest
;;   commit on their melpa branches are auto-generated and moved to HEAD every
;;   time there's a commit to its main branch.
(package! magit :pin "0aa26864e3fc4e6949711a4821caf6819e7ab171")
(when (modulep! +forge)
  (package! forge :pin "35cc600d62a01d50699a529b0caa7d40e642d62a")
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "eeffdd9e20ad133e5981f216965445bfae20292a"))
