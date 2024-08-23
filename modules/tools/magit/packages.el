;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

;; NOTE: Always bump magit and forge to HEAD~1, not HEAD, because the latest
;;   commit on their melpa branches are auto-generated and moved to HEAD every
;;   time there's a commit to its main branch.
(package! magit :pin "2da34f1317c619ec2dfb9e0d969449261ca7f31f")
(when (modulep! +forge)
  (package! forge :pin "30f181f785522f2debf60945d6b589a65bc415f6")
  (package! code-review
    :recipe (:host github
             :repo "doomelpa/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "eeffdd9e20ad133e5981f216965445bfae20292a"))
