;; -*- no-byte-compile: t; -*-
;;; os/tty/packages.el

(if (featurep! +osc)
    (package! clipetty
      :recipe (:host github :repo "spudlyo/clipetty")
      :pin "01b39044b9b65fa4ea7d3166f8b1ffab6f740362")
  ;; Despite its name, this works for macOS as well.
  (package! xclip :pin "ef2ad92f3157d40b6d12c7e8485d633eaae7fc45"))

(when (featurep! :editor evil)
  (package! evil-terminal-cursor-changer
    ;; HACK Fix #3845: original package is abandoned. This fork fixes a breaking
    ;;      bug that crashes the first terminal frame opened from the daemon.
    ;; Fix: #6551: kisaragi-hiu fork does not work with emacs inside tmux. 
    ;; Original package was recently updated.
    :recipe (:host github :repo "7696122/evil-terminal-cursor-changer")
    :pin "12ea9c0438c67e560b3866dc78b5c7d1d93f8cc5"))
