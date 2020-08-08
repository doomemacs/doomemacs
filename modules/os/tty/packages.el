;; -*- no-byte-compile: t; -*-
;;; os/tty/packages.el

(if (featurep! +osc)
    (package! clipetty
      :recipe (:host github :repo "spudlyo/clipetty")
      :pin "01b39044b9b65fa4ea7d3166f8b1ffab6f740362")
  ;; Despite its name, this works for macOS as well.
  (package! xclip :pin "2951c6b62b29780c7a35c64601a59999b83aa145"))

(when (featurep! :editor evil)
  (package! evil-terminal-cursor-changer :pin "b49ca4393d2f3cc6014174950059b36a5cb22949"))
