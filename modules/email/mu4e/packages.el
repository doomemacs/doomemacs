;; -*- no-byte-compile: t; -*-
;;; email/mu4e/packages.el

(when (modulep! +org)
  (package! org-msg :pin "60e22e446325a9b3387396459d98be7c1c52579d"))

(package! mu4e-alert :pin "b34d0ea7b75709cc25d842a783cebea855dc9f7d")
