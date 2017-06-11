;;; tools/password-store/config.el -*- lexical-binding: t; -*-

(def-package! password-store
  :config
  (setq password-store-password-length 12))

(def-package! pass
  :commands (pass)
  :bind (:map pass-mode-map
         ("j" . pass-next-entry)
         ("k" . pass-prev-entry)
         ("d" . pass-kill)
         ("C-j" . pass-next-directory)
         ("C-k" . pass-next-directory)))

(def-package! auth-password-store
  :demand t
  :config
  (auth-pass-enable))
