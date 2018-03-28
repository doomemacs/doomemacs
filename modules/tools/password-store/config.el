;;; tools/password-store/config.el -*- lexical-binding: t; -*-

(defvar +pass-user-fields '("login" "user" "username" "email")
  "A list of fields for `+pass/ivy' to search for the username.")

(defvar +pass-url-fields '("url" "site" "location")
  "A list of fields for `+pass/ivy' to search for the username.")


;;
;; Plugins
;;

(def-package! password-store
  :defer t
  :config
  (setq password-store-password-length 12))


(def-package! pass
  :commands pass
  :config
  (set! :evil-state 'pass-mode 'emacs)
  (set! :popup "^\\*Password-Store" '((side . left) (size . 0.25)) '((quit)))
  (map! :map pass-mode-map
        "j"   #'pass-next-entry
        "k"   #'pass-prev-entry
        "d"   #'pass-kill
        "C-j" #'pass-next-directory
        "C-k" #'pass-next-directory))


(def-package! helm-pass
  :when (featurep! :completion helm)
  :commands helm-pass)


;; Is built into Emacs 26+
(def-package! auth-source-pass
  :when (featurep! +auth)
  :config (auth-source-pass-enable))
