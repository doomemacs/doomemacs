;;; tools/password-store/config.el -*- lexical-binding: t; -*-

(defvar +pass-user-fields '("login" "user" "username" "email")
  "A list of fields for `+pass/ivy' to search for the username.")

(defvar +pass-url-fields '("url" "site" "location")
  "A list of fields for `+pass/ivy' to search for the username.")


;;
;; Plugins
;;

;; `password-store'
(setq password-store-password-length 12)


;; `pass'
(def-package! pass
  :defer t
  :config
  (set! :evil-state 'pass-mode 'emacs)
  (set! :popup "^\\*Password-Store"
    '((side . left) (size . 0.25))
    '((quit)))
  (map! :map pass-mode-map
        "j"   #'pass-next-entry
        "k"   #'pass-prev-entry
        "d"   #'pass-kill
        "C-j" #'pass-next-directory
        "C-k" #'pass-next-directory))


;; Is built into Emacs 26+
(when (and EMACS26+ (featurep! +auth))
  (auth-source-pass-enable))
