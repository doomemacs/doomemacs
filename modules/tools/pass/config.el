;;; tools/pass/config.el -*- lexical-binding: t; -*-

(defvar +pass-user-fields '("login" "user" "username" "email")
  "A list of fields for `+pass/ivy' to search for the username.")

(defvar +pass-url-fields '("url" "site" "location")
  "A list of fields for `+pass/ivy' to search for the username.")


;;
;; Packages

;;;###package password-store
(setq password-store-password-length 12)

(after! evil-collection-pass
  ;; FIXME This needs to be upstreamed to evil-collection.
  (add-to-list 'evil-collection-pass-command-to-label '(pass-update-buffer . "gr")))

(after! pass
  (set-evil-initial-state! 'pass-mode 'normal)
  (set-popup-rule! "^\\*Password-Store" :side 'left :size 0.25 :quit nil)
  (map! :map pass-mode-map
        :n "j"   #'pass-next-entry
        :n "k"   #'pass-prev-entry
        :n "d"   #'pass-kill
        :n "C-j" #'pass-next-directory
        :n "C-k" #'pass-prev-directory))


;; Is built into Emacs 26+
(when (modulep! +auth)
  (auth-source-pass-enable))
