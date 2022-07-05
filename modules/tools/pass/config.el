;;; tools/pass/config.el -*- lexical-binding: t; -*-

(defvar +pass-user-fields '("login" "user" "username" "email")
  "A list of fields for `+pass/ivy' to search for the username.")

(defvar +pass-url-fields '("url" "site" "location")
  "A list of fields for `+pass/ivy' to search for the username.")


;;
;; Packages

;;;###package password-store
(setq password-store-password-length 12)

;; Fix hard-coded password-store location; respect PASSWORD_STORE_DIR envvar
(defadvice! +pass--respect-pass-dir-envvar-a (entry)
  "Return a string with the file content of ENTRY."
  :override #'auth-source-pass--read-entry
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name (format "%s.gpg" entry) (password-store-dir)))
    (buffer-substring-no-properties (point-min) (point-max))))


(after! pass
  (set-evil-initial-state! 'pass-mode 'normal)
  (set-popup-rule! "^\\*Password-Store" :side 'left :size 0.25 :quit nil)

  ;; FIXME This needs to be upstreamed to evil-collection.
  (add-to-list  'evil-collection-pass-command-to-label '(pass-update-buffer . "gr"))

  (map! :map pass-mode-map
    :n "j"    #'pass-next-entry
    :n "k"    #'pass-prev-entry
    :n "d"    #'pass-kill
    :n "C-j" #'pass-next-directory
    :n "C-k" #'pass-prev-directory))


;; Is built into Emacs 26+
(when (featurep! +auth)
  (auth-source-pass-enable))
