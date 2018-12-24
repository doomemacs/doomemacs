;;; tools/password-store/config.el -*- lexical-binding: t; -*-

(defvar +pass-user-fields '("login" "user" "username" "email")
  "A list of fields for `+pass/ivy' to search for the username.")

(defvar +pass-url-fields '("url" "site" "location")
  "A list of fields for `+pass/ivy' to search for the username.")


;;
;; Packages

;; `password-store'
(setq password-store-password-length 12)

;; Fix hard-coded password-store location; respect PASSWORD_STORE_DIR envvar
(defun +password-store*read-entry (entry)
  "Return a string with the file content of ENTRY."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name (format "%s.gpg" entry) (password-store-dir)))
    (buffer-substring-no-properties (point-min) (point-max))))
(advice-add #'auth-source-pass--read-entry :override #'+password-store*read-entry)


;; `pass'
(after! pass
  (set-env! "PASSWORD_STORE_DIR")
  (set-evil-initial-state! 'pass-mode 'emacs)
  (set-popup-rule! "^\\*Password-Store" :side 'left :size 0.25 :quit nil)
  (define-key! pass-mode-map
    "j"    #'pass-next-entry
    "k"    #'pass-prev-entry
    "d"    #'pass-kill
    "\C-j" #'pass-next-directory
    "\C-k" #'pass-prev-directory))


;; Is built into Emacs 26+
(when (and EMACS26+ (featurep! +auth))
  (auth-source-pass-enable))
