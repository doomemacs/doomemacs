;;; tools/pass/autoload/ivy.el -*- lexical-binding: t; -*-
;;;###if (featurep! :completion ivy)

;;;###autoload
(defun +pass/ivy (arg)
  "Complete and act on password store entries."
  (interactive "P")
  (ivy-read "Pass: " (password-store-list)
            :action (if arg
                        #'password-store-url
                      #'password-store-copy)
            :caller '+pass/ivy))

(after! ivy
  (ivy-add-actions
   '+pass/ivy
   '(("o" password-store-copy "copy password")
     ("e" password-store-edit "edit entry")
     ("u" +pass/copy-user "copy username")
     ("b" password-store-url "open url in browser")
     ("f" password-store-copy-field "get field"))))
