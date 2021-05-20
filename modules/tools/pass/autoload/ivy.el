;;; tools/pass/autoload/ivy.el -*- lexical-binding: t; -*-
;;;###if (featurep! :completion ivy)

;;;###autoload
(defun +pass/ivy (arg)
  "TODO"
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
     ("e" +pass/edit-entry "edit entry")
     ("u" +pass/copy-user "copy username")
     ("b" +pass/copy-url "open url in browser")
     ("f" +pass/copy-field "get field"))))
