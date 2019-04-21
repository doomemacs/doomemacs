;;; tools/gtags/config.el -*- lexical-binding: t; -*-

(defun +gtags-ask-enabled-for-project ()
  "check `+gtags-check-project-enabled-flag' "
  (when (not (boundp '+gtags-enable-for-this-project))
    (let ((default-directory (projectile-project-root)))
      (add-dir-local-variable nil '+gtags-enable-for-this-project
                              (yes-or-no-p "Do you want enable gtags for the project?"))
      (save-buffer)
      (kill-buffer)
      (hack-dir-local-variables-non-file-buffer))))

(defmacro +gtags-enable-for-mode! (mode)
  `(add-hook! ',(intern-soft (format "%s-local-vars-hook" mode))
     (progn
       (when +gtags-enable-for-this-project
         (ggtags-mode 1)
         (set-lookup-handlers! mode
           :definition #'ggtags-find-definition
           :references #'ggtags-find-reference)
         (when (featurep 'lsp-mode)
           (lsp-mode -1)
           (lsp-shutdown-workspace)))

       (message "hook done"))))

(def-package! ggtags
  :init
  (add-hook 'projectile-find-file-hook #'+gtags-ask-enabled-for-project)
  :config
  (map! :leader
        (:prefix ("c". "code")
          :desc "Find Symbol" "s" #'ggtags-find-other-symbol)))
