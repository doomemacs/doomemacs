;;; tools/gtags/config.el -*- lexical-binding: t; -*-

(defgroup gtags nil
  "Gtags related settings"
  :group 'programming)

(defcustom +gtags-enable-proj-paths '() "specify the path that gtags enable"
  :group 'gtags
  :type '(repeat string))

(defmacro +gtags-enable-for-mode! (mode)
  `(add-hook! ',(intern-soft (format "%s-hook" mode))
     (when (or (not (featurep 'lsp-mode)) (member (projectile-project-root) +gtags-enable-proj-paths))
       (ggtags-mode 1)
       (set-lookup-handlers! ',mode
         :definition #'ggtags-find-definition
         :references #'ggtags-find-reference)
       (message "hook done!"))))

(advice-add! '(lsp) :around
             (lambda (orig-fn &rest args)
               (if (not (member (projectile-project-root) +gtags-enable-proj-paths))
                   (apply orig-fn args)
                 t)))


(def-package! ggtags
  :config
  (map! :leader
        (:prefix ("c". "code")
          :desc "Find Symbol" "s" #'ggtags-find-other-symbol)))
