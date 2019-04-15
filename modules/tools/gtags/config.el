;;; tools/gtags/config.el -*- lexical-binding: t; -*-

(defvar +ggtags-default-enabled-mode
   (list 'c-mode
         'c++-mode
         'java-mode)
   "A list of mode which ggtags enabled")

(def-package! ggtags
  :config
  (add-hook 'c-mode-common-hook
            (λ!
             (cl-loop for i in +ggtags-default-enabled-mode
                      do (when (derived-mode-p i)
                        (ggtags-mode 1)))))
  (set-lookup-handlers! +ggtags-default-enabled-mode
    :definition #'ggtags-find-definition
    :references #'ggtags-find-reference)

  (map! :leader
        (:prefix ("c". "code")
          :desc "Find Symbol" "s" #'ggtags-find-other-symbol)))
