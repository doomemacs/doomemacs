;;; core-os.el

(defun doom-open-with (&optional app-name path)
  (error "`doom-open-with' not implemented"))

(cond (IS-MAC     (require 'core-os-osx))
      (IS-LINUX   (require 'core-os-osx))
      (IS-WINDOWS (require 'core-os-osx)))

(provide 'core-os)
;;; core-os.el ends here
