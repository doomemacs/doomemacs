;;; core-os.el

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (eq system-type 'windows-nt))

;; Stubs, these should be defined in all OS modules
(noop! doom-open-with (&optional app-name path))
(noop! os-switch-to-term)
(noop! os-switch-to-term-and-cd)
(noop! os-open-in-default-program)
(noop! os-reveal)
(noop! os-reveal-project)
(noop! os-open-in-browser)
(noop! os-upload)
(noop! os-upload-folder)

(cond (IS-MAC     (require 'core-os-osx))
      (IS-LINUX   (require 'core-os-linux))
      (IS-WINDOWS (require 'core-os-win32)))

(provide 'core-os)
;;; core-os.el ends here
