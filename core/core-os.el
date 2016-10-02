;;; core-os.el

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (eq system-type 'windows-nt))

(setq
 ;; Use a shared clipboard
 x-select-enable-clipboard t
 select-enable-clipboard t
 ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
 x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Stubs, these should be defined in all OS modules
(noop! doom-open-with (&optional app-name path))
(noop! os-open-in-browser)
(noop! os-open-in-default-program)
(noop! os-reveal)
(noop! os-reveal-project)
(noop! os-switch-to-term)
(noop! os-switch-to-term-and-cd)

(cond (IS-MAC     (require 'core-os-osx))
      (IS-LINUX   (require 'core-os-linux))
      (IS-WINDOWS (require 'core-os-win32)))

(provide 'core-os)
;;; core-os.el ends here
