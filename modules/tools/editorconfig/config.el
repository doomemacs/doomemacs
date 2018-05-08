;;; tools/editorconfig/config.el -*- lexical-binding: t; -*-

;; Handles whitespace (tabs/spaces) settings externally. This way projects can
;; specify their own formatting rules.
(def-package! editorconfig
  :hook (doom-init . editorconfig-mode)
  :config
  ;; Register missing indent variables
  (setq editorconfig-indentation-alist
        (append '((mips-mode mips-tab-width)
                  (haxor-mode haxor-tab-width)
                  (nasm-mode nasm-basic-offset))
                editorconfig-indentation-alist))

  ;; editorconfig cannot procure the correct settings for extension-less files.
  ;; Executable scripts with a shebang line, for example. So why not use Emacs'
  ;; major mode to drop editorconfig a hint? This is accomplished by temporarily
  ;; appending an extension to `buffer-file-name' when we talk to editorconfig.
  (defvar doom-editorconfig-mode-alist
    '((sh-mode     . "sh")
      (python-mode . "py")
      (ruby-mode   . "rb")
      (perl-mode   . "pl")
      (php-mode    . "php"))
    "An alist mapping major modes to extensions. Used by
`doom*editorconfig-smart-detection' to give editorconfig filetype hints.")

  (defun doom*editorconfig-smart-detection (orig-fn &rest args)
    "Retrieve the properties for the current file. If it doesn't have an
extension, try to guess one."
    (let ((buffer-file-name
           (if (and (not (bound-and-true-p org-src-mode))
                    (file-name-extension buffer-file-name))
               buffer-file-name
             (format "%s%s" buffer-file-name
                     (if-let* ((ext (cdr (assq major-mode doom-editorconfig-mode-alist))))
                         (concat "." ext)
                       "")))))
      (apply orig-fn args)))
  (advice-add #'editorconfig-call-editorconfig-exec :around #'doom*editorconfig-smart-detection)

  ;; Editorconfig makes indentation too rigid in Lisp modes, so tell
  ;; editorconfig to ignore indentation there. I prefer dynamic indentation
  ;; support built into Emacs.
  (dolist (mode '(emacs-lisp-mode lisp-mode))
    (map-delete editorconfig-indentation-alist mode)))


(def-package! editorconfig-conf-mode
  :mode "\\.?editorconfig$")

