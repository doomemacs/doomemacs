;;; tools/editorconfig/config.el -*- lexical-binding: t; -*-

;; editorconfig cannot procure the correct settings for extension-less files.
;; Executable scripts with a shebang line, for example. So why not use Emacs'
;; major mode to drop editorconfig a hint? This is accomplished by temporarily
;; appending an extension to `buffer-file-name' when we talk to editorconfig.
(defvar +editorconfig-mode-alist
  '((sh-mode       . "sh")
    (python-mode   . "py")
    (ruby-mode     . "rb")
    (enh-ruby-mode . "rb")
    (perl-mode     . "pl")
    (php-mode      . "php"))
  "An alist mapping major modes to extensions. Used by
`doom*editorconfig-smart-detection' to give editorconfig filetype hints.")


;; Handles whitespace (tabs/spaces) settings externally. This way projects can
;; specify their own formatting rules.
(def-package! editorconfig
  :after-call (doom-switch-buffer-hook after-find-file)
  :config
  (defun doom*editorconfig-smart-detection (orig-fn)
    "Retrieve the properties for the current file. If it doesn't have an
extension, try to guess one."
    (let ((buffer-file-name
           (if (and (not (bound-and-true-p org-src-mode))
                    (file-name-extension buffer-file-name))
               buffer-file-name
             (format "%s%s" buffer-file-name
                     (if-let* ((ext (cdr (assq major-mode +editorconfig-mode-alist))))
                         (concat "." ext)
                       "")))))
      (funcall orig-fn)))
  (advice-add #'editorconfig-call-editorconfig-exec :around #'doom*editorconfig-smart-detection)

  (defun +editorconfig|disable-ws-butler-maybe (props)
    "Disable `ws-butler-mode' if trim_trailing_whitespace is true."
    (when (and (equal (gethash 'trim_trailing_whitespace props) "true")
               (bound-and-true-p ws-butler-mode))
      (ws-butler-mode -1)))
  (add-hook 'editorconfig-after-apply-functions #'+editorconfig|disable-ws-butler-maybe)

  (defun +editorconfig|disable-indent-detection (props)
    "Inhibit `dtrt-indent' if an explicit indent_style and indent_size is
specified by editorconfig."
    (when (or (gethash 'indent_style props)
              (gethash 'indent_size props))
      (setq doom-inhibit-indent-detection 'editorconfig)))
  (add-hook 'editorconfig-after-apply-functions #'+editorconfig|disable-indent-detection)

  ;; Editorconfig makes indentation too rigid in Lisp modes, so tell
  ;; editorconfig to ignore indentation there. The dynamic indentation support
  ;; built into Emacs is superior.
  (setq editorconfig-lisp-use-default-indent t)

  ;;
  (editorconfig-mode +1))
