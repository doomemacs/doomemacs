;;; tools/editorconfig/config.el -*- lexical-binding: t; -*-

;; editorconfig cannot procure the correct settings for extension-less files.
;; Executable scripts with a shebang line, for example. So why not use Emacs'
;; major mode to drop editorconfig a hint? This is accomplished by temporarily
;; appending an extension to `buffer-file-name' when we talk to editorconfig.
(defvar +editorconfig-mode-alist
  '((emacs-lisp-mode . "el")
    (js2-mode        . "js")
    (perl-mode       . "pl")
    (php-mode        . "php")
    (python-mode     . "py")
    (ruby-mode       . "rb")
    (sh-mode         . "sh"))
  "An alist mapping major modes to extensions. Used by
`doom--editorconfig-smart-detection-a' to give editorconfig filetype hints.")


;; Handles whitespace (tabs/spaces) settings externally. This way projects can
;; specify their own formatting rules.
(use-package! editorconfig
  :hook (doom-first-buffer . editorconfig-mode)
  :config
  (when (require 'ws-butler nil t)
    (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))

  ;; Fix #5057 archives don't need editorconfig settings, and they may otherwise
  ;; interfere with the process of opening them (office formats are zipped XML
  ;; formats).
  (add-to-list 'editorconfig-exclude-regexps
               "\\.\\(zip\\|\\(doc\\|xls\\|ppt\\)x\\)\\'")

  (defadvice! +editorconfig--smart-detection-a (fn)
    "Retrieve the properties for the current file. If it doesn't have an
extension, try to guess one."
    :around #'editorconfig-call-editorconfig-exec
    (let ((buffer-file-name
           (if (and (not (bound-and-true-p org-src-mode))
                    (file-name-extension buffer-file-name))
               buffer-file-name
             (format "%s%s" (buffer-file-name (buffer-base-buffer))
                     (if-let (ext (alist-get major-mode +editorconfig-mode-alist))
                         (concat "." ext)
                       "")))))
      (funcall fn)))

  (add-hook! 'editorconfig-after-apply-functions
    (defun +editorconfig-disable-indent-detection-h (props)
      "Inhibit `dtrt-indent' if an explicit indent_style and indent_size is
specified by editorconfig."
      (when (or (gethash 'indent_style props)
                (gethash 'indent_size props))
        (setq doom-inhibit-indent-detection 'editorconfig)))))
