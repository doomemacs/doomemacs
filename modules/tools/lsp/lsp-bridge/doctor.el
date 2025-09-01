;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; completion/lso-bridge/doctor.el

;; Compatibility
(dolist (module '(company corfu))
  (when (modulep! :completion module)
    (error! "This module is incompatible with :completion %s. You can disable one or the other." module)))

(when (modulep! :tools lsp)
  (warn! "This module provides independent lsp features which might be conflict with :tools lsp. If you meet something wierd using lsp-bridge, disabling :tools lsp might work."))

;; Checks
(when (not (and (modulep! :lang markdown)
                (modulep! :editor snippets)))
  (error! "This module requires :lang markdown and :editor snippets for markdown-mode and yasnippet. You can uncomment 'markdown' in :lang and 'snippets' in :editor in your init.el and run 'doom sync"))

(defun check-python-packages-installed (packages)
  "Check if required Python packages are installed.
Returns list of missing packages or nil if all are present."
  (let ((pip-exec (or (executable-find "pip3") (executable-find "pip"))))
    (unless pip-exec
      (error! "Unable to check if relative packages are installed since any pip executable isn't found."))

    (cl-loop for pkg in packages
             unless (zerop (call-process pip-exec nil nil nil "show" pkg))
             collect pkg)))

;; Necesary packages
(let ((packages '("epc" "sexpdata" "six" "setuptools"
                    "paramiko" "rapidfuzz" "watchdog" "packaging")))
      (let ((missing (check-python-packages-installed packages)))
        (when missing
          (error! "Missing packages: %s. You can install them through pip." (string-join missing ", ")))))

;; Optional packages
(let ((packages '("orjson")))
      (let ((missing (check-python-packages-installed packages)))
        (when missing
          (warn! "Missing optional packages: %s. To attain better performance, you can install them through pip." (string-join missing ", ")))))
