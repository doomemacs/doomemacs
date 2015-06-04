(use-package company
  :diminish (company-mode . "=")
  :init
  (progn
    (defvar company-dictionary-alist '())
    (defvar company-dictionary-major-minor-modes '())
    (defvar company-dictionary-dir (concat BASE-DIR "dict/")))

    (after "abbrev" (diminish 'abbrev-mode "A"))
  :config
  (progn
    (global-company-mode +1)
    (setq company-idle-delay nil
          company-minimum-prefix-length 1
          company-show-numbers nil
          company-tooltip-limit 20
          company-dabbrev-downcase nil
          company-dabbrev-ignore-case nil
          company-tooltip-align-annotations t
          company-require-match 'never
          company-global-modes
          '(not eshell-mode comint-mode org-mode erc-mode message-mode help-mode))

    ;; sort candidates by
    (setq-default company-frontends
                  '(company-pseudo-tooltip-unless-just-one-frontend
                    company-echo-metadata-frontend
                    company-preview-if-just-one-frontend))

    (progn ; Rewrite evil-complete to use company-dabbrev
      (setq company-dabbrev-code-other-buffers t)
      (setq company-dabbrev-code-buffers nil)
      (setq evil-complete-next-func
            (lambda(arg)
              (call-interactively 'company-dabbrev)
              (if (eq company-candidates-length 1)
                  (company-complete))))
      (setq evil-complete-previous-func
            (lambda (arg)
              (let ((company-selection-wrap-around t))
                (call-interactively 'company-dabbrev)
                (if (eq company-candidates-length 1)
                    (company-complete)
                  (call-interactively 'company-select-previous))))))

    (progn ; backends
      (setq-default company-backends (append '(company-dictionary company-keywords) company-backends))
      (add-to-list 'company-transformers 'company-sort-by-occurrence)
      (after "yasnippet"
        (setq-default company-backends (append '(company-capf company-yasnippet) company-backends)))

      (defmacro narf/add-company-backend (hook backends)
        "Register a company backend for a mode."
        (let ((def-name (intern (format "narf--init-%s" hook))))
          `(progn
             (defun ,def-name ()
               (set (make-local-variable 'company-backends)
                    (append '((,@backends company-semantic)) company-backends)))
             (add-hook ',(intern (format "%s-hook" hook)) ',def-name))))
      (narf/add-company-backend nxml-mode       (company-nxml company-yasnippet))
      (narf/add-company-backend emacs-lisp-mode (company-elisp company-yasnippet))


      ;; Simulates ac-source-dictionary (without global dictionary)
      (defun company-dictionary (command &optional arg &rest ignored)
        "`company-mode' back-end for user-provided dictionaries."
        (interactive (list 'interactive))
        (unless company-dictionary-alist
          ;; initialize dictionary
          (dolist (file (f-files company-dictionary-dir))
            (add-to-list 'company-dictionary-alist `(,(intern (f-base file)) ,@(s-split "\n" (f-read file) t)))))
        (let ((dict (let ((minor-modes (-filter (lambda (mode) (when (boundp mode) (symbol-value mode)))
                                                company-dictionary-major-minor-modes))
                          (dicts (cdr (assq major-mode company-dictionary-alist))))
                      (dolist (mode minor-modes)
                        (setq dicts (append dicts (cdr (assq mode company-dictionary-alist)))))
                      dicts)))
          (cl-case command
            (interactive (company-begin-backend 'company-dictionary))
            (prefix (and dict (or (company-grab-symbol) 'stop)))
            (candidates
             (let ((completion-ignore-case nil)
                   (symbols dict))
               (all-completions arg symbols)))
            (sorted t)))))

    (use-package company-statistics
      :config
      (shut-up
        (setq company-statistics-file (expand-file-name "company-statistics-cache.el" TMP-DIR))
        (company-statistics-mode)))))


(provide 'core-company)
;;; core-company.el ends here
