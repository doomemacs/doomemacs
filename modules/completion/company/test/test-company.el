;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; completion/company/test/test-company.el

(describe "completion/company"
  (before-all
    (load! "../autoload"))

  (describe ":company-backend"
    :var (a)
    (before-each
      (setq company-backends '(default)
            text-mode-hook nil
            a (get-buffer-create "x"))
      (set-buffer a)
      (spy-on 'require))
    (after-each
      (fmakunbound '+company|init-text-mode)
      (kill-buffer a))

    (it "adds hooks and defines +company|init-MODE"
      (set-company-backend! 'text-mode '(backend-1))
      (expect (fboundp '+company|init-text-mode))
      (expect text-mode-hook :to-equal '(+company|init-text-mode)))

    (it "adds grouped backends"
      (set-company-backend! 'text-mode '(backend-1))
      (text-mode)
      (expect company-backends :to-equal '((backend-1) default)))

    (it "adds multiple backends"
      (set-company-backend! 'text-mode 'backend-1 'backend-2)
      (text-mode)
      (expect company-backends :to-equal '(backend-1 backend-2 default)))

    (it "adds single backend"
      (set-company-backend! 'text-mode 'backend-1)
      (text-mode)
      (expect company-backends :to-equal '(backend-1 default)))

    (it "overwrites past values"
      (set-company-backend! 'text-mode 'backend-1)
      (set-company-backend! 'text-mode 'backend-2)
      (text-mode)
      (expect company-backends :to-equal '(backend-2 default)))

    (it "unsets past values"
      (set-company-backend! 'text-mode 'backend-1)
      (set-company-backend! 'text-mode nil)
      (text-mode)
      (expect company-backends :to-equal '(default)))))
