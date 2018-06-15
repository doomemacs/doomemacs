;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; completion/company/test/test-company.el

(load! "../autoload")

;;
(xdescribe "completion/company"
  :var (company-backends)

  (before-all
    (provide 'company))
  (after-all
    (unload-feature 'company t))

  (describe ":company-backend"
    :var (text-mode-hook company-backends)
    (before-each
      (setq company-backends '(default)
            text-mode-hook nil))

    (it "adds grouped backends"
      (set-company-backend! 'text-mode '(backend-1))
      (with-temp-buffer
        (text-mode)
        (expect company-backends :to-equal '((backend-1) default))))

    (it "adds multiple backends"
      (set-company-backend! 'text-mode 'backend-1 'backend-2)
      (with-temp-buffer
        (text-mode)
        (expect company-backends :to-equal '(backend-1 backend-2 default))))

    (it "adds single backend"
      (set-company-backend! 'text-mode 'backend-1)
      (with-temp-buffer
        (text-mode)
        (expect company-backends :to-equal '(backend-1 default))))

    (it "overwrites past values"
      (set-company-backend! 'text-mode 'backend-1)
      (set-company-backend! 'text-mode 'backend-2)
      (with-temp-buffer
        (text-mode)
        (expect company-backends :to-equal '(backend-2 default))))))
