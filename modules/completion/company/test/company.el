;;; completion/company/test/company.el -*- lexical-binding: t; -*-

(require! :completion company)
(require 'company)

(def-test! set-company-backend
  (let ((default-backends (default-value 'company-backends)))
    (set! :company-backend 'emacs-lisp-mode '(backend-1))
    (set! :company-backend 'lisp-interaction-mode 'backend-1 'backend-2)
    (set! :company-backend 'text-mode 'backend-1)
    (with-temp-buffer
      (emacs-lisp-mode)
      (should (equal (car company-backends) '(backend-1))))
    (with-temp-buffer
      (lisp-interaction-mode)
      (should (equal company-backends
                     (append '(backend-1 backend-2) default-backends))))
    (with-temp-buffer
      (text-mode)
      (should (eq (car company-backends) 'backend-1)))
    ;; global backends shouldn't be affected
    (should (equal company-backends default-backends))))
