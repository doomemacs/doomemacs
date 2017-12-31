;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; completion/company/test/company.el

(require! :completion company)
(require 'company)

;;
(def-test! set-company-backend
  :minor-mode company-mode
  (let ((company-backends '(default)))
    (set! :company-backend 'emacs-lisp-mode '(backend-1))
    (set! :company-backend 'lisp-interaction-mode 'backend-1 'backend-2)
    (set! :company-backend 'text-mode 'backend-1)
    (with-temp-buffer
      (emacs-lisp-mode)
      (should (equal company-backends '((backend-1) default))))
    (with-temp-buffer
      (lisp-interaction-mode)
      (should (equal company-backends '(backend-1 backend-2 default))))
    (with-temp-buffer
      (text-mode)
      (should (equal company-backends '(backend-1 default))))
    ;; global backends shouldn't be affected
    (should (equal company-backends '(default)))))
