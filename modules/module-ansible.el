;;; module-ansible.el

(after! company-dict
  (add-to-list 'company-dict-minor-mode-list 'ansible-mode))

(define-minor-mode ansible-mode
  :lighter " ans" :keymap (make-sparse-keymap))
(associate! ansible-mode :in (yaml-mode) :files ("roles/"))

(use-package company-ansible
  :defer t
  :init
  (define-company-backend! ansible-mode (ansible)))

(provide 'module-ansible)
;;; module-ansible.el ends here
