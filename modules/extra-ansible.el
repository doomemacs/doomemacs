;;; extra-ansible.el

(def-project-type! ansible-mode "ans"
  :modes (yaml-mode)
  :files ("roles/"))

(use-package company-ansible
  :commands (company-ansible)
  :init (def-company-backend! ansible-mode (ansible)))

(provide 'extra-ansible)
;;; module-ansible.el ends here
