;;; lang/org/contrib/roam.el -*- lexical-binding: t; -*-
;;;###if (featurep! +roam)

(use-package! org-roam
  :commands (org-roam-insert
             org-roam-find-file
             org-roam-switch-to-buffer
             org-roam-show-graph
             org-roam)
  :hook
  (after-init . org-roam-mode)
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert" "i" #'org-roam-insert)
  (setq org-roam-directory org-directory)
  :config
  (require 'org-roam-protocol))

(use-package company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))
