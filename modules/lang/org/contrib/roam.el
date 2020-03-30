;;; lang/org/contrib/roam.el -*- lexical-binding: t; -*-
;;;###if (featurep! +roam)

(use-package! org-roam
  :commands (org-roam
             org-roam-insert
             org-roam-find-file
             org-roam-switch-to-buffer
             org-roam-show-graph)
  :init
  (map! :after org
        :map org-mode-map
        :localleader
        :prefix ("m" . "org-roam")
        "m" #'org-roam
        "i" #'org-roam-insert
        "b" #'org-roam-switch-to-buffer
        "f" #'org-roam-find-file
        "g" #'org-roam-show-graph
        "i" #'org-roam-insert)
  :config
  (setq org-roam-directory org-directory)
  (org-roam-mode +1))


;; Since the org module lazy loads org-protocol (waits until an org URL is
;; detected), we can safely chain `org-roam-protocol' to it.
(use-package! org-roam-protocol
  :after org-protocol)


(use-package! company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))
