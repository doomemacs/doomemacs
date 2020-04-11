;;; lang/org/+sidebar.el -*- lexical-binding: t; -*-
;;;###if (featurep! +sidebar)

(use-package! org-sidebar
  :defer t
  :init
  (map! :after org
        :map org-mode-map
        :localleader
        (:prefix-map ("u" . "UI toggle")
          :desc "Toggle sidebar"      "s" #'org-sidebar-toggle
          :desc "Toggle sidebar tree" "t" #'org-sidebar-tree-toggle)))
