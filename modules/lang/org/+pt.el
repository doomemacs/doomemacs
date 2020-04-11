;;; lang/org/+pt.el -*- lexical-binding: t; -*-
;;;###if (featurep! +pt)

(use-package! org-pivotal
  :defer t
  :init
  (map! :after org
        :map org-mode-map
        :localleader
        (:prefix-map ("S" . "Sync")
          (:prefix-map ("p" . "PT")
            :desc "Push story"   "p" #'org-pivotal-push-story
            :desc "Pull stories" "l" #'org-pivotal-pull-stories))))
