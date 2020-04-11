;;; lang/org/+trello.el -*- lexical-binding: t; -*-
;;;###if (featurep! +trello)

(add-to-list 'auto-mode-alist '("\\.trello\\'" . org-mode))

(use-package! org-trello
  :defer t
  :init
  (map! :after org
        :map org-mode-map
        :localleader
        (:prefix ("S" . "Sync")
          (:prefix ("t" . "Trello")
            :desc "Install metadata" "i" #'org-trello-install-board-metadata
            :desc "Update metadata"  "u" #'org-trello-update-board-metadata
            :desc "Push buffer"      "p" #'+org-trello/push-buffer
            :desc "Pull buffer"      "l" #'+org-trello/pull-buffer))))
