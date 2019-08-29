;;; +hugo.el --- ox-hugo support -*- lexical-binding: t; -*-
;;;###if (featurep! +hugo)

(use-package! ox-hugo
  :after ox
  (map! :map org-mode-map
        :localleader
        (:prefix ("e" . "export")
          :desc "to hugo"        "h" #'org-hugo-export-to-md
          :desc "to hugo & open" "H" #'org-hugo-export-as-md)))

;;; +hugo.el ends here
