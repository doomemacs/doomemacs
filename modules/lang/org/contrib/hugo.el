;;; +hugo.el --- ox-hugo support -*- lexical-binding: t; -*-
;;;###if (featurep! +hugo)

(use-package! ox-hugo
  :after ox
  :preface
  (map! :after org
        :map org-mode-map
        :localleader
        (:prefix "e"
          (:prefix ("H" . "hugo")
            :desc "Subtree or File to Md to file"        "H" #'org-hugo-export-wim-to-md
            :desc "File to Md file"                      "h" #'org-hugo-export-to-md
            :desc "Subtree or File to Md to file & open" "O" (λ! (org-open-file (org-hugo-export-wim-to-md)))
            :desc "File to Md file & open"               "o" (λ! (org-open-file (org-hugo-export-to-md)))
            :desc "All subtrees (or File) to Md file(s)" "A" (λ! (org-hugo-export-wim-to-md :all-subtrees))
            :desc "File to a temporary Md buffer"        "t" #'org-hugo-export-as-md))))
