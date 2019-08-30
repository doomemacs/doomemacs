;;; +hugo.el --- ox-hugo support -*- lexical-binding: t; -*-
;;;###if (featurep! +hugo)

(use-package! ox-hugo
  :after ox)

(map! :map org-mode-map
      :localleader
      (:prefix "e"
        (:prefix ("H" . "hugo")
          :desc "Subtree or File to Md to file"        "H" #'org-hugo-export-wim-to-md
          :desc "File to Md file"                      "h" #'org-hugo-export-to-md
          :desc "Subtree or File to Md to file & open" "O" '(lambda ()
                                                              (interactive)
                                                              (org-open-file (org-hugo-export-wim-to-md)))
          :desc "File to Md file & open"               "o" '(lambda ()
                                                              (interactive)
                                                              (org-open-file (org-hugo-export-to-md)))
          :desc "All subtrees (or File) to Md file(s)" "A" '(lambda ()
                                                              (interactive)
                                                              (org-hugo-export-wim-to-md :all-subtrees))
          :desc "File to a temporary Md buffer"        "t" #'org-hugo-export-as-md)))

;;; +hugo.el ends here
