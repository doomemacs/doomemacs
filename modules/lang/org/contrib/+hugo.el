;;; +hugo.el --- ox-hugo support -*- lexical-binding: t; -*-
;;;###if (featurep! +hugo)

(defun org-hugo-doom-subtree-to-file-and-open ()
  (interactive)
  (org-open-file (org-hugo-export-wim-to-md)))

(defun org-hugo-doom-to-file-and-open ()
  (interactive)
  (org-open-file (org-hugo-export-to-md)))

(defun org-hugo-doom-all-subtrees-to-files ()
  (interactive)
  (org-hugo-export-wim-to-md :all-subtrees))

(use-package! ox-hugo
  :after ox)

(map! :map org-mode-map
      :localleader
      (:prefix "e"
        (:prefix ("H" . "hugo")
          :desc "Subtree to file"         "H" #'org-hugo-export-wim-to-md
          :desc "To file"                 "h" #'org-hugo-export-wim-to-md
          :desc "Subtree to file & open"  "O" #'org-hugo-doom-subtree-to-file-and-open
          :desc "To file & open"          "o" #'org-hugo-doom-to-file-and-open
          :desc "All subtrees to files"   "a" #'org-hugo-doom-all-subtrees-to-files
          :desc "To temporary buffer"     "t" #'org-hugo-export-as-md)))

;;; +hugo.el ends here
