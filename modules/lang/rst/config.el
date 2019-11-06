(use-package! sphinx-mode
  :init
  (add-hook! 'rst-mode-hook #'sphinx-mode))

(use-package! rst
  :config
  (map! :localleader
        :map rst-mode-map
        (:prefix ("a" . "adjust")
          ("a" #'rst-adjust
           "r" #'rst-adjust-region))
        (:prefix ("t" . "table of contents")
          ("t" #'rst-toc
           "i" #'rst-toc-insert
           "u" #'rst-toc-update
           "f" #'rst-toc-follow-link))))
