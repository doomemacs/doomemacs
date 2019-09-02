;;; ui/deft/config.el -*- lexical-binding: t; -*-

(use-package! deft
  :commands deft
  :init
  (setq deft-extensions '("org" "md" "tex" "txt")
        deft-default-extension "org"
        ;; de-couples filename and note title:
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        deft-org-mode-title-prefix t
        ;; converts the filter string into a readable file-name using kebab-case:
        deft-file-naming-rules
              '((noslash . "-")
                (nospace . "-")
                (case-fn . downcase)))
  :config
  ;; start filtering immediately
  (set-evil-initial-state! 'deft-mode 'insert)
  (map! :map deft-mode-map
        :localleader
        "RET" #'deft-new-file-named
        "a"   #'deft-archive-file
        "c"   #'deft-filter-clear
        "d"   #'deft-delete-file
        "f"   #'deft-find-file
        "g"   #'deft-refresh
        "l"   #'deft-filter
        "n"   #'deft-new-file
        "r"   #'deft-rename-file
        "s"   #'deft-toggle-sort-method
        "t"   #'deft-toggle-incremental-search))
