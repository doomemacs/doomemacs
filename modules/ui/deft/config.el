;;; ui/deft/config.el -*- lexical-binding: t; -*-

(def-package! deft
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
        :n "RET" #'deft-new-file-named
        :n "a" #'deft-archive-file
        :n "c" #'deft-filter-clear
        :n "d" #'deft-delete-file
        :n "f" #'deft-find-file
        :n "g" #'deft-refresh
        :n "l" #'deft-filter
        :n "n" #'deft-new-file
        :n "r" #'deft-rename-file
        :n "s" #'deft-toggle-sort-method
        :n "t" #'deft-toggle-incremental-search))
