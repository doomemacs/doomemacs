;;; ui/deft/config.el -*- lexical-binding: t; -*-

(use-package! deft
  :commands deft
  :init
  (setq deft-default-extension "org"
        ;; de-couples filename and note title:
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        ;; converts the filter string into a readable file-name using kebab-case:
        deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))
  :config
  (add-to-list 'deft-extensions "tex")
  (add-hook 'deft-mode-hook #'doom-mark-buffer-as-real-h)
  ;; start filtering immediately
  (set-evil-initial-state! 'deft-mode 'insert)
  (map! :map deft-mode-map
        :n "gr"  #'deft-refresh
        :n "C-s" #'deft-filter
        :i "C-n" #'deft-new-file
        :i "C-m" #'deft-new-file-named
        :i "C-d" #'deft-delete-file
        :i "C-r" #'deft-rename-file
        :n "r"   #'deft-rename-file
        :n "a"   #'deft-new-file
        :n "A"   #'deft-new-file-named
        :n "d"   #'deft-delete-file
        :n "D"   #'deft-archive-file
        :n "q"   #'kill-current-buffer
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
