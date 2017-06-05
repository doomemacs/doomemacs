;;; lang/org/+agenda.el

(after! org-agenda
  (setq-default
   diary-file (concat doom-local-dir "diary.org")
   ;; calendar-mark-diary-entries-flag nil
   org-agenda-dim-blocked-tasks nil
   org-agenda-files (directory-files +org-dir t "\\.org$" t)
   org-agenda-inhibit-startup t
   org-agenda-skip-unavailable-files nil)

  (after! recentf
    ;; Don't clobber recentf with agenda files
    (defun +org-is-agenda-file (filename)
      (cl-find (file-truename filename) org-agenda-files
               :key #'file-truename
               :test #'equal))
    (add-to-list 'recentf-exclude #'+org-is-agenda-file))

  ;;
  (map! :map org-agenda-mode-map
        :e "<escape>" #'org-agenda-Quit
        :e "m"   #'org-agenda-month-view
        :e "C-j" #'org-agenda-next-item
        :e "C-k" #'org-agenda-previous-item
        :e "C-n" #'org-agenda-next-item
        :e "C-p" #'org-agenda-previous-item))
