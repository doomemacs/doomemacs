;;; lang/org/contrib/roam.el -*- lexical-binding: t; -*-
;;;###if (featurep! +roam)

(use-package! org-roam
  :hook (org-load . org-roam-mode)
  :hook (org-roam-backlinks-mode . turn-on-visual-line-mode)
  :commands (org-roam-buffer-toggle-display
             org-roam-find-file
             org-roam-graph-show
             org-roam-insert
             org-roam-switch-to-buffer
             org-roam-dailies-date
             org-roam-dailies-today
             org-roam-dailies-tomorrow
             org-roam-dailies-yesterday)
  :preface
  ;; Set this to nil so we can later detect whether the user has set a custom
  ;; directory for it, and default to `org-directory' if they haven't.
  (defvar org-roam-directory nil)
  :init
  (map! :after org
        :map org-mode-map
        :localleader
        :prefix ("m" . "org-roam")
        "b" #'org-roam-switch-to-buffer
        "f" #'org-roam-find-file
        "g" #'org-roam-graph-show
        "i" #'org-roam-insert
        "m" #'org-roam
        (:prefix ("d" . "by date")
          :desc "Arbitrary date" "d" #'org-roam-dailies-date
          :desc "Today"          "t" #'org-roam-dailies-today
          :desc "Tomorrow"       "m" #'org-roam-dailies-tomorrow
          :desc "Yesterday"      "y" #'org-roam-dailies-yesterday))
  :config
  (setq org-roam-directory (expand-file-name (or org-roam-directory "")
                                             org-directory)
        org-roam-verbose nil  ; https://youtu.be/fn4jIlFwuLU
        org-roam-completion-system
        (cond ((featurep! :completion helm) 'helm)
              ((featurep! :completion ivy) 'ivy)
              ((featurep! :completion ido) 'ido)
              ('default)))

  ;; HACK Hide the mode line in the org-roam buffer, since it serves no purpose.
  ;;      This makes it easier to distinguish among other org buffers.
  (defadvice! +org--hide-mode-line-a (&rest _)
    :after #'org-roam-buffer--get-create
    (with-current-buffer org-roam-buffer
      (hide-mode-line-mode +1))))


;; Since the org module lazy loads org-protocol (waits until an org URL is
;; detected), we can safely chain `org-roam-protocol' to it.
(use-package! org-roam-protocol
  :after org-protocol)


(use-package! company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))
