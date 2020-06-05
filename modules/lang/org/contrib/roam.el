;;; lang/org/contrib/roam.el -*- lexical-binding: t; -*-
;;;###if (featurep! +roam)

(defvar +org-roam-open-buffer-on-find-file t
  "If non-nil, open the org-roam buffer when opening an org roam file.")


;;
;;; Packages

(use-package! org-roam
  :hook (org-load . org-roam-mode)
  :hook (org-roam-backlinks-mode . turn-on-visual-line-mode)
  :commands (org-roam-buffer-toggle-display
             org-roam-find-file
             org-roam-graph
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
        "g" #'org-roam-graph
        "i" #'org-roam-insert
        "m" #'org-roam
        (:prefix ("d" . "by date")
          :desc "Arbitrary date" "d" #'org-roam-dailies-date
          :desc "Today"          "t" #'org-roam-dailies-today
          :desc "Tomorrow"       "m" #'org-roam-dailies-tomorrow
          :desc "Yesterday"      "y" #'org-roam-dailies-yesterday))
  :config
  (setq org-roam-directory (expand-file-name (or org-roam-directory "roam")
                                             org-directory)
        org-roam-verbose nil  ; https://youtu.be/fn4jIlFwuLU
        org-roam-buffer-window-parameters '((no-delete-other-windows . t)) ; make org-roam buffer sticky
        org-roam-completion-system
        (cond ((featurep! :completion helm) 'helm)
              ((featurep! :completion ivy) 'ivy)
              ((featurep! :completion ido) 'ido)
              ('default)))

  ;; Normally, the org-roam buffer doesn't open until you explicitly call
  ;; `org-roam'. If `+org-roam-open-buffer-on-find-file' is non-nil, the
  ;; org-roam buffer will be opened for you when you use `org-roam-find-file'
  ;; (but not `find-file', to limit the scope of this behavior).
  (add-hook! 'find-file-hook
    (defun +org-roam-open-buffer-maybe-h ()
      (and +org-roam-open-buffer-on-find-file
           (memq 'org-roam-buffer--update-maybe post-command-hook)
           (not (window-parameter nil 'window-side)) ; don't proc for popups
           (not (eq 'visible (org-roam-buffer--visibility)))
           (with-current-buffer (window-buffer)
             (org-roam-buffer--get-create)))))

  ;; Hide the mode line in the org-roam buffer, since it serves no purpose. This
  ;; makes it easier to distinguish among other org buffers.
  (add-hook 'org-roam-buffer-prepare-hook #'hide-mode-line-mode))


;; Since the org module lazy loads org-protocol (waits until an org URL is
;; detected), we can safely chain `org-roam-protocol' to it.
(use-package! org-roam-protocol
  :after org-protocol)


(use-package! company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))
