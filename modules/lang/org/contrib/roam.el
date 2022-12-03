;;; lang/org/contrib/roam.el -*- lexical-binding: t; -*-
;;;###if (modulep! +roam)

(defvar +org-roam-open-buffer-on-find-file t
  "If non-nil, open the org-roam buffer when opening an org roam file.")


;;
;;; Packages

(use-package! org-roam
  :hook (org-load . +org-init-roam-maybe-h)
  :hook (org-roam-backlinks-mode . turn-on-visual-line-mode)
  :commands (org-roam-buffer-toggle-display
             org-roam-dailies-find-date
             org-roam-dailies-find-today
             org-roam-dailies-find-tomorrow
             org-roam-dailies-find-yesterday)
  :preface
  ;; Set this to nil so we can later detect if the user has set custom values
  ;; for these variables. If not, default values will be set in the :config
  ;; section.
  (defvar org-roam-directory nil)
  (defvar org-roam-db-location nil)
  :init
  (map! :after org
        :map org-mode-map
        :localleader
        :prefix ("m" . "org-roam")
        "b" #'org-roam-switch-to-buffer
        "f" #'org-roam-find-file
        "g" #'org-roam-graph
        "i" #'org-roam-insert
        "I" #'org-roam-insert-immediate
        "m" #'org-roam
        "t" #'org-roam-tag-add
        "T" #'org-roam-tag-delete
        (:prefix ("d" . "by date")
         :desc "Find previous note" "b" #'org-roam-dailies-find-previous-note
         :desc "Find date"          "d" #'org-roam-dailies-find-date
         :desc "Find next note"     "f" #'org-roam-dailies-find-next-note
         :desc "Find tomorrow"      "m" #'org-roam-dailies-find-tomorrow
         :desc "Capture today"      "n" #'org-roam-dailies-capture-today
         :desc "Find today"         "t" #'org-roam-dailies-find-today
         :desc "Capture Date"       "v" #'org-roam-dailies-capture-date
         :desc "Find yesterday"     "y" #'org-roam-dailies-find-yesterday
         :desc "Find directory"     "." #'org-roam-dailies-find-directory))
  :config
  (defun +org-init-roam-maybe-h ()
    "Activate `org-roam-mode'. If it fails, fail gracefully."
    (unless (with-demoted-errors "ORG ROAM ERROR: %s"
              (org-roam-mode +1)
              t)
      (message "To try reinitializing org-roam, run 'M-x org-roam-mode'")
      (org-roam-mode -1)))

  (setq org-roam-directory
        (file-name-as-directory
         (file-truename
          (expand-file-name (or org-roam-directory "roam")
                            org-directory)))
        org-roam-db-location (or org-roam-db-location
                                 (concat doom-data-dir "org-roam.db"))
        ;; Make org-roam buffer sticky; i.e. don't replace it when opening a
        ;; file with an *-other-window command.
        org-roam-buffer-window-parameters '((no-delete-other-windows . t))
        org-roam-link-use-custom-faces 'everywhere
        org-roam-completion-everywhere t
        org-roam-completion-system
        (cond ((modulep! :completion helm) 'helm)
              ((modulep! :completion ivy) 'ivy)
              ((modulep! :completion ido) 'ido)
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
  ;; makes it easier to distinguish from other org buffers.
  (add-hook 'org-roam-buffer-prepare-hook #'hide-mode-line-mode))


;; Since the org module lazy loads org-protocol (waits until an org URL is
;; detected), we can safely chain `org-roam-protocol' to it.
(use-package! org-roam-protocol
  :after org-protocol)
