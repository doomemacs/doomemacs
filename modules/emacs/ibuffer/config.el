;;; emacs/ibuffer/config.el -*- lexical-binding: t; -*-

(after! ibuffer
  (set-popup-rule! "^\\*Ibuffer\\*$" :ignore t)

  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-filter-group-name-face '(:inherit (success bold))
        ibuffer-formats
        `((mark modified read-only locked
                ,@(if (modulep! +icons)
                      `(;; Here you may adjust by replacing :right with :center
                        ;; or :left According to taste, if you want the icon
                        ;; further from the name
                        " " (icon 2 2 :left :elide)
                        ,(propertize " " 'display `(space :align-to 8)))
                    '(" "))
                (name 18 18 :left :elide)
                " " (size 9 -1 :right)
                " " (mode 16 16 :left :elide)
                ,@(when (require 'ibuffer-vc nil t)
                    '(" " (vc-status 12 :left)))
                " " filename-and-process)
          (mark " " (name 16 -1) " " filename)))

  ;; Display buffer icons on GUI
  (define-ibuffer-column icon (:name "  ")
    (let ((icon (if (and (buffer-file-name)
                         (all-the-icons-auto-mode-match?))
                    (all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                  (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
      (if (symbolp icon)
          (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0))
        icon)))

  ;; Redefine size column to display human readable size
  (define-ibuffer-column size
    (:name "Size"
     :inline t
     :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size)))

  (when (modulep! :ui workspaces)
    (define-ibuffer-filter workspace-buffers
        "Filter for workspace buffers"
      (:reader (+workspace-get (read-string "workspace name: "))
       :description "workspace")
      (memq buf (+workspace-buffer-list qualifier)))

    (define-key ibuffer-mode-map [remap ibuffer-visit-buffer] #'+ibuffer/visit-workspace-buffer))

  (when (modulep! :completion ivy)
    (defadvice! +ibuffer--use-counsel-maybe-a (_file &optional _wildcards)
      "Use `counsel-find-file' instead of `find-file'."
      :override #'ibuffer-find-file
      (interactive
       (let* ((buf (ibuffer-current-buffer))
              (default-directory (if (buffer-live-p buf)
                                     (with-current-buffer buf
                                       default-directory)
                                   default-directory)))
         (list (counsel--find-file-1 "Find file: " nil
                                     #'identity
                                     'counsel-find-file) t)))
      (find-file _file _wildcards)))

  (map! :map ibuffer-mode-map :n "q" #'kill-current-buffer))


(use-package! ibuffer-projectile
  ;; Group ibuffer's list by project root
  :hook (ibuffer . ibuffer-projectile-set-filter-groups)
  :config
  (setq ibuffer-projectile-prefix
        (if (modulep! +icons)
            (concat (all-the-icons-octicon
                     "file-directory"
                     :face ibuffer-filter-group-name-face
                     :v-adjust -0.05)
                    " ")
          "Project: ")))
