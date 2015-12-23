;;; core-helm.el

(use-package helm
  :init
  (defvar helm-global-prompt "⮁⮁⮁ ")
  (setq-default
   helm-quick-update t
   helm-reuse-last-window-split-state t

   helm-mode-fuzzy-match nil
   helm-buffers-fuzzy-matching nil
   helm-apropos-fuzzy-match nil
   helm-M-x-fuzzy-match nil
   helm-recentf-fuzzy-match nil
   helm-projectile-fuzzy-match t

   helm-display-header-line nil
   helm-ff-auto-update-initial-value nil
   helm-ff-skip-boring-files t
   helm-find-files-doc-header nil
   helm-move-to-line-cycle-in-source t

   ;; Don't override evil-ex's completion
   helm-mode-handle-completion-in-region nil

   helm-candidate-number-limit 50
   helm-bookmark-show-location t)

  :config
  (require 'helm-files)

  (mapc (lambda (r) (add-to-list 'helm-boring-file-regexp-list r))
        (list "\\.projects$" "\\.DS_Store$"))

  (map! (:map (helm-map helm-generic-files-map helm-find-files-map helm-swoop-map helm-projectile-find-file-map)
          "C-w"        'backward-kill-word
          "C-r"        'evil-paste-from-register ; Evil registers in helm! Glorious!
          "/"          nil
          "<left>"     'backward-char
          "<right>"    'forward-char
          "<escape>"   'helm-keyboard-quit
          [escape]     'helm-keyboard-quit)
        (:map helm-find-files-map
          "C-w"        'helm-find-files-up-one-level
          "TAB"        'helm-execute-persistent-action
          "/"          'helm-execute-persistent-action)
        (:map helm-ag-map
          "<backtab>"  'helm-ag-edit)
        (:map helm-ag-edit-map
          "<escape>"   'helm-ag--edit-abort
          :n "zx"      'helm-ag--edit-abort)
        (:map helm-map
          "C-S-n"        'helm-next-source
          "C-S-p"        'helm-previous-source
          "C-u"        'helm-delete-minibuffer-contents))

  ;;; Helm hacks
  (defvar narf-helm-header-fg (face-attribute 'helm-source-header :foreground) "docstring")
  (defun narf*helm-hide-source-header-maybe ()
    (if (<= (length helm-sources) 1)
        (set-face-attribute 'helm-source-header nil :height 0.1 :foreground "#111111")
      (set-face-attribute 'helm-source-header nil :height 1.0 :foreground narf-helm-header-fg)))

  (defun narf*helm-hide-header (source &optional force)
    (setq header-line-format nil)
    (setq mode-line-format nil))

  (defun narf*helm-replace-prompt (plist)
    (if (keywordp (car plist))
        (setq plist (plist-put plist :prompt helm-global-prompt))
      (setcar (nthcdr 2 plist) helm-global-prompt))
    plist)

  ;; Shrink source headers if there is only one source
  (add-hook 'helm-after-initialize-hook 'narf*helm-hide-source-header-maybe)
  ;; A simpler prompt: see `helm-global-prompt'
  (advice-add 'helm :filter-args 'narf*helm-replace-prompt)
  ;; Hide mode-line in helm windows
  (advice-add 'helm-display-mode-line :override 'narf*helm-hide-header)

  (helm-mode 1))

(use-package projectile
  :config
  (add-hook! kill-emacs 'narf|projectile-invalidate-cache-maybe)

  (setq-default projectile-enable-caching t)
  (setq projectile-require-project-root nil
        projectile-cache-file (concat narf-temp-dir "projectile.cache")
        projectile-known-projects-file (concat narf-temp-dir "projectile.projects")
        projectile-indexing-method 'alien
        projectile-project-root-files narf-project-root-files)

  (add-to-list 'projectile-globally-ignored-files "ido.last")
  (add-to-list 'projectile-globally-ignored-directories "assets")
  (add-to-list 'projectile-other-file-alist '("scss" "css"))
  (add-to-list 'projectile-other-file-alist '("css" "scss"))

  (projectile-global-mode +1)

  (require 'helm-projectile))

(use-package helm-ag
  :commands (helm-ag
             helm-ag-mode
             helm-do-ag
             helm-do-ag-this-file
             helm-do-ag-project-root
             helm-do-ag-buffers
             helm-ag-project-root
             helm-ag-pop-stack
             helm-ag-buffers
             helm-ag-clear-stack))

(use-package helm-org
  :commands (helm-org-in-buffer-headings
             helm-org-agenda-files-headings
             helm-org-capture-templates))

(use-package helm-css-scss ; https://github.com/ShingoFukuyama/helm-css-scss
  :commands (helm-css-scss
             helm-css-scss-multi
             helm-css-scss-insert-close-comment)
  :config
  (setq helm-css-scss-split-direction 'split-window-vertically
        helm-css-scss-split-with-multiple-windows t))

(use-package helm-swoop    ; https://github.com/ShingoFukuyama/helm-swoop
  :defines  (helm-swoop-last-prefix-number)
  :commands (helm-swoop helm-multi-swoop helm-multi-swoop-all)
  :config
  (setq helm-swoop-use-line-number-face t
        helm-swoop-split-with-multiple-windows t
        helm-swoop-candidate-number-limit 200
        helm-swoop-speed-or-color t
        helm-swoop-pre-input-function (lambda () "")))

;; (use-package helm-c-yasnippet :commands helm-yas-visit-snippet-file)
(use-package helm-semantic :commands helm-semantic-or-imenu)
(use-package helm-elisp    :commands helm-apropos)
(use-package helm-command  :commands helm-M-x)

(use-package helm-descbinds :commands helm-descbinds
  :config
  (setq helm-descbinds-window-style 'split-window)
  (defun narf/helm-descbinds-leader ()
    (interactive)
    (narf-helm-descbinds "^,\\ "))
  (defun narf/helm-descbinds-localleader ()
    (interactive)
    (narf-helm-descbinds "^\\\\\\ "))

  (defun narf-helm-descbinds (&optional input buffer)
    (let ((enable-recursive-minibuffers t))
      (helm :sources (helm-descbinds-sources (or buffer (current-buffer)))
            :buffer "*helm-descbinds*"
            :resume 'noresume
            :allow-nest t
            :input input))))

(provide 'core-helm)
;;; core-helm.el ends here
