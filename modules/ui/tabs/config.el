;;; ui/tabs/config.el -*- lexical-binding: t; -*-

(use-package! centaur-tabs
  :hook (doom-first-file . centaur-tabs-mode)
  :init
  (setq centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'left
        centaur-tabs-set-modified-marker t
        centaur-tabs-close-button "✕"
        centaur-tabs-modified-marker "•"
        ;; Scrolling (with the mouse wheel) past the end of the tab list
        ;; replaces the tab list with that of another Doom workspace. This
        ;; prevents that.
        centaur-tabs-cycle-scope 'tabs)

  :config
  (add-hook! '(+doom-dashboard-mode-hook +popup-buffer-mode-hook)
    (defun +tabs-disable-centaur-tabs-mode-maybe-h ()
      "Disable `centaur-tabs-mode' in current buffer."
      (when (centaur-tabs-mode-on-p)
        (centaur-tabs-local-mode))))

  (defadvice! +tabs--fixed-centaur-tabs-project-name-a ()
    :override #'centaur-tabs-project-name
    (let ((project-name (cdr (project-current))))
      ;; In earlier versions of project.el, `project-current' returned a cons
      ;; cell (VCBACKEND . PROJECTROOT). In more recent versions it returns
      ;; (TYPE VCBACKEND PROJECTROOT), which throws an error.
      ;; REVIEW This should be upstreamed.
      (when (listp project-name)
        (setq project-name (cadr project-name)))
      (if project-name
          (format "Project: %s" (expand-file-name project-name))
        centaur-tabs-common-group-name))))


;; TODO tab-bar-mode (emacs 27)
;; TODO tab-line-mode (emacs 27)
