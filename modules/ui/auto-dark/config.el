;;; ui/auto-dark/config.el -*- lexical-binding: t; -*-

(use-package! auto-dark
  :hook (doom-load-theme . auto-dark-mode)
  :config
  ;; HACK: `auto-dark--enable-themes' disables all current themes then loads
  ;;   the target themes from scratch via `load-theme'. Doom's `load-theme'
  ;;   advice already handles enabling themes and running hooks, so this causes
  ;;   redundant work and can flash. Override to only disable non-target themes
  ;;   and enable (not reload) themes that are already loaded.
  (defadvice! +auto-dark--use-doom-theme-loading-a (&optional themes)
    "Use Doom's theme infrastructure when switching themes."
    :override #'auto-dark--enable-themes
    (let ((target-themes (remq 'user (delete-dups (or themes custom-enabled-themes)))))
      ;; Disable themes not in the target set
      (dolist (theme custom-enabled-themes)
        (unless (memq theme target-themes)
          (disable-theme theme)))
      ;; Enable target themes, loading if necessary
      (dolist (theme (reverse target-themes))
        (if (custom-theme-p theme)
            (enable-theme theme)
          (load-theme theme t)))))

  ;; Sync `doom-theme' when auto-dark switches themes, so that
  ;; `doom/reload-theme' reloads the correct (current) theme.
  (add-hook! '(auto-dark-dark-mode-hook auto-dark-light-mode-hook)
    (defun +auto-dark-sync-doom-theme-h ()
      "Update `doom-theme' to reflect auto-dark's current theme set."
      (when-let* ((themes (auto-dark--themes-for-mode
                           auto-dark--last-dark-mode-state)))
        (setq doom-theme (if (cdr themes) themes (car themes)))))))
