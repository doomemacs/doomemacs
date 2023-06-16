;;; ui/tabs/config.el -*- lexical-binding: t; -*-

(use-package! centaur-tabs
  :hook (server-after-make-frame . centaur-tabs-mode)
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
        (centaur-tabs-local-mode)))))

  ;; When started in daemon mode, centaur tabs does not work at all, so here is a fix
  (if (not (daemonp))
	    (centaur-tabs-mode)

    (defun centaur-tabs--daemon-mode (frame)
	    (unless (and (featurep 'centaur-tabs) (centaur-tabs-mode-on-p))
		    (run-at-time nil nil (lambda () (centaur-tabs-mode)))))
    (add-hook 'after-make-frame-functions #'centaur-tabs--daemon-mode))


;; TODO tab-bar-mode (emacs 27)
;; TODO tab-line-mode (emacs 27)
