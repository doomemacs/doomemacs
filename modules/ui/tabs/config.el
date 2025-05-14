;;; ui/tabs/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(use-package! centaur-tabs
  :defer t
  :init
  (setq centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'left
        centaur-tabs-set-modified-marker t
        centaur-tabs-close-button "✕"
        centaur-tabs-modified-marker "•"
        centaur-tabs-icon-type 'nerd-icons
        ;; Scrolling (with the mouse wheel) past the end of the tab list
        ;; replaces the tab list with that of another Doom workspace. This
        ;; prevents that.
        centaur-tabs-cycle-scope 'tabs)

  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'centaur-tabs-mode)
    (add-hook 'doom-first-file-hook #'centaur-tabs-mode))

  :config
  (defun +tabs-buffer-list ()
    (seq-filter (lambda (b)
                  (when (buffer-live-p b)
                    (or (eq (current-buffer) b)
                        (not (doom-temp-buffer-p b)))))
                (if (bound-and-true-p persp-mode)
                    (persp-buffer-list)
                  (buffer-list))))
  (setq centaur-tabs-buffer-list-function #'+tabs-buffer-list)

  (add-hook! '(+doom-dashboard-mode-hook +popup-buffer-mode-hook)
    (defun +tabs-disable-centaur-tabs-mode-maybe-h ()
      "Disable `centaur-tabs-mode' in current buffer."
      (when (centaur-tabs-mode-on-p)
        (centaur-tabs-local-mode)))))

;; TODO tab-bar-mode (emacs 27)
;; TODO tab-line-mode (emacs 27)
