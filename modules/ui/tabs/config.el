;;; ui/tabs/config.el -*- lexical-binding: t; -*-

(defcustom +tabs-buffer-update-groups-delay 0.1
  "Minimum wait time (in seconds) before tab groups are recalculated."
  :type 'float
  :group 'doom)


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
  (add-hook! '(+doom-dashboard-mode-hook +popup-buffer-mode-hook)
    (defun +tabs-disable-centaur-tabs-mode-maybe-h ()
      "Disable `centaur-tabs-mode' in current buffer."
      (when (centaur-tabs-mode-on-p)
        (centaur-tabs-local-mode))))

  ;; HACK: `centaur-tabs-buffer-update-groups' is both expensive and called too
  ;;   frequently. There really is no reason to call it more than 10 times per
  ;;   second, as buffers rarely change groups more frequently than that.
  (let ((time (float-time)))
    (defadvice! +tabs--rate-limit-buffer-update-groups-a (fn)
      :around #'centaur-tabs-buffer-update-groups
      (let ((now (float-time)))
        (if-let ((buf (and (< now (+ time +tabs-buffer-update-groups-delay))
                           (assq (current-buffer) centaur-tabs--buffers))))
            (car (nth 2 buf))
          (setq time now)
          (funcall fn)))))

  ;; This is deferred twice to ensure these settings apply *after* any user
  ;; configuration!
  (after! centaur-tabs
    ;; HACK: `centaur-tabs-line-tab' reads `centaur-tabs-ace-jump-keys' without
    ;;   length guards. If there are fewer entries than you have tabs, you'll
    ;;   see an error (ema2159/centaur-tabs#231).
    ;; REVIEW: Remove when ema2159/centaur-tabs#231 is dealt with.
    (when (< (length centaur-tabs-ace-jump-keys) 100)
      (setq centaur-tabs-ace-jump-keys
            (append centaur-tabs-ace-jump-keys (make-list 100 ?\ ))))))

;; TODO tab-bar-mode (emacs 27)
;; TODO tab-line-mode (emacs 27)
