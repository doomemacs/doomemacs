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
        centaur-tabs-icon-type 'nerd-icons
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

  ;; HACK: The function centaur-tabs-buffer-update-groups gets called way too
  ;; frequently leading to performance degredation.
  ;; There is really no reason to call it more than 10 times a second,
  ;; as buffers do not change groups more frequently than that.
  (defvar centaur-tabs-buffer-groups-patience 0.1
    "The amount of time (in seconds) to wait before recalculating groups.")
  (setq centaur-tabs--buffers-time (float-time))
  (defadvice! centaur-tabs-buffer-update-groups--patience (fn)
    "Add patience to centaur-tabs-buffer-update-groups"
    :around #'centaur-tabs-buffer-update-groups
    (if (and (< (time-to-seconds)
                (+ centaur-tabs-buffer-groups-patience
                   centaur-tabs--buffers-time))
             centaur-tabs--buffers
             (assq (current-buffer) centaur-tabs--buffers))
        (car (nth 2 (assq (current-buffer) centaur-tabs--buffers)))
      (setq centaur-tabs--buffers-time (float-time))
      (funcall fn))))


;; TODO tab-bar-mode (emacs 27)
;; TODO tab-line-mode (emacs 27)
