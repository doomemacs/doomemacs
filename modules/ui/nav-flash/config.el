;;; ui/nav-flash/config.el

(def-package! nav-flash
  :commands nav-flash-show
  :init
  (defun +doom*blink-cursor-maybe (orig-fn &rest args)
    "Blink current line if the window has moved."
    (let ((point (save-excursion (goto-char (window-start))
                                 (point-marker))))
      (apply orig-fn args)
      (unless (equal point
                     (save-excursion (goto-char (window-start))
                                     (point-marker)))
        (+doom/blink-cursor))))

  (defun +doom/blink-cursor (&rest _)
    "Blink current line using `nav-flash'."
    (interactive)
    (unless (minibufferp)
      (nav-flash-show)
      ;; only show in the current window
      (overlay-put compilation-highlight-overlay 'window (selected-window))))

  ;; NOTE In :feature jump `recenter' is hooked to a bunch of jumping commands,
  ;; which will trigger nav-flash.

  (advice-add #'windmove-do-window-select :around #'+doom*blink-cursor-maybe)
  (advice-add #'recenter :around #'+doom*blink-cursor-maybe)

  (after! evil
    (advice-add #'evil-window-top    :after #'+doom/blink-cursor)
    (advice-add #'evil-window-middle :after #'+doom/blink-cursor)
    (advice-add #'evil-window-bottom :after #'+doom/blink-cursor)))


