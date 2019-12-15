;;; emacs/vc/autoload/vc.el -*- lexical-binding: t; -*-

(autoload 'browse-at-remote--file-url "browse-at-remote")

;;;###autoload
(defun +vc/browse-at-remote-file-or-region ()
  "Open the current file at remote in your browser.
If a selection is active, highlight them. Otherwise omits the #L<N> suffix in
the URL."
  (interactive)
  (if (or (use-region-p)
          (ignore-errors (evil-visual-state-p)))
      (browse-at-remote)
    (browse-url (browse-at-remote--file-url (buffer-file-name)))))

;;;###autoload
(defun +vc/browse-at-remote-kill-file-or-region ()
  "Copy the current file's remote URL to your clipboard.
If a selection is active, highlight them. Otherwise omits the #L<N> suffix in
the URL."
  (interactive)
  (if (or (use-region-p)
          (ignore-errors (evil-visual-state-p)))
      (browse-at-remote-kill)
    (kill-new (browse-at-remote--file-url (buffer-file-name)))))


(defun +vc--remote-homepage ()
  (or (let ((url (browse-at-remote--remote-ref)))
        (cdr (browse-at-remote--get-url-from-remote (car url))))
      (user-error "Can't find homepage for current project")))

;;;###autoload
(defun +vc/browse-at-remote-homepage ()
  "Open homepage for current project in browser."
  (interactive)
  (browse-url (+vc--remote-homepage)))

;;;###autoload
(defun +vc/browse-at-remote-kill-homepage ()
  "Copy homepage URL of current project to clipboard."
  (interactive)
  (kill-new (+vc--remote-homepage)))
