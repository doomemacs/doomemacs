;;; lang/org/autoload/trello.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-trello/pull-buffer ()
  (interactive)
  (org-trello-sync-buffer 1))

;;;###autoload
(defun org-trello/push-buffer ()
  (interactive)
  (org-trello-sync-buffer))

;;;###autoload
(defun org-trello/pull-card ()
  (interactive)
  (org-trello-sync-card 1))

;;;###autoload
(defun org-trello/push-card ()
  (interactive)
  (org-trello-sync-card))
