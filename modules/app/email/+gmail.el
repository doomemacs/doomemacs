;;; app/email/+gmail.el -*- lexical-binding: t; -*-

(after! mu4e
   ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; In my workflow, emails won't be moved at all. Only their flags/labels are
  ;; changed. Se we redefine the trash and refile marks not to do any moving.
  ;; However, the real magic happens in `+email|gmail-fix-flags'.
  ;;
  ;; Gmail will handle the rest.
  (defun +email--mark-seen (docid msg target)
    (mu4e~proc-move docid (mu4e~mark-check-target target) "+S-u-N"))

  (delq (assq 'delete mu4e-marks) mu4e-marks)
  (setf (alist-get 'trash mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (_target msg) (mu4e-get-trash-folder msg))
              :action #'+email--mark-seen)
        ;; Refile will be my "archive" function.
        (alist-get 'refile mu4e-marks)
        (list :char '("d" . "▼")
              :prompt "dtrash"
              :dyn-target (lambda (_target msg) (mu4e-get-trash-folder msg))
              :action #'+email--mark-seen))

  ;; This hook correctly modifies gmail flags on emails when they are marked.
  ;; Without it, refiling (archiving), trashing, and flagging (starring) email
  ;; won't properly result in the corresponding gmail action, since the marks
  ;; are ineffectual otherwise.
  (defun +email|gmail-fix-flags (mark msg)
    (pcase mark
      (`trash  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
      (`refile (mu4e-action-retag-message msg "-\\Inbox"))
      (`flag   (mu4e-action-retag-message msg "+\\Starred"))
      (`unflag (mu4e-action-retag-message msg "-\\Starred"))))
  (add-hook 'mu4e-mark-execute-pre-hook #'+email|gmail-fix-flags))
