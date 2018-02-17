;;; feature/file-templates/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +file-templates-get-short-path ()
  "TODO"
  (when (string-match "/modules/\\(.+\\)$" buffer-file-truename)
    (match-string 1 buffer-file-truename)))

;;;###autoload
(defun +file-templates/insert-license ()
  "Insert a license file template into the current file."
  (interactive)
  (require 'yasnippet)
  (let* ((templates
          (let ((yas-choose-tables-first nil) ; avoid prompts
                (yas-choose-keys-first nil))
            (cl-loop for tpl in (yas--all-templates (yas--get-snippet-tables 'text-mode))
                     for uuid = (yas--template-uuid tpl)
                     if (string-prefix-p "__license-" uuid)
                     collect (cons (string-remove-prefix "__license-" uuid) tpl))))
         (uuid (yas-choose-value (mapcar #'car templates))))
    (when uuid
      (yas-expand-snippet (cdr (assoc uuid templates))))))
