;;; editor/file-templates/autoload.el -*- lexical-binding: t; -*-

(defun +file-templates--set (pred plist)
  (if (null (car-safe plist))
      (setq +file-templates-alist
            (delq (assoc pred +file-templates-alist)
                  +file-templates-alist))
    (push `(,pred ,@plist) +file-templates-alist)))

;;;###autodef
(defun set-file-template! (pred &rest plist)
  "Register a file template.

PRED can either be a regexp string or a major mode symbol. PLIST may contain
these properties:

  :when FUNCTION
    Provides a secondary predicate. This function takes no arguments and is
    executed from within the target buffer. If it returns nil, this rule will be
    skipped over.
  :trigger STRING|FUNCTION
    If a string, this is the yasnippet trigger keyword used to trigger the
      target snippet.
    If a function, this function will be run in the context of the buffer to
      insert a file template into. It is given no arguments and must insert text
      into the current buffer manually.
    If omitted, `+file-templates-default-trigger' is used.
  :mode SYMBOL
    What mode to get the yasnippet snippet from. If omitted, either PRED (if
    it's a major-mode symbol) or the mode of the buffer is used.
  :project BOOL
    If non-nil, ignore this template if this buffer isn't in a project.
  :ignore BOOL
    If non-nil, don't expand any template for this file and don't test any other
    file template rule against this buffer.

\(fn PRED &key WHEN TRIGGER MODE PROJECT IGNORE)"
  (declare (indent defun))
  (defer-until! (boundp '+file-templates-alist)
    (+file-templates--set pred plist)))

;;;###autodef
(defun set-file-templates! (&rest templates)
  "Like `set-file-template!', but can register multiple file templates at once.

\(fn &rest (PRED &key WHEN TRIGGER MODE PROJECT IGNORE))"
  (defer-until! (boundp '+file-templates-alist)
    (dolist (template templates)
      (+file-templates--set (car template) (cdr template)))))


;;
;;; Library

;;;###autoload
(cl-defun +file-templates--expand (pred &key project mode trigger ignore _when)
  "Auto insert a yasnippet snippet into current file and enter insert mode (if
evil is loaded and enabled)."
  (when (and pred (not ignore))
    (when (if project (doom-project-p) t)
      (unless mode
        (setq mode
              (if (and (symbolp pred) (not (booleanp pred)))
                  pred
                major-mode)))
      (unless mode
        (user-error "Couldn't determine mode for %s file template" pred))
      (unless trigger
        (setq trigger +file-templates-default-trigger))
      (if (functionp trigger)
          (funcall trigger)
        (require 'yasnippet)
        (unless yas-minor-mode
          (yas-minor-mode-on))
        (when (and yas-minor-mode
                   (when-let
                       (template (cl-find trigger (yas--all-templates (yas--get-snippet-tables mode))
                                          :key #'yas--template-key :test #'equal))
                     (yas-expand-snippet (yas--template-content template)))
                   (and (featurep 'evil) evil-local-mode)
                   (and yas--active-field-overlay
                        (overlay-buffer yas--active-field-overlay)
                        (overlay-get yas--active-field-overlay 'yas--field)))
          (evil-initialize-state 'insert))))))

;;;###autoload
(defun +file-templates-get-short-path ()
  "Fetches a short file path for the header in Doom module templates."
  (let ((path (file-truename (or buffer-file-name default-directory))))
    (save-match-data
      (cond ((string-match "/modules/\\(.+\\)$" path)
             (match-string 1 path))
            ((file-in-directory-p path doom-emacs-dir)
             (file-relative-name path doom-emacs-dir))
            ((file-in-directory-p path doom-private-dir)
             (file-relative-name path doom-private-dir))
            ((abbreviate-file-name path))))))


;;
;;; Commands

;;;###autoload
(defun +file-templates/insert-license ()
  "Insert a license file template into the current file."
  (interactive)
  (require 'yasnippet)
  (unless (gethash 'text-mode yas--tables)
    (yas-reload-all t))
  (let ((templates
         (let (yas-choose-tables-first ; avoid prompts
               yas-choose-keys-first)
           (cl-loop for tpl in (yas--all-templates (yas--get-snippet-tables 'text-mode))
                    for uuid = (yas--template-uuid tpl)
                    if (string-prefix-p "__license-" uuid)
                    collect (cons (string-remove-prefix "__license-" uuid) tpl)))))
    (when-let (uuid (yas-choose-value (mapcar #'car templates)))
      (yas-expand-snippet (cdr (assoc uuid templates))))))

;;;###autoload
(defun +file-templates/debug ()
  "Tests the current buffer and outputs the file template rule most appropriate
for it. This is used for testing."
  (interactive)
  (cl-destructuring-bind (pred &rest plist &key trigger mode &allow-other-keys)
      (or (cl-find-if #'+file-template-p +file-templates-alist)
          (user-error "Found no file template for this file"))
    (if (or (functionp trigger)
            (cl-find trigger
                     (yas--all-templates
                      (yas--get-snippet-tables
                       mode))
                     :key #'yas--template-key :test #'equal))
        (message "Found %s" (cons pred plist))
      (message "Found rule, but can't find associated snippet: %s" (cons pred plist)))))
