;;; editor/snippets/autoload/snippets.el -*- lexical-binding: t; -*-

(defun +snippets--remove-p (x y)
  (and (equal (yas--template-key x) (yas--template-key y))
       (file-in-directory-p (yas--template-get-file x) doom-emacs-dir)))

;;;###autoload
(defun +snippets-prompt-private (prompt choices &optional display-fn)
  "Prioritize private snippets over built-in ones if there are multiple
choices.

There are two groups of snippets in Doom Emacs. The built in ones (under
`doom-emacs-dir'; provided by Doom or its plugins) or your private snippets
(outside of `doom-eamcs-dir').

If there are multiple snippets with the same key in either camp (but not both),
you will be prompted to select one.

If there are conflicting keys across the two camps, the built-in ones are
ignored. This makes it easy to override built-in snippets with private ones."
  (when (eq this-command 'yas-expand)
    (let* ((gc-cons-threshold most-positive-fixnum)
           (choices (cl-remove-duplicates choices :test #'+snippets--remove-p)))
      (if (cdr choices)
          (cl-loop for fn in (cdr (memq '+snippets-prompt-private yas-prompt-functions))
                   if (funcall fn prompt choices display-fn)
                   return it)
        (car choices)))))

(defun +snippet--ensure-dir (dir)
  (unless (file-directory-p dir)
    (if (y-or-n-p (format "%S doesn't exist. Create it?" (abbreviate-file-name dir)))
        (make-directory dir t)
      (error "%S doesn't exist" (abbreviate-file-name dir)))))

(defun +snippet--get-template-by-uuid (uuid &optional mode)
  "Look up the template by uuid in child-most to parent-most mode order.
Finds correctly active snippets from parent modes (based on Yas' logic)."
  (cl-loop with mode = (or mode major-mode)
           for active-mode in (yas--modes-to-activate mode)
           if (gethash active-mode yas--tables)
           if (gethash uuid (yas--table-uuidhash it))
           return it))

(defun +snippet--completing-read-uuid (prompt all-snippets &rest args)
  (plist-get
   (text-properties-at
    0 (apply #'completing-read prompt
             (cl-loop for (_ . tpl) in (mapcan #'yas--table-templates (if all-snippets
                                                                          (hash-table-values yas--tables)
                                                                        (yas--get-snippet-tables)))

                      for txt = (format "%-25s%-30s%s"
                                        (yas--template-key tpl)
                                        (yas--template-name tpl)
                                        (abbreviate-file-name (yas--template-load-file tpl)))
                      collect
                      (progn
                        (set-text-properties 0 (length txt) `(uuid ,(yas--template-uuid tpl)
                                                                   path ,(yas--template-load-file tpl))
                                             txt)
                        txt))
             args))
   'uuid))

(defun +snippet--abort ()
  (interactive)
  (set-buffer-modified-p nil)
  (kill-current-buffer))

(defvar +snippet--current-snippet-uuid nil)
(defun +snippet--edit ()
  (interactive)
  (when +snippet--current-snippet-uuid
    (let ((buf (current-buffer)))
      (+snippets/edit +snippet--current-snippet-uuid)
      (kill-buffer buf))))


;;
;;; Commands

;;;###autoload
(defun +snippets/goto-start-of-field ()
  "Go to the beginning of the current field."
  (interactive)
  (let* ((snippet (car (yas-active-snippets)))
         (active-field (yas--snippet-active-field snippet))
         (position (if (yas--field-p active-field)
                       (yas--field-start active-field)
                     -1)))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

;;;###autoload
(defun +snippets/goto-end-of-field ()
  "Go to the end of the current field."
  (interactive)
  (let* ((snippet (car (yas-active-snippets)))
         (active-field (yas--snippet-active-field snippet))
         (position (if (yas--field-p active-field)
                       (yas--field-end active-field)
                     -1)))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

;;;###autoload
(defun +snippets/delete-backward-char (&optional field)
  "Prevents Yas from interfering with backspace deletion."
  (interactive)
  (let ((field (or field (and (overlayp yas--active-field-overlay)
                              (overlay-buffer yas--active-field-overlay)
                              (overlay-get yas--active-field-overlay 'yas--field)))))
    (unless (and (yas--field-p field)
                 (eq (point) (marker-position (yas--field-start field))))
      (call-interactively #'delete-backward-char))))

;;;###autoload
(defun +snippets/delete-forward-char-or-field (&optional field)
  "Delete forward, or skip the current field if it's empty. This is to prevent
buggy behavior when <delete> is pressed in an empty field."
  (interactive)
  (let ((field (or field (and yas--active-field-overlay
                              (overlay-buffer yas--active-field-overlay)
                              (overlay-get yas--active-field-overlay 'yas--field)))))
    (cond ((not (yas--field-p field))
           (delete-char 1))
          ((and (not (yas--field-modified-p field))
                (eq (point) (marker-position (yas--field-start field))))
           (yas--skip-and-clear field)
           (yas-next-field 1))
          ((eq (point) (marker-position (yas--field-end field))) nil)
          ((delete-char 1)))))

;;;###autoload
(defun +snippets/delete-to-start-of-field (&optional field)
  "Delete to start-of-field."
  (interactive)
  (unless field
    (setq field (and (overlayp yas--active-field-overlay)
                     (overlay-buffer yas--active-field-overlay)
                     (overlay-get yas--active-field-overlay 'yas--field))))
  (when (yas--field-p field)
    (let ((sof (marker-position (yas--field-start field))))
      (when (and field (> (point) sof))
        (delete-region sof (point))))))

;;;###autoload
(defun +snippets/find ()
  "Open a snippet file (in all of `yas-snippet-dirs')."
  (interactive)
  (let* ((dirs (doom-files-in (cl-loop for dir in yas-snippet-dirs
                                       if (symbolp dir)
                                       collect (symbol-value dir)
                                       else collect dir)
                              :depth 0 :type 'dirs))
         (files (doom-files-in dirs :depth 0 :full t)))
    (let ((template-path (completing-read "Find snippet: " (mapcar #'abbreviate-file-name files))))
      (unless (file-readable-p template-path)
        (user-error "Cannot read %S" template-path))
      (find-file template-path)
      (unless (file-in-directory-p template-path +snippets-dir)
        (read-only-mode +1)
        (setq header-line-format "This is a built-in snippet. Press C-c C-e to modify it"
              +snippet--current-snippet-uuid template-uuid)))))

;;;###autoload
(defun +snippets/find-private ()
  "Open a private snippet file in `+snippets-dir'."
  (interactive)
  (doom-project-find-file +snippets-dir))

;;;###autoload
(defun +snippets/find-for-current-mode (template-uuid)
  "Open a snippet for this mode."
  (interactive
   (list
    (+snippet--completing-read-uuid "Visit snippet: " current-prefix-arg)))
  (if-let* ((template (+snippet--get-template-by-uuid template-uuid major-mode))
            (template-path (yas--template-load-file template)))
      (progn
        (unless (file-readable-p template-path)
          (user-error "Cannot read %S" template-path))
        (find-file template-path)
        (unless (file-in-directory-p template-path +snippets-dir)
          (read-only-mode +1)
          (setq header-line-format "This is a built-in snippet. Press C-c C-e to modify it"
                +snippet--current-snippet-uuid template-uuid)))
    (user-error "Cannot find template with UUID %S" template-uuid)))

;;;###autoload
(defun +snippets/new ()
  "Create a new snippet in `+snippets-dir'."
  (interactive)
  (let ((default-directory
          (expand-file-name (symbol-name major-mode)
                            +snippets-dir)))
    (+snippet--ensure-dir default-directory)
    (with-current-buffer (switch-to-buffer "untitled-snippet")
      (snippet-mode)
      (erase-buffer)
      (yas-expand-snippet (concat "# -*- mode: snippet -*-\n"
                                  "# name: $1\n"
                                  "# uuid: $2\n"
                                  "# key: ${3:trigger-key}${4:\n"
                                  "# condition: t}\n"
                                  "# --\n"
                                  "$0"))
      (when (bound-and-true-p evil-local-mode)
        (evil-insert-state)))))

;;;###autoload
(defun +snippets/new-alias (template-uuid)
  "Create an alias for a snippet with uuid TEMPLATE-UUID.

You will be prompted for a snippet to alias."
  (interactive
   (list
    (+snippet--completing-read-uuid "Select snippet to alias: "
                                    current-prefix-arg)))
  (unless (require 'doom-snippets nil t)
    (user-error "This command requires the `doom-snippets' library bundled with Doom Emacs"))
  (let ((default-directory (expand-file-name (symbol-name major-mode) +snippets-dir)))
    (+snippet--ensure-dir default-directory)
    (with-current-buffer (switch-to-buffer "untitled-snippet")
      (snippet-mode)
      (erase-buffer)
      (yas-expand-snippet
       (concat "# -*- mode: snippet -*-\n"
               "# name: $1\n"
               "# key: ${2:trigger-key}${3:\n"
               "# condition: t}\n"
               "# type: command\n"
               "# --\n"
               "(%alias \"${4:" (or template-uuid "uuid") "}\")"))
      (when (bound-and-true-p evil-local-mode)
        (evil-insert-state)))))

;;;###autoload
(defun +snippets/edit (template-uuid)
  "Edit a snippet with uuid TEMPLATE-UUID.

If the snippet isn't in `+snippets-dir', it will be copied there (where it will
shadow the default snippet)."
  (interactive
   (list
    (+snippet--completing-read-uuid "Select snippet to edit: "
                                    current-prefix-arg)))
  (if-let* ((major-mode (if (eq major-mode 'snippet-mode)
                            (intern (file-name-base (directory-file-name default-directory)))
                          major-mode))
            (template (+snippet--get-template-by-uuid template-uuid major-mode))
            (template-path (yas--template-load-file template)))
      (if (file-in-directory-p template-path +snippets-dir)
          (find-file template-path)
        (let ((buf (get-buffer-create (format "*%s*" (file-name-nondirectory template-path)))))
          (with-current-buffer (switch-to-buffer buf)
            (insert-file-contents template-path)
            (snippet-mode)
            (setq default-directory
                  (expand-file-name (file-name-nondirectory template-path)
                                    (expand-file-name (symbol-name major-mode)
                                                      +snippets-dir))))))
    (user-error "Couldn't find a snippet with uuid %S" template-uuid)))

;;;###autoload
(defun +snippets-show-hints-in-header-line-h ()
  (setq header-line-format
        (substitute-command-keys
         (concat "\\[yas-load-snippet-buffer-and-close] to finish, "
                 "\\[+snippet--abort] to abort, "
                 "\\[yas-tryout-snippet] to test it"))))


;;
;;; Hooks

;;;###autoload
(defun +snippets-enable-project-modes-h (mode &rest _)
  "Automatically enable snippet libraries for project minor modes defined with
`def-project-mode!'."
  (if (symbol-value mode)
      (yas-activate-extra-mode mode)
    (yas-deactivate-extra-mode mode)))

;;;###autoload
(defun +snippets-read-only-maybe-h ()
  "Enable `read-only-mode' if snippet is built-in."
  (when (file-in-directory-p default-directory doom-local-dir)
    (read-only-mode 1)
    (message "This is a built-in snippet, enabling read only mode. Use `yas-new-snippet' to redefine snippets")))


;;
;;; Advice

;;;###autoload
(defun +snippets-expand-on-region-a (orig-fn &optional no-condition)
  "Fix off-by-one when expanding snippets on an evil visual region.

Also strips whitespace out of selection. Also switches to insert mode. If
`evil-local-mode' isn't enabled, or we're not in visual mode, run ORIG-FN as
is."
  (if (not (and (bound-and-true-p evil-local-mode)
                (evil-visual-state-p)))
      (funcall orig-fn no-condition)
    ;; Trim whitespace in selected region, so as not to introduce extra
    ;; whitespace into `yas-selected-text'.
    (evil-visual-select (save-excursion
                          (goto-char evil-visual-beginning)
                          (skip-chars-forward " \t")
                          (point))
                        (save-excursion
                          (goto-char evil-visual-end)
                          (skip-chars-backward " \t")
                          (point))
                        'inclusive)
    (letf! ((defun region-beginning () evil-visual-beginning)
            (defun region-end () evil-visual-end))
      (funcall orig-fn no-condition)))
  (when (and (bound-and-true-p evil-local-mode)
             (not (or (evil-emacs-state-p)
                      (evil-insert-state-p)))
             (yas-active-snippets))
    (evil-insert-state +1)))
