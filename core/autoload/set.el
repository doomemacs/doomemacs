;;; set.el

(defvar doom-describe-setting-buffer nil
  "Buffer that was current when ‘describe-setting’ was invoked.")

;;;###autoload
(defun doom/describe-setting (setting)
  "Display the full documentation of SETTING (a keyword)."
  (interactive
   (let ((st (symbol-at-point))
         (enable-recursive-minibuffers t)
         val)
     (setq val (completing-read (if st
                                    (format "Describe setting (default %s): " st)
                                  "Describe setting: ")
                                (mapcar (lambda (x) (symbol-name (car x))) doom-settings)
                                'fboundp t nil nil
                                (and st (symbol-name st))))
     (list (if (equal val "")
               st (intern val)))))
  (or (and setting (keywordp setting))
      (user-error "You didn't specify a setting keyword"))
  (or (assq setting doom-settings)
      (user-error "Symbol's setting definition is void: %s" setting))

  (let ((doom-describe-setting-buffer
         (or doom-describe-setting-buffer (current-buffer)))
        (file-name
         (find-lisp-object-file-name 'doom--set:enable 'defun)))
    (help-setup-xref
     (list (lambda (setting buffer)
             (let ((doom-describe-setting-buffer (if (buffer-live-p buffer) buffer)))
               (describe-setting function)))
           setting doom-describe-setting-buffer)
     (called-interactively-p 'interactive))

    (save-excursion
      (with-help-window (help-buffer)
        (prin1 setting)
        ;; Use " is " instead of a colon so that
        ;; it is easier to get out the function name using forward-sexp.
        (princ " is a DOOM Emacs setting, defined in ")
        (princ (format-message " in `%s'" (help-fns-short-filename file-name)))
        (with-current-buffer standard-output
          ;; Return the text we displayed.
          (buffer-string))))))

