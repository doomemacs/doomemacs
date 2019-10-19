;;; core/autoload/cli.el -*- lexical-binding: t; -*-

;; Externs
(defvar evil-collection-mode-list)

;;;###autoload
(defun doom--cli-run (command &rest _args)
  (when (featurep 'general)
    (general-auto-unbind-keys))
  (let* ((evil-collection-mode-list nil)
         (default-directory doom-emacs-dir)
         (buf (get-buffer-create " *bin/doom*"))
         (doom-format-backend 'ansi)
         (ignore-window-parameters t)
         (noninteractive t)
         (standard-output
          (lambda (char)
            (with-current-buffer buf
              (insert char)
              (when (memq char '(?\n ?\r))
                (ansi-color-apply-on-region (line-beginning-position -1) (line-end-position))
                (redisplay))))))
    (doom-initialize t)
    (setq doom-modules (doom-modules))
    (doom-initialize-modules t)
    (doom-initialize-packages t)
    (with-current-buffer (switch-to-buffer buf)
      (erase-buffer)
      (require 'package)
      (redisplay)
      (doom-dispatch command nil)
      (print! (green "\nDone!"))))
  (when (featurep 'general)
    (general-auto-unbind-keys 'undo))
  (message (format! (green "Done!"))))


;;;###autoload
(defun doom//autoloads (&optional yes)
  "TODO"
  (interactive "P")
  (let ((doom-auto-accept yes))
    (doom--cli-run "autoloads")))

;;;###autoload
(defun doom//update (&optional yes)
  "TODO"
  (interactive "P")
  (let ((doom-auto-accept yes))
    (doom--cli-run "update")))

;;;###autoload
(defun doom//upgrade (&optional yes)
  "TODO"
  (interactive "P")
  (let ((doom-auto-accept yes))
    (doom--cli-run "upgrade"))
  (when (y-or-n-p "You must restart Emacs for the upgrade to take effect. Restart?")
    (doom/restart-and-restore)))

;;;###autoload
(defun doom//install (&optional yes)
  "TODO"
  (interactive "P")
  (let ((doom-auto-accept yes))
    (doom--cli-run "install")))

;;;###autoload
(defun doom//autoremove (&optional yes)
  "TODO"
  (interactive "P")
  (let ((doom-auto-accept yes))
    (doom--cli-run "autoremove")))

;;;###autoload
(defun doom//refresh (&optional yes)
  "TODO"
  (interactive "P")
  (let ((doom-auto-accept yes))
    (doom--cli-run "refresh")))



;;
;;; Library

;;;###autoload
(defun doom-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
code of the process and OUTPUT is its stdout output."
  (with-temp-buffer
    (cons (or (apply #'call-process command nil t nil args)
              -1)
          (string-trim (buffer-string)))))

;;;###autoload
(defun doom-exec-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Unlike `doom-call-process', this pipes output to `standard-output' on the fly to
simulate 'exec' in the shell, so batch scripts could run external programs
synchronously without sacrificing their output.

Warning: freezes indefinitely on any stdin prompt."
  ;; FIXME Is there any way to handle prompts?
  (with-temp-buffer
    (cons (let ((process
                 (make-process :name "doom-sh"
                               :buffer (current-buffer)
                               :command (cons command args)
                               :connection-type 'pipe))
                done-p)
            (set-process-filter
             process (lambda (process output)
                       (princ output (current-buffer))
                       (princ output)))
            (set-process-sentinel
             process (lambda (process _event)
                       (when (memq (process-status process) '(exit stop))
                         (setq done-p t))))
            (while (not done-p)
              (sit-for 0.1))
            (process-exit-status process))
          (string-trim (buffer-string)))))

(defun doom--cli-normalize (args specs)
  (let* ((args (cl-remove-if-not #'stringp args))
         (optspec (cl-remove-if-not #'listp specs))
         (argspec (cl-remove-if #'listp specs))
         (options (mapcar #'list (mapcar #'car-safe optspec)))
         extra
         arguments)
    (dolist (spec optspec)
      (setf (nth 1 spec) (doom-enlist (nth 1 spec))))
    (while args
      (let ((arg (pop args)))
        (cl-check-type arg string)
        (if (not (string-prefix-p "-" arg))
            (push arg arguments)
          (if-let (specs (cl-remove-if-not
                          (if (string-prefix-p "--" arg)
                              (doom-partial #'member arg)
                            (lambda (flags)
                              (cl-loop for switch in (split-string (string-remove-prefix "-" arg) "" t)
                                       if (member (concat "-" switch) flags)
                                       return t)))
                          optspec
                          :key #'cadr))
              (pcase-dolist (`(,sym ,flags ,type) specs)
                (setf (alist-get sym options)
                      (list
                       (let ((value (if type (pop args))))
                         (pcase type
                           (`&string value)
                           (`&int `(truncate (read ,value)))
                           (`&float `(float (read ,value)))
                           (`&path `(expand-file-name ,value))
                           (`&directory
                            `(let ((path (expand-file-name ,value)))
                               (unless (file-directory-p path)
                                 (error "Directory does not exist: %s" path))
                               path))
                           (`&file
                            `(let ((path (expand-file-name ,value)))
                               (unless (file-exists-p path)
                                 (error "File does not exist: %s" path))
                               path))
                           (`&sexp `(read ,value))
                           ((or `nil `t) arg)
                           (_ (error "Not a valid type: %S" type)))))))
            (push arg extra)))))
    (list optspec (nreverse options)
          argspec (nreverse arguments))))

;;;###autoload
(defun doom-cli-getopts (args specs)
  "TODO"
  (cl-destructuring-bind (optspec options argspec arguments)
      (doom--cli-normalize args specs)
    (let ((i 0)
          optional-p
          noerror-p)
      (cl-dolist (spec argspec)
        (cond ((eq spec '&rest)
               (push (list (cadr (member '&rest specs))
                           `(quote
                             ,(reverse
                               (butlast (reverse arguments) i))))
                     options)
               (cl-return))
              ((eq spec '&all)
               (push (list (cadr (member '&all specs))
                           `(quote ,args))
                     options))
              ((eq spec '&noerror) (setq noerror-p t))
              ((eq spec '&optional) (setq optional-p t))
              ((and (>= i (length arguments)) (not optional-p))
               (signal 'wrong-number-of-arguments
                       (list argspec (length arguments))))
              ((push (list spec (nth i arguments)) options)
               (cl-incf i)))))
    (nreverse options)))

;;;###autoload
(defmacro let-cliopts! (args spec &rest body)
  "Run BODY with command line ARGS parsed according to SPEC."
  (declare (indent 2))
  `(eval (append (list 'let (doom-cli-getopts ,args ',spec))
                 (quote ,body))
         t))
