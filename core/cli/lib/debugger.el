;;; core/cli/debugger.el -*- lexical-binding: t; -*-

(cl-defun doom-cli--debugger (error data)
  (cl-incf num-nonmacro-input-events)
  (cl-destructuring-bind (backtrace &optional type data . _)
      (cons (doom-cli--backtrace) data)
    (with-output-to! doom--cli-log-buffer
      (let ((straight-error-p
             (and (bound-and-true-p straight-process-buffer)
                  (string-match-p (regexp-quote straight-process-buffer)
                                  (or (get type 'error-message) "")))))
        (cond (straight-error-p
               (print! (error "There was an unexpected package error"))
               (when-let (output (straight--process-get-output))
                 (print-group!
                  (print! "%s" (string-trim output)))))
              ((print! (error "There was an unexpected error"))
               (print-group!
                (print! "%s %s" (bold "Message:") (get type 'error-message))
                (print! "%s %S" (bold "Data:") (cons type data))
                (when backtrace
                  (print! (bold "Backtrace:"))
                  (print-group!
                   (dolist (frame (seq-take backtrace 10))
                     (print!
                      "%0.74s" (replace-regexp-in-string
                                "[\n\r]" "\\\\n"
                                (format "%S" frame)))))))))
        (when backtrace
          (with-temp-file doom-cli-log-error-file
            (insert "# -*- lisp-interaction -*-\n")
            (insert "# vim: set ft=lisp:\n")
            (let ((standard-output doom--cli-log-error-buffer)
                  (print-quoted t)
                  (print-escape-newlines t)
                  (print-escape-control-characters t)
                  (print-level nil)
                  (print-circle nil))
              (when straight-error-p
                (print (string-trim (or (straight--process-get-output) ""))))
              (mapc #'print (cons (list type data) backtrace)))
            (print! (warn "Extended backtrace logged to %s")
                    (relpath doom-cli-log-error-file)))))))
  (throw 'exit 255))

(defun doom-cli--backtrace ()
  (let* ((n 0)
         (frame (backtrace-frame n))
         (frame-list nil)
         (in-program-stack nil))
    (while frame
      (when in-program-stack
        (push (cdr frame) frame-list))
      (when (eq (elt frame 1) 'doom-cli--debugger)
        (setq in-program-stack t))
      (when (and (eq (elt frame 1) 'doom-cli-execute)
                 (eq (elt frame 2) :doom))
        (setq in-program-stack nil))
      (setq n (1+ n)
            frame (backtrace-frame n)))
    (reverse frame-list)))
