;;; core/cli/debugger.el -*- lexical-binding: t; -*-

(cl-defun doom-cli--debugger (error data)
  (cl-incf num-nonmacro-input-events)
  (cl-destructuring-bind (backtrace &optional type data . rest)
      (cons (doom-cli--backtrace) data)
    (with-output-to! doom--cli-log-buffer
      (let ((straight-error
             (and (bound-and-true-p straight-process-buffer)
                  (stringp data)
                  (string-match-p (regexp-quote straight-process-buffer)
                                  data)
                  (with-current-buffer (straight--process-buffer)
                    (split-string (buffer-string) "\n" t)))))
        (cond (straight-error
               (print! (error "The package manager threw an error"))
               (print! (error "Last 25 lines of straight's error log:"))
               (print-group!
                (print!
                 "%s" (string-join
                       (seq-subseq straight-error
                                   (max 0 (- (length straight-error) 25))
                                   (length straight-error))
                       "\n"))))
              ((print! (error "There was an unexpected error"))
               (print-group!
                (print! "%s %s" (bold "Message:") (get type 'error-message))
                (print! "%s %S" (bold "Error:") (append (list type data) rest))
                (when backtrace
                  (print! (bold "Backtrace:"))
                  (print-group!
                   (dolist (frame (seq-take backtrace 10))
                     (let* ((frame (replace-regexp-in-string
                                    "[\n\r]" "\\\\n" (prin1-to-string frame)))
                            (frame (if (> (length frame) 74)
                                       (concat (substring frame 0 74) "...")
                                     frame)))
                       (print! "%s" frame))))))))
        (when backtrace
          (with-temp-file doom-cli-log-error-file
            (insert "# -*- lisp-interaction -*-\n")
            (insert "# vim: set ft=lisp:\n")
            (let ((standard-output (current-buffer))
                  (print-quoted t)
                  (print-escape-newlines t)
                  (print-escape-control-characters t)
                  (print-level nil)
                  (print-circle nil))
              (when straight-error
                (print (string-join straight-error "\n")))
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
