;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; core/test/helpers.el

(eval-and-compile
  (setq doom-interactive-mode 'test)
  (doom-initialize 'force 'noerror)
  (require 'buttercup)
  (setq split-width-threshold 0
        split-height-threshold 0
        window-min-width 0
        window-min-height 0))

;;
;;; Buttercup extensions

(buttercup-define-matcher-for-binary-function :to-equal-file file-equal-p)

(buttercup-define-matcher :to-expand-into (form expected)
  (cl-destructuring-bind (form expected)
      (mapcar #'funcall (list form expected))
    (let ((expanded (macroexpand-1 form)))
      (if (equal expanded expected)
          (cons t (format "Expected `%S' to not expand to `%S'"
                          form expected))
        (cons nil (format "Expected `%S' to not expand to `%S', but got `%S' instead"
                          form expected expanded))))))

(buttercup-define-matcher :to-output (form &optional expected-output)
  (let ((expected-output (and (functionp expected-output)
                              (funcall expected-output)))
        output)
    (with-current-buffer (get-buffer "*Messages*")
      (let ((standard-output (current-buffer))
            (start (point)))
        (let ((inhibit-message t))
          (funcall form))
        (setq output (buffer-substring-no-properties start (point-max)))
        (with-silent-modifications (erase-buffer))))
    (cond ((null expected-output)
           (if (string-empty-p output)
               (cons nil (format "Expected output %S, but got none"
                                 expected-output))
             (cons t (format "Expected no output, but got %S"
                             output))))
          ((not (equal expected-output output))
           (cons nil (format "Expected output %S, but got %S instead"
                             expected-output output)))
          ((cons t (format "Expected to not get %S as output"
                           expected-output))))))

(buttercup-define-matcher :to-contain-items (items expected)
  (cl-destructuring-bind (items expected)
      (mapcar #'funcall (list items expected))
    (if-let (missing (cl-set-difference expected items))
        (cons nil (format "Expected list to contain %S, but it was missing %S"
                          expected missing))
      (cons t (format "Expected list to not contain %S, but it did: %S"
                      expected items)))))


;;
;;; Helper macros

(defmacro insert!! (&rest text)
  "Insert TEXT in buffer, then move cursor to last {0} marker."
  `(progn
     (insert ,@text)
     (when (search-backward "{0}" nil t)
       (replace-match "" t t))))
