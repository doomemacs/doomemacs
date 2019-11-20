;;; lang/scala/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +scala-comment-indent-new-line (&optional _)
  "Continue the commnt on the current line.

Meant to be used for `scala-mode's `comment-line-break-function'."
  (let* ((state (syntax-ppss))
         (comment-start-pos (nth 8 state)))
    (save-match-data
      (cond ((and (integerp (nth 4 state))
                  ;; Ensure that we're inside a scaladoc comment
                  (string-match-p "^/\\*\\*?[^\\*]?"
                                  (buffer-substring-no-properties
                                   comment-start-pos
                                   (min (+ comment-start-pos 4)
                                        (point-max))))
                  (progn
                    (setq prev-line (buffer-substring-no-properties
                                     (line-beginning-position 0)
                                     (line-end-position 0)))
                    (or (string-match "^\\s-*\\*" prev-line)
                        (string-match "\\s-*/*" prev-line))))
             (newline nil t)
             (indent-according-to-mode)
             (insert (make-string (max 0 (- (1- (match-end 0))
                                            (match-beginning 0)))
                                  ? )
                     "*")
             (scala-indent:indent-on-scaladoc-asterisk))
            ((nth 4 state) ; for line comments
             (call-interactively #'comment-indent-new-line))
            (t
             (newline nil t)
             (indent-according-to-mode))))))

;;;###autoload
(defun +scala/open-repl ()
  "Open a scala repl. Uses `run-scala' if in a sbt project."
  (interactive)
  (if (and (require 'sbt-mode nil t)
           (sbt:find-root))
      (run-scala)
    (let ((buffer-name "*scala-repl")
          buffer)
      (unless (comint-check-proc buffer-name)
        (setq buffer (make-comint-in-buffer
                      "scala-repl" buffer-name "scala")))
      (display-buffer buffer)
      buffer)))
