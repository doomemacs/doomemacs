;;; lang/racket/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +racket/open-repl ()
  "Open the Racket REPL."
  (interactive)
  (pop-to-buffer
   (or (get-buffer "*Racket REPL*")
       (progn (racket-run-and-switch-to-repl)
              (let ((buf (get-buffer "*Racket REPL*")))
                (bury-buffer buf)
                buf)))))

;;;###autoload
(defun +racket-lookup-documentation (thing)
  "A `+lookup/documentation' handler for `racket-mode' and `racket-xp-mode'."
  (let ((buf (if racket-xp-mode
                 (racket-xp-describe thing)
               (racket-repl-describe thing))))
    (when buf
      (pop-to-buffer buf)
      t)))

;;;###autoload
(defun +racket-lookup-definition (_thing)
  "A `+lookup/definition' handler for `racket-mode' and `racket-xp-mode'."
  (call-interactively
   (if racket-xp-mode
       #'racket-xp-visit-definition
     #'racket-repl-visit-definition)))
