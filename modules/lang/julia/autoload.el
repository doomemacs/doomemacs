;;; lang/julia/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +julia/repl ()
  "Run an inferior instance of `julia' inside Emacs."
  (interactive)
  (let ((buffer (get-buffer-create "*Julia*")))
    (unless (comint-check-proc "*Julia*")
      (apply #'make-comint-in-buffer "Julia" "*Julia*" julia-program julia-arguments))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (inferior-julia-mode))))


