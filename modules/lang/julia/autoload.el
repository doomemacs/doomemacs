;;; lang/julia/autoload.el -*- lexical-binding: t; -*-

;; `ob-julia' needs this variable to be defined, but it's defined in
;; `ess-custom', which won't be available if you're using :lang julia and not
;; :lang ess.
;;;###autoload (defvar inferior-julia-program-name (or (executable-find "julia-basic") "julia"))

;;;###autoload
(defun +julia/open-repl ()
  "Run an inferior instance of `julia' inside Emacs."
  (interactive)
  (if (require 'julia-repl nil t)
      (prog1 (julia-repl)
        (julia-repl-use-emacsclient))
    (let ((buffer (get-buffer-create "*Julia*")))
      (unless (comint-check-proc "*Julia*")
        (apply #'make-comint-in-buffer "Julia" "*Julia*" julia-program julia-arguments))
      (pop-to-buffer buffer)
      (with-current-buffer buffer
        (inferior-julia-mode))
      buffer)))
