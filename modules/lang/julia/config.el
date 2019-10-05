;;; lang/julia/config.el -*- lexical-binding: t; -*-

(use-package! julia-mode
  :interpreter "julia"
  :config
  (set-repl-handler! 'julia-mode #'+julia/open-repl)

  ;; Borrow matlab.el's fontification of math operators
  ;; From <https://ogbe.net/emacsconfig.html>
  (dolist (mode '(julia-mode ess-julia-mode))
    (font-lock-add-keywords
     mode
     `((,(let ((OR "\\|"))
           (concat "\\(" ;; stolen `matlab.el' operators first
                   "[<>!]=?" OR
                   "\\.[/*^']" OR
                   "==" OR
                   "=>" OR
                   "\\<xor\\>" OR
                   "[-+*\\/^&|$]=?" OR ;; this has to come before next (updating operators)
                   "[-!^&|*+\\/~:]" OR
                   ;; more extra julia operators follow
                   "[%$]" OR
                   ;; bitwise operators
                   ">>>" OR ">>" OR "<<" OR
                   ">>>=" OR ">>" OR "<<" OR
                   ;; comparison
                   "[<>!]=?" OR
                   "\\)"))
        1 font-lock-type-face)))))


(after! julia-repl
  (add-hook 'julia-repl-hook #'julia-repl-use-emacsclient))
