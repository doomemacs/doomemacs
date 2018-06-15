;;; lang/julia/config.el -*- lexical-binding: t; -*-

(use-package julia-mode
  :interpreter "julia"
  :config
  (set-repl-handler! 'julia-mode #'+julia/repl)

  ;; Borrow matlab.el's fontification of math operators
  ;; From <https://ogbe.net/emacsconfig.html>
  (font-lock-add-keywords
   'julia-mode
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
      1 font-lock-type-face))))

