;;; private/exwm/autoload/exwm-edit.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +exwm-edit-default-major-mode 'org-mode
  "The major mode used when a programming language is not detected
after running `exwm-edit--compose'.")

;;;###autoload
(defvar +exwm-edit-activate-appropriate-major-mode--timer nil
  "A variable used internally to store a timer object.")

;;;###autoload
(defun +exwm-edit-activate-appropriate-major-mode ()
  "Detects what programming language (if any) is present in the
application's input field and enables the appropriate major mode."
  (setq exwm-edit-activate-appropriate-major-mode--timer
        (run-at-time
         0.01 0.01
         (defun exwm-edit-activate-appropriate-major-mode--timer-fn (&rest _)
           (unless (string-prefix-p "*exwm-edit "
                                    (buffer-name))
             (cancel-timer exwm-edit-activate-appropriate-major-mode--timer))
           (when (buffer-modified-p)
             (cancel-timer exwm-edit-activate-appropriate-major-mode--timer)
             (let ((header-line-format--old header-line-format))
               (when (string-prefix-p "*exwm-edit "
                                      (buffer-name))
                 (cl-case (language-detection-buffer)
                   (ada (ada-mode))
                   (awk (awk-mode))
                   (c (c-mode))
                   (cpp (c++-mode))
                   (clojure (clojure-mode))
                   (csharp (csharp-mode))
                   (css (css-mode))
                   (dart (dart-mode))
                   (delphi (delphi-mode))
                   (emacslisp (emacs-lisp-mode))
                   (erlang (erlang-mode))
                   (fortran (fortran-mode))
                   (fsharp (fsharp-mode))
                   (go (go-mode))
                   (groovy (groovy-mode))
                   (haskell (haskell-mode))
                   (html (html-mode))
                   (java (java-mode))
                   (javascript (javascript-mode))
                   (json (json-mode))
                   (latex (latex-mode))
                   (lisp (lisp-mode))
                   (lua (lua-mode))
                   (matlab (matlab-mode))
                   (objc (objc-mode))
                   (perl (perl-mode))
                   (php (php-mode))
                   (prolog (prolog-mode))
                   (python (python-mode))
                   (r r-(mode))
                   (ruby (ruby-mode))
                   (rust (rust-mode))
                   (scala (scala-mode))
                   (shell (shell-script-mode))
                   (smalltalk (smalltalk-mode))
                   (sql (sql-mode))
                   (swift (swift-mode))
                   (visualbasic (visual-basic-mode))
                   (xml (sgml-mode))
                   (t (funcall +exwm-edit-default-major-mode)))
                 (setq header-line-format header-line-format--old))))))))
