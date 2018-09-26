;;; lang/emacs-lisp/autoload.el -*- lexical-binding: t; -*-

;;
;; Library

;;;###autoload
(defun +emacs-lisp-eval (beg end)
  "Evaluate a region and print it to the echo area (if one line long), otherwise
to a pop up buffer."
  (require 'pp)
  (let ((result
         (let ((debug-on-error t))
           (eval (read
                  (concat "(progn "
                          (buffer-substring-no-properties beg end)
                          "\n)"))
                 t)))
        (buf (get-buffer-create "*doom eval*"))
        (inhibit-read-only t))
    (with-current-buffer buf
      (read-only-mode +1)
      (erase-buffer)
      (setq-local scroll-margin 0)
      (let (emacs-lisp-mode-hook)
        (emacs-lisp-mode))
      (prin1 result buf)
      (pp-buffer)
      (let ((lines (count-lines (point-min) (point-max))))
        (if (> lines 1)
            (save-selected-window
              (pop-to-buffer buf)
              (with-current-buffer buf
                (goto-char (point-min))))
          (message "%s" (buffer-substring (point-min) (point-max)))
          (kill-buffer buf))))))

(defvar +emacs-lisp--face nil)
;;;###autoload
(defun +emacs-lisp-highlight-vars-and-faces (end)
  "Match defined variables and functions.

Functions are differentiated into special forms, built-in functions and
library/userland functions"
  (catch 'matcher
    (while (re-search-forward "\\_<.+?\\_>" end t)
      (unless (save-excursion
                (let ((ppss (syntax-ppss)))
                  (or (nth 3 ppss) (nth 4 ppss))))
        (let ((symbol (intern-soft (match-string-no-properties 0))))
          (and (cond ((null symbol) nil)
                     ((eq symbol t) nil)
                     ((special-variable-p symbol)
                      (setq +emacs-lisp--face 'font-lock-variable-name-face))
                     ((and (fboundp symbol)
                           (eq (char-before (match-beginning 0)) ?\())
                      (let ((unaliased (indirect-function symbol)))
                        (unless (or (macrop unaliased)
                                    (special-form-p unaliased))
                          (let (unadvised)
                            (while (not (eq (setq unadvised (ad-get-orig-definition unaliased))
                                            (setq unaliased (indirect-function unadvised)))))
                            unaliased)
                          (setq +emacs-lisp--face
                                (if (subrp unaliased)
                                    'font-lock-constant-face
                                  'font-lock-function-name-face))))))
               (throw 'matcher t)))))
    nil))

;; `+emacs-lisp-highlight-vars-and-faces' is a potentially expensive function
;; and should be byte-compiled, no matter what, to ensure it runs as fast as
;; possible:
(when (not (byte-code-function-p (symbol-function '+emacs-lisp-highlight-vars-and-faces)))
  (with-no-warnings
    (byte-compile #'+emacs-lisp-highlight-vars-and-faces)))


;;
;; Commands

;;;###autoload
(defun +emacs-lisp/repl ()
  "Open the Emacs Lisp REPL (`ielm')."
  (interactive)
  (pop-to-buffer
   (or (get-buffer "*ielm*")
       (progn (ielm)
              (let ((buf (get-buffer "*ielm*")))
                (bury-buffer buf)
                buf)))))


;;
;; Hooks

;;;###autoload
(defun +emacs-lisp|extend-imenu ()
  "Improve imenu support with better expression regexps and Doom-specific forms."
  (setq imenu-generic-expression
        '(("Evil Commands" "^\\s-*(evil-define-\\(?:command\\|operator\\|motion\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
          ("Unit tests" "^\\s-*(\\(?:ert-deftest\\|describe\\) +\"\\([^\")]+\\)\"" 1)
          ("Package" "^\\s-*(\\(?:def-\\)?package! +\\(\\_<[^ ()\n]+\\_>\\)" 1)
          ("Settings" "^\\s-*(def-setting! +\\([^ ()\n]+\\)" 1)
          ("Major modes" "^\\s-*(define-derived-mode +\\([^ ()\n]+\\)" 1)
          ("Modelines" "^\\s-*(def-modeline! +\\([^ ()\n]+\\)" 1)
          ("Modeline Segments" "^\\s-*(def-modeline-segment! +\\([^ ()\n]+\\)" 1)
          ("Advice" "^\\s-*(def\\(?:\\(?:ine-\\)?advice\\))")
          ("Modes" "^\\s-*(define-\\(?:global\\(?:ized\\)?-minor\\|generic\\|minor\\)-mode +\\([^ ()\n]+\\)" 1)
          ("Macros" "^\\s-*(\\(?:cl-\\)?def\\(?:ine-compile-macro\\|macro\\) +\\([^ )\n]+\\)" 1)
          ("Inline Functions" "\\s-*(\\(?:cl-\\)?defsubst +\\([^ )\n]+\\)" 1)
          ("Functions" "^\\s-*(\\(?:cl-\\)?def\\(?:un\\|un\\*\\|method\\|generic\\|-memoized!\\) +\\([^ ,)\n]+\\)" 1)
          ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\|var\\(?:-local\\)?\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
          ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2))))

;;;###autoload
(defun +emacs-lisp|disable-flycheck-maybe ()
  "Disable flycheck-mode if in emacs.d."
  (when (and (bound-and-true-p flycheck-mode)
             (eq major-mode 'emacs-lisp-mode)
             (or (not buffer-file-name)
                 (cl-loop for dir in (list doom-emacs-dir doom-private-dir)
                          if (file-in-directory-p buffer-file-name dir)
                          return t)))
    (flycheck-mode -1)))
