;;; lang/org/+babel.el

(add-hook '+org-init-hook '+org|init-babel t)

(defun +org|init-babel ()
  (setq org-confirm-babel-evaluate nil   ; you don't need my permission
        org-src-fontify-natively t       ; make code pretty
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-edit-src-content-indentation 0)

  (org-babel-do-load-languages
   'org-babel-load-languages
   (mapcar (lambda (sym) (cons sym t))
           '(calc
             css
             emacs-lisp
             haskell
             js
             latex
             ledger
             lilypond
             lisp
             matlab
             plantuml
             python
             restclient
             ruby
             rust
             sh
             sqlite
             sql-mode
             translate
             )))

  ;; In a recent update, `org-babel-get-header' was removed from org-mode, which
  ;; is something a fair number of babel plugins use. So until those plugins update...
  (defun org-babel-get-header (params key &optional others)
    (delq nil
          (mapcar
           (lambda (p) (when (funcall (if others #'not #'identity) (eq (car p) key)) p))
           params)))

  ;; I prefer C-c C-c for confirming over the default C-c '
  (map! :map org-src-mode-map "C-c C-c" #'org-edit-src-exit)

  ;; I know the keybindings, no need for the header line
  (defun +org|src-mode-remove-header ()
    (when header-line-format (setq header-line-format nil)))
  (add-hook 'org-src-mode-hook '+org|src-mode-remove-header))
