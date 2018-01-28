;;; lang/org/+babel.el -*- lexical-binding: t; -*-

(add-hook 'org-load-hook #'+org|init-babel)

(defun +org|init-babel ()
  (setq org-src-fontify-natively t      ; make code pretty
        org-src-preserve-indentation t  ; use native major-mode indentation
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate nil) ; you don't need my permission

  (defun +org*babel-execute-src-block (orig-fn &rest args)
    "Load babel libraries as needed when babel blocks are executed."
    (let* ((language (org-element-property :language (org-element-at-point)))
           (lang-sym (intern language)))
      (unless (cdr (assoc lang-sym org-babel-load-languages))
        (require (intern (format "ob-%s" language)))
        (add-to-list 'org-babel-load-languages (cons lang-sym t)))
      (apply orig-fn args)))
  (advice-add #'org-babel-execute-src-block :around #'+org*babel-execute-src-block)

  ;; I prefer C-c C-c for confirming over the default C-c '
  (map! :map org-src-mode-map "C-c C-c" #'org-edit-src-exit)

  ;; In a recent update, `org-babel-get-header' was removed from org-mode, which
  ;; is something a fair number of babel plugins use. So until those plugins
  ;; update, this polyfill will do:
  (defun org-babel-get-header (params key &optional others)
    (cl-loop with fn = (if others #'not #'identity)
             for p in params
             if (funcall fn (eq (car p) key))
             collect p)))
