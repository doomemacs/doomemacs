;;; lang/javascript/autoload.el -*- lexical-binding: t; -*-

(defvar +javascript-npm-conf (make-hash-table :test 'equal))

;;;###autoload
(defun +javascript-npm-conf (&optional project-root refresh-p)
  "Retrieves an alist of this project's 'package.json'. If REFRESH-P is non-nil
ignore the cache."
  (let ((project-root (or project-root (doom-project-root))))
    (or (and (not refresh-p)
             (gethash project-root +javascript-npm-conf))
        (let ((package-file (expand-file-name "package.json" project-root)))
          (when-let* ((json (and (file-exists-p package-file)
                                 (require 'json)
                                 (json-read-file package-file))))
            (puthash project-root json +javascript-npm-conf))))))

;;;###autoload
(defun +javascript-npm-dep-p (packages &optional project-root refresh-p)
  (when-let* ((data (and (bound-and-true-p +javascript-npm-mode)
                         (+javascript-npm-conf project-root refresh-p))))
    (let ((deps (append (cdr (assq 'dependencies data))
                        (cdr (assq 'devDependencies data)))))
      (cond ((listp packages)
             (funcall (if (eq (car packages) 'and)
                          #'cl-every
                        #'cl-some)
                      (lambda (pkg) (assq pkg deps))
                      (if (listp packages) packages (list packages))))
            ((symbolp packages)
             (assq packages deps))
            (t (error "Expected a package symbol or list, got %s" packages))))))

;;;###autoload
(defun +javascript/repl ()
  "Open a Javascript REPL. Meaning either `skewer-repl', if any of the
skewer-*-mode's are enabled, or `nodejs-repl' otherwise."
  (interactive)
  (call-interactively
   (if (and (featurep 'skewer-mode)
            (or skewer-mode skewer-css-mode skewer-html-mode))
       'skewer-repl
     'nodejs-repl)))

;;;###autoload
(defun +javascript/skewer-this-buffer ()
  "Toggle a globalized skewer-mode, attaching an external browser (once),
initiating an internal httpd server (once) and enabling the appropriate
skewer-mode for the current buffer.

Run this for any buffer you want to skewer."
  (interactive)
  (when (bound-and-true-p impatient-mode)
    (error "Skewer-mode isn't compatible with impatient mode"))
  (require 'skewer-mode)
  (unless (process-status "httpd")
    (run-skewer))
  (unless (and skewer-mode skewer-css-mode skewer-html-mode)
    (pcase major-mode
      ((or 'css-mode 'scss-mode 'less-css-mode) (skewer-css-mode +1))
      ((or 'web-mode 'html-mode) (skewer-html-mode +1))
      ('js2-mode (skewer-mode +1))
      (_ (error "Invalid mode %s" major-mode)))))

;;;###autoload
(defun +javascript/skewer-cleanup ()
  "Disable skewer-mode globally and disable the httpd server."
  (interactive)
  (when (process-status "httpd")
    (httpd-stop))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (if skewer-mode (skewer-mode -1))
      (if skewer-css-mode (skewer-css-mode -1))
      (if skewer-html-mode (skewer-html-mode -1)))))

;;;###autoload
(defun add-node-modules-path ()
  "Search the current buffer's parent directories for `node_modules/.bin`.
If it's found, then add it to the `exec-path'."
(defvar add-node-modules-path-debug nil)
  (interactive)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (path (and root
                    (expand-file-name "node_modules/.bin/" root))))
    (if root
        (progn
          (make-local-variable 'exec-path)
          (add-to-list 'exec-path path)
          (when add-node-modules-path-debug
            (message (concat "added " path  " to exec-path"))))
      (when add-node-modules-path-debug
        (message (concat "node_modules not found in " root))))))

;;;###autoload
(defun set-up-tide-mode ()
  (interactive)
  (tide-setup)
  (if company-mode (push 'company-tide company-backends))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  )
