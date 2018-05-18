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


;;
;; Commands
;;

;;;###autoload
(defun +javascript/repl ()
  "Open a Javascript REPL. Meaning either `skewer-repl', if any of the
skewer-*-mode's are enabled, or `nodejs-repl' otherwise."
  (interactive)
  (call-interactively
   (if (and (featurep 'skewer-mode)
            (or skewer-mode skewer-css-mode skewer-html-mode))
       #'skewer-repl
     #'nodejs-repl)))

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


;;
;; Hooks
;;

;;;###autoload
(defun +javascript|add-node-modules-path ()
  "Search the current buffer's parent directories for `node_modules/.bin`.
If it's found, then add it to the `exec-path'."
  (if-let* ((root (locate-dominating-file
                   (or (buffer-file-name) default-directory)
                   "node_modules"))
            (path (expand-file-name "node_modules/.bin/" root)))
      (progn
        (make-local-variable 'exec-path)
        (cl-pushnew path exec-path :test #'string=)
        (when doom-debug-mode
          (message "Added %s to exec-path" path)))
    (when doom-debug-mode
      (message "node_modules not found in %s" root))))

;;;###autoload
(defun +javascript|cleanup-tide-processes ()
  "Clean up dangling tsserver processes if there are no more buffers with
`tide-mode' active that belong to that server's project."
  (when tide-mode
    (unless (cl-loop with project-name = (tide-project-name)
                     for buf in (delq (current-buffer) (buffer-list))
                     if (and (buffer-local-value 'tide-mode buf)
                             (with-current-buffer buf
                               (string= (tide-project-name) project-name)))
                     return buf)
      (kill-process (tide-current-server)))))


;;
;; Advice
;;

;;;###autoload
(defun +javascript*tide-project-root ()
  "Resolve to `doom-project-root' if `tide-project-root' fails."
  (or tide-project-root
      (or (locate-dominating-file default-directory "tsconfig.json")
          (locate-dominating-file default-directory "jsconfig.json"))
      (doom-project-root 'nocache)))
