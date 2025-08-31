;;; lang/javascript/autoload.el -*- lexical-binding: t; -*-

(defvar +javascript-npm-conf (make-hash-table :test 'equal))

;;;###autoload
(defun +javascript-npm-conf (&optional project-root refresh-p)
  "Retrieve an alist of this project's package.json.

If REFRESH-P is non-nil ignore the cache. PROJECT-ROOT determines where to look
for the package.json file, and defaults to the current buffer's project root."
  (let ((project-root (or project-root (doom-project-root))))
    (or (and (not refresh-p)
             (gethash project-root +javascript-npm-conf))
        (let ((package-file (expand-file-name "package.json" project-root)))
          (when-let (json (and (file-exists-p package-file)
                               (require 'json)
                               (json-read-file package-file)))
            (puthash project-root json +javascript-npm-conf))))))

;;;###autoload
(defun +javascript-npm-dep-p (packages &optional project-root refresh-p)
  "Return non-nil if PACKAGES are dependencies of the NPM project at PROJECT-ROOT.

This value is cached unless REFRESH-P is non-nil. If PROJECT-ROOT is omitted,
the current buffer's project root is used."
  (when-let (data (and (bound-and-true-p +javascript-npm-mode)
                       (+javascript-npm-conf project-root refresh-p)))
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
(defun +javascript-add-npm-path-h ()
  "Add node_modules/.bin to `exec-path'."
  (when-let ((search-directory (or (doom-project-root) default-directory))
             (node-modules-parent (locate-dominating-file search-directory "node_modules/"))
             (node-modules-dir (expand-file-name "node_modules/.bin/" node-modules-parent)))
    (make-local-variable 'exec-path)
    (add-to-list 'exec-path node-modules-dir)
    (doom-log ":lang:javascript: add %s to $PATH" (expand-file-name "node_modules/" node-modules-parent))))


;;
;; Commands

;;;###autoload
(defun +javascript/open-repl ()
  "Open a Javascript REPL via `nodejs-repl'."
  (interactive)
  (nodejs-repl)
  (current-buffer))
