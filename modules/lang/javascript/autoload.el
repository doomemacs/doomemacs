;;; lang/javascript/autoload.el

;;;###autoload
(defun +javascript/install ()
  "Installs NodeJS and tern."
  (interactive)
  (pcase (doom-system-os)
    ('arch
     (unless (and (executable-find "node")
                  (executable-find "npm"))
       (doom-sudo "pacman --needed --noconfirm -S nodejs npm")))
    ('debian) ;; TODO
    ('macos
     (unless (executable-find "node")
       (doom-sh "brew install node")))
    (_ (error "Not a supported OS")))
  (unless (executable-find "node")
    (error "node isn't installed, run `+javascript/install' again"))
  (unless (executable-find "tern")
    (sh "npm -g install tern")))

(defvar +javascript-npm-conf (make-hash-table :test 'equal))

;;;###autoload
(defun +javascript-npm-conf (&optional project-root refresh-p)
  "Retrieves an alist of this project's 'package.json'. If REFRESH-P is non-nil
ignore the cache."
  (let ((project-root (or project-root (doom-project-root))))
    (or (and (not refresh-p)
             (gethash project-root +javascript-npm-conf))
        (let ((package-file (expand-file-name "package.json" project-root)))
          (when-let (json (and (file-exists-p package-file)
                               (json-read-file package-file)))
            (puthash project-root json +javascript-npm-conf))))))

