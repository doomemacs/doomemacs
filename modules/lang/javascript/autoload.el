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

