;;; private/hlissner/autoload/hlissner.el

;;;###autoload
(defun +hlissner/install-snippets ()
  "Install my snippets from https://github.com/hlissner/emacs-snippets into
private/hlissner/snippets."
  (interactive)
  (bs-fetch :github "hlissner/emacs-snippets"
            (expand-file-name "snippets" (doom-module-path :private 'hlissner))))

;;;###autoload
(defun +hlissner/find-in-templates ()
  "Browse through snippets folder"
  (interactive)
  (projectile-find-file-in-directory (car yas-snippet-dirs)))

;;;###autoload
(defun +hlissner/find-in-snippets ()
  "Browse through snippets folder"
  (interactive)
  (projectile-find-file-in-directory (car yas-snippet-dirs)))

;;;###autoload
(defun +hlissner/find-in-dotfiles ()
  (interactive)
  (projectile-find-file-in-directory (expand-file-name ".dotfiles" "~")))

;;;###autoload
(defun +hlissner/find-in-emacsd ()
  (interactive)
  (projectile-find-file-in-directory doom-emacs-dir))

;;;###autoload
(defun +hlissner/browse-emacsd ()
  (interactive)
  (let ((default-directory doom-emacs-dir))
    (call-interactively (command-remapping 'find-file))))

;;;###autoload
(defun +hlissner/browse-dotfiles ()
  (interactive)
  (let ((default-directory (expand-file-name ".dotfiles" "~")))
    (call-interactively (command-remapping 'find-file))))

;;;###autoload
(defun +hlissner/find-in-notes ()
  (interactive)
  (projectile-find-file-in-directory +org-dir))

;;;###autoload
(defun +hlissner/browse-notes ()
  (interactive)
  (let ((default-directory +org-dir))
    (call-interactively (command-remapping 'find-file))))


