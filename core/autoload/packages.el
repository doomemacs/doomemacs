;;; core/autoload/packages.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom/reload-packages ()
  "Reload `doom-packages', `package' and `quelpa'."
  (interactive)
  ;; HACK straight.el must be loaded for this to work
  (message "Reloading packages")
  (doom-initialize-packages t)
  (message "Reloading packages...DONE"))


;;
;;; Bump commands

(defun doom--package-merge-recipes (package plist)
  (require 'straight)
  (doom-plist-merge
   (plist-get plist :recipe)
   (if-let (recipe (straight-recipes-retrieve package))
       (cdr (if (memq (car recipe) '(quote \`))
                (eval recipe t)
              recipe))
     (let ((recipe (plist-get (cdr (assq package doom-packages))
                              :recipe)))
       (if (keywordp (car recipe))
           recipe
         (cdr recipe))))))

(defun doom--package-to-bump-string (package plist)
  "Return a PACKAGE and its PLIST in 'username/repo@commit' format."
  (format "%s@%s"
          (plist-get (doom--package-merge-recipes package plist) :repo)
          (substring-no-properties (plist-get plist :pin) 0 12)))

(defun doom--package-at-point (&optional point)
  "Return the package and plist from the (package! PACKAGE PLIST...) at point."
  (save-match-data
    (save-excursion
      (and point (goto-char point))
      (while (and (or (atom (sexp-at-point))
                      (doom-point-in-string-or-comment-p))
                  (search-backward "(" nil t)))
      (when (eq (car-safe (sexp-at-point)) 'package!)
        (cl-destructuring-bind (beg . end)
            (bounds-of-thing-at-point 'sexp)
          (let* ((doom-packages nil)
                 (buffer-file-name
                  (or buffer-file-name
                      (bound-and-true-p org-src-source-file-name)))
                 (package (eval (sexp-at-point) t)))
            (list :beg beg
                  :end end
                  :package (car package)
                  :plist (cdr package))))))))

;;;###autoload
(defun doom/bumpify-package-at-point ()
  "Convert `package!' call at point to a bump string."
  (interactive)
  (cl-destructuring-bind (&key package plist beg end)
      (doom--package-at-point)
    (when-let (str (doom--package-to-bump-string package plist))
      (goto-char beg)
      (delete-region beg end)
      (insert str))))

;;;###autoload
(defun doom/bumpify-packages-in-buffer ()
  "Convert all `package!' calls in buffer into bump strings."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "(package!" nil t)
      (unless (doom-point-in-string-or-comment-p)
        (doom/bumpify-package-at-point)))))

;;;###autoload
(defun doom/bump-package-at-point (&optional select)
  "Inserts or updates a `:pin' for the `package!' statement at point.
Grabs the latest commit id of the package using 'git'."
  (interactive "P")
  (doom-initialize-packages)
  (cl-destructuring-bind (&key package plist beg end)
      (or (doom--package-at-point)
          (user-error "Not on a `package!' call"))
    (let* ((recipe (doom--package-merge-recipes package plist))
           (branch (plist-get recipe :branch))
           (oldid (or (plist-get plist :pin)
                      (doom-package-get package :pin)))
           (url (straight-vc-git--destructure recipe (upstream-repo upstream-host)
                  (straight-vc-git--encode-url upstream-repo upstream-host)))
           (id (or (when url
                     (cdr (doom-call-process
                           "git" "ls-remote" url
                           (unless select branch))))
                   (user-error "Couldn't find a recipe for %s" package)))
           (id (car (split-string
                     (if select
                         (completing-read "Commit: " (split-string id "\n" t))
                       id)))))
      (when (and oldid
                 (plist-member plist :pin)
                 (equal oldid id))
        (user-error "%s: no update necessary" package))
      (save-excursion
        (if (re-search-forward ":pin +\"\\([^\"]+\\)\"" end t)
            (replace-match id t t nil 1)
          (goto-char (1- end))
          (insert " :pin " (prin1-to-string id))))
      (cond ((not oldid)
             (message "%s: → %s" package (substring id 0 10)))
            ((< (length oldid) (length id))
             (message "%s: extended to %s..." package id))
            ((message "%s: %s → %s"
                      package
                      (substring oldid 0 10)
                      (substring id 0 10)))))))

;;;###autoload
(defun doom/bump-packages-in-buffer (&optional select)
  "Inserts or updates a `:pin' to all `package!' statements in current buffer.
If SELECT (prefix arg) is non-nil, prompt you to choose a specific commit for
each package."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (doom-initialize-packages)
    (let (packages)
      (while (search-forward "(package! " nil t)
        (unless (let ((ppss (syntax-ppss)))
                  (or (nth 4 ppss)
                      (nth 3 ppss)
                      (save-excursion
                        (and (goto-char (match-beginning 0))
                             (not (plist-member (sexp-at-point) :pin))))))
          (condition-case e
              (push (doom/bump-package-at-point select) packages)
            (user-error (message "%s" (error-message-string e))))))
      (if packages
          (message "Updated %d packages\n- %s" (length packages) (string-join packages "\n- "))
        (message "No packages to update")))))

;;;###autoload
(defun doom/bump-module (category &optional module select)
  "Bump packages in CATEGORY MODULE.
If SELECT (prefix arg) is non-nil, prompt you to choose a specific commit for
each package."
  (interactive
   (let* ((module (completing-read
                   "Bump module: "
                   (let ((modules (doom-module-list 'all)))
                     (mapcar (lambda (m)
                               (if (listp m)
                                   (format "%s %s" (car m) (cdr m))
                                 (format "%s" m)))
                             (append '(:private :core)
                                     (delete-dups (mapcar #'car modules))
                                     modules)))
                   nil t nil nil))
          (module (split-string module " " t)))
     (list (intern (car module))
           (ignore-errors (intern (cadr module)))
           current-prefix-arg)))
  (mapc (fn! ((cat . mod))
          (if-let (packages-file
                   (pcase cat
                     (:private (car (doom-glob doom-private-dir "packages.el")))
                     (:core    (car (doom-glob doom-core-dir "packages.el")))
                     (_ (doom-module-locate-path cat mod "packages.el"))))
              (with-current-buffer
                  (or (get-file-buffer packages-file)
                      (find-file-noselect packages-file))
                (doom/bump-packages-in-buffer select)
                (save-buffer))
            (message "Module %s has no packages.el file" (cons cat mod))))
        (if module
            (list (cons category module))
          (cl-remove-if-not (lambda (m) (eq (car m) category))
                            (append '((:core) (:private))
                                    (doom-module-list 'all))))))

;;;###autoload
(defun doom/bump-package (package)
  "Bump PACKAGE in all modules that install it."
  (interactive
   (list (intern (completing-read "Bump package: "
                          (mapcar #'car (doom-package-list 'all))))))
  (let* ((packages (doom-package-list 'all))
         (modules (plist-get (alist-get package packages) :modules)))
    (unless modules
      (user-error "This package isn't installed by any Doom module"))
    (dolist (module modules)
      (when-let (packages-file (doom-module-locate-path (car module) (cdr module)))
        (doom/bump-module (car module) (cdr module))))))


;;
;;; Bump commits

;;;###autoload
(defun doom/bumpify-diff (&optional interactive)
  "Copy user/repo@hash -> user/repo@hash's of changed packages to clipboard.

Must be run from a magit diff buffer."
  (interactive (list 'interactive))
  (save-window-excursion
    (magit-diff-staged)
    (unless (eq major-mode 'magit-diff-mode)
      (user-error "Not in a magit diff buffer"))
    (let (targets lines)
      (save-excursion
        (while (re-search-forward "^modified +\\(.+\\)$" nil t)
          (cl-pushnew (doom-module-from-path (match-string 1)) targets
                      :test #'equal)))
      (while (re-search-forward "^-" nil t)
        (let ((file (magit-file-at-point))
              before after)
          (save-window-excursion
            (call-interactively #'magit-diff-visit-file)
            (or (looking-at-p "(package!")
                (re-search-forward "(package! " (line-end-position) t)
                (re-search-backward "(package! "))
            (let ((buffer-file-name file))
              (cl-destructuring-bind (&key package plist _beg _end)
                  (doom--package-at-point)
                (setq before (doom--package-to-bump-string package plist)))))
          (re-search-forward "^+")
          (save-window-excursion
            (call-interactively #'magit-diff-visit-file)
            (or (looking-at-p "(package!")
                (re-search-forward "(package! " (line-end-position) t)
                (re-search-backward "(package! "))
            (let ((buffer-file-name file))
              (cl-destructuring-bind (&key package plist _beg _end)
                  (doom--package-at-point)
                (setq after (doom--package-to-bump-string package plist)))))
          (cl-pushnew (format "%s -> %s" before after) lines)))
      (if (null lines)
          (user-error "No bumps to bumpify")
        (prog1 (funcall (if interactive #'kill-new #'identity)
                        (format "bump: %s\n\n%s"
                                (mapconcat (lambda (x)
                                             (mapconcat #'symbol-name x " "))
                                           (cl-loop with alist = ()
                                                    for (category . module) in (reverse targets)
                                                    do (setf (alist-get category alist)
                                                             (append (alist-get category alist) (list module)))
                                                    finally return alist)
                                           " ")
                                (string-join (sort (reverse lines) #'string-lessp)
                                             "\n")))
          (when interactive
            (message "Copied to clipboard")))))))

;;;###autoload
(defun doom/commit-bumps ()
  "Create a pre-filled magit commit for currently bumped packages."
  (interactive)
  (magit-commit-create
   (list "-e" "-m" (doom/bumpify-diff))))


;;
;;; Package metadata

;;;###autoload
(defun doom-package-homepage (package)
  "Return the url to PACKAGE's homepage (usually a repo)."
  (doom-initialize-packages)
  (or (get package 'homepage)
      (put package 'homepage
           (cond ((when-let (location (locate-library (symbol-name package)))
                    (with-temp-buffer
                      (if (string-match-p "\\.gz$" location)
                          (jka-compr-insert-file-contents location)
                        (insert-file-contents (concat (file-name-sans-extension location) ".el")
                                              nil 0 4096))
                      (let ((case-fold-search t))
                        (when (re-search-forward " \\(?:URL\\|homepage\\|Website\\): \\(http[^\n]+\\)\n" nil t)
                          (match-string-no-properties 1))))))
                 ((when-let ((recipe (straight-recipes-retrieve package)))
                    (straight--with-plist (straight--convert-recipe recipe)
                        (host repo)
                      (pcase host
                        (`github (format "https://github.com/%s" repo))
                        (`gitlab (format "https://gitlab.com/%s" repo))
                        (`bitbucket (format "https://bitbucket.com/%s" (plist-get plist :repo)))
                        (`git repo)
                        (_ nil)))))
                 ((or package-archive-contents
                      (progn (package-refresh-contents)
                             package-archive-contents))
                  (pcase (ignore-errors (package-desc-archive (cadr (assq package package-archive-contents))))
                    (`nil nil)
                    ("org" "https://orgmode.org")
                    ((or "melpa" "melpa-mirror")
                     (format "https://melpa.org/#/%s" package))
                    ("gnu"
                     (format "https://elpa.gnu.org/packages/%s.html" package))
                    (archive
                     (if-let (src (cdr (assoc package package-archives)))
                         (format "%s" src)
                       (user-error "%S isn't installed through any known source (%s)"
                                   package archive)))))
                 ((user-error "Can't get homepage for %S package" package))))))
