;;; core/cli/make.el -*- lexical-binding: t; -*-

(defvar doom-cli-docs-menu
  `((("docs/*.org" "modules/README.org")
     (left ("← Back to index" . "doom-index:")))
    ("docs/index.org"
     (left ("↖ FAQ" . "doom-faq:")))
    ("modules/*/README.org"
     (left ("← Back to module index" . "doom-module-index:")))
    ("modules/*/*/README.org"
     (left ("← Back to module index" . "doom-module-index:"))
     (right ("! Issues"
             . ,(fn! (&key category module)
                  (format "doom-module-issues::%s %s" category module)))))))

(defvar doom-cli-docs-common-menu
  `(("↖ Github"
     . ,(fn! (&key readme)
          (if (file-exists-p! readme doom-modules-dir)
              (format "doom-repo:tree/develop/modules/%s" (string-remove-prefix "./" (file-name-directory readme)))
            (format "doom-repo:tree/develop/%s" (string-remove-prefix "./../" readme)))))
    ("± Suggest edits" . "doom-suggest-edit:")
    ("? Help"
     . ,(fn! (&key title)
          (cond ((equal title "Changelog")   "doom-help-changelog:")
                ((string-prefix-p ":" title) "doom-help-modules:")
                ("doom-help:"))))))


;;
;;; Commands

;; (load! "make/changelog")

(defcli! make (&rest targets)
  "(Re)Generate project files and boilerplate."
  (dolist (target targets t)
    (if-let (fn (intern-soft (format "doom-cli--make-%s" target)))
        (funcall fn)
      (print! (error "No target: %S") target))))

(defcli! make-index (&rest dirs)
  "Generate a module index at the root of DIR."
  (let ((missing-dirs (seq-remove #'file-directory-p dirs)))
    (dolist (dir missing-dirs)
      (print! (error "Directory not found or not a directory: %S" dir)))
    (when missing-dirs
      (user-error "Aborting...")))
  (doom-cli--make-module-index dirs))

;; (defun doom-cli--make-workflows ()
;;   )

(defun doom-cli--make-module-index (&optional dirs)
  (doom-cli---make-module-index
   (or (mapcar #'expand-file-name dirs)
       (list doom-modules-dir))))

(defun doom-cli--make-docs ()
  (print! (start "Generating menus in docs"))
  (print-group!
   (dolist (menu doom-cli-docs-menu)
     (dolist (glob (doom-enlist (car menu)))
       (dolist (file (doom-glob doom-emacs-dir glob))
         (unless (string-prefix-p "." (file-name-nondirectory file))
           (doom-cli--with-file file
             (let ((plist (doom--cli--read-readme file)))
               (doom-log "%s %s"
                         (plist-get plist :category)
                         (plist-get plist :module))
               (print-group!
                (apply #'doom-cli---make-readme-menu (cdr menu) plist)))))))))

  (print! (start "Generating module index"))
  (print-group!
   (doom-cli--make-module-index (list doom-modules-dir))))




;;
;;; Helpers

(defmacro doom-cli--with-file (file &rest body)
  (declare (indent 1))
  `(let ((inhibit-read-only t))
     (with-current-buffer
         (or (get-file-buffer ,file)
             (find-file-noselect ,file))
       (save-excursion
         (goto-char (point-min))
         ,@body
         (when (buffer-modified-p)
           (save-buffer))))))

(defun doom-cli--each-module (dir fn)
  (mapcar (lambda (subdir)
            (apply fn (doom--cli--read-readme (expand-file-name "README.org" subdir))))
          (seq-filter (lambda (path)
                        (and (file-directory-p path)
                             (not (string-suffix-p "." (file-name-nondirectory path)))))
                      (doom-glob doom-modules-dir (or dir "*")
                                 "*"))))

(defun doom--cli--read-readme (readme)
  (let ((default-directory (file-name-directory readme)))
    (append
     `(:directory ,default-directory
       :readme ,(concat "./" (file-relative-name readme doom-modules-dir)))
     (when-let (mod (doom-module-from-path readme))
       (let ((category (car mod))
             (module   (cdr mod)))
         (or (file-directory-p
              (doom-path doom-modules-dir
                         (doom-keyword-name category) (symbol-name module)))
             (setq module nil))
         `(:category ,category
           :module   ,module)))
     (when (file-readable-p readme)
       (with-temp-buffer
         (insert-file-contents readme)
         (append (flatten-list
                  (mapcar (lambda (x)
                            (cons (doom-keyword-intern (downcase (car x)))
                                  (cdr x)))
                          (let ((keywords '("TITLE" "SUBTITLE" "SINCE" "CREATED" "VERSION" "STATUS")))
                            (org-collect-keywords keywords keywords (list default-directory)))))
                 `(:flags ,(save-excursion
                             (when (re-search-forward "^\\*\\* Module flags" nil t)
                               (let ((end (save-excursion (org-get-next-sibling) (point)))
                                     flags)
                                 (while (re-search-forward "^\\s-*\\- \\(\\+[^:]+ \\)::" end t)
                                   (push (string-trim-right (match-string 1)) flags))
                                 (sort flags #'string-lessp))))
                   :description ,(save-excursion
                                   (when (re-search-forward "^\\* Description" nil t)
                                     (string-trim
                                      (buffer-substring-no-properties
                                       (line-beginning-position 2)
                                       (save-excursion (org-next-visible-heading 1)
                                                       (point)))))))))))))

(cl-defun doom-cli---make-readme-menu (menu &rest plist &key category directory &allow-other-keys)
  (let ((beg (point-min)))
    (goto-char beg)
    (when (looking-at-p org-drawer-regexp)
      (re-search-forward org-drawer-regexp nil t 2)
      (forward-char 1)
      (setq beg (point)))
    (save-excursion
      (forward-line 1)
      (when (looking-at-p "^----+$")
        (delete-region beg (line-beginning-position 2)))))
  (let* ((fn
          (lambda (menu)
            (cl-destructuring-bind (icon . label)
                (split-string (car menu) " ")
              (if (cdr menu)
                  (format "%s [[%s][%s]]"
                          icon
                          (cond ((functionp (cdr menu))
                                 (apply (cdr menu) plist))
                                ((file-name-absolute-p (cdr menu))
                                 (concat "file:"
                                         (file-relative-name (file-truename (cdr menu))
                                                             directory)))
                                ((cdr menu)))
                          (string-join label " "))
                (format "%s+ %s+" icon (string-join label " "))))))
         (lenfn
          (lambda (link)
            (length (replace-regexp-in-string org-link-any-re "\\3" link))))
         (sep  "  ")
         (lhs  (mapconcat fn (alist-get 'left menu) sep))
         (rhs  (mapconcat
                fn (append (alist-get 'right menu)
                           doom-cli-docs-common-menu)
                sep))
         (llen (funcall lenfn lhs))
         (rlen (funcall lenfn rhs))
         (pad  (max 0 (- 80 llen rlen))))
    (insert lhs
            (if (zerop rlen) ""
              (format "%s%s" (make-string pad 32) rhs))
            "\n" (make-string 80 ?-) "\n")))

(defun doom-cli---make-module-index (modules-dir &optional minlevel)
  (unless minlevel
    (setq minlevel 2))
  (doom-cli--with-file (doom-path modules-dir "README.org")
    (letf! (defun modules-dirs (dir glob)
             (seq-filter #'file-directory-p (doom-glob dir glob)))
      (if (not (re-search-forward "^\\* Module list (\\([0-9]+\\))" nil t))
          (goto-char (point-max))
        (goto-char (line-beginning-position))
        (delete-region (line-beginning-position)
                       (save-excursion (org-end-of-subtree t t)
                                       (point))))
      (delete-blank-lines)
      (insert (format "\n* Module list (%d) :unfold:\n" (length (modules-dirs modules-dir "*/*")))
              "# Do not edit this list by hand; run 'doom make-index path/to/modules'\n")
      (dolist (category-dir (modules-dirs modules-dir "*"))
        (let* ((category (file-name-nondirectory category-dir))
               (readme (car (doom-glob category-dir "README.org")))
               (module-count (length (modules-dirs category-dir "*"))))
          (insert (make-string minlevel ?*) " ")
          (if readme
              (insert (format "[[doom-module::%s][:%s]] (%d) :unfold:\n"
                              category category module-count)
                      (string-trim (plist-get (doom--cli--read-readme readme) :description))
                      "\n\n")
            (insert (format ":%s (%d) :unfold:\n" category module-count)
                    "/(No description)/\n\n"))
          (doom-cli--each-module
           category-dir
           (fn! (&key title subtitle description flags category module readme)
             (let ((label (if title
                              (format "[[doom-module:%s %s][%s]]"
                                      category module module)
                            (symbol-name module)))
                   (flagstr (if flags
                                (mapconcat
                                 (lambda (f)
                                   (format "[[doom-module:%s %s %s][%s]]"
                                           category module f f))
                                 flags " ")
                              ""))
                   (maxwidth (+ 12
                                (* (1- minlevel) 2)
                                (length (symbol-name module)))))
               (insert (make-string (1+ minlevel) ?*) " " label
                       (apply #'format ": %s%s"
                              (if flags
                                  (list (make-string
                                         (max 0 (- 80 maxwidth
                                                   (max 0 (1- (length flags)))
                                                   (cl-reduce #'+ flags :key #'length)))
                                         ?\s)
                                        flagstr)
                                (let ((str "-"))
                                  (list (make-string (max 0 (- 80 maxwidth (length str)))
                                                     ?\s)
                                        str))))
                       "\n" (if (and (stringp subtitle)
                                     (not (string-empty-p subtitle)))
                                (concat ": " subtitle "\n\n")
                              "")
                       (or description "/(No description)/")
                       "\n"))
             (insert "\n")))
          (insert "\n"))))))
