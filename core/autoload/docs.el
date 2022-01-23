;;; core/autoload/docs.el -*- lexical-binding: t; -*-

;;
;;; `doom-docs-mode'

(defun doom--docs-hide-meta-h ()
  "Hide all meta or comment lines."
  (save-excursion
    (goto-char (point-min))
    (let (case-fold-search)
      (while (re-search-forward "^[ \t]*\\#" nil t)
        (catch 'abort
          (org-flag-region
           (line-beginning-position)
           (cond ((looking-at "+\\(?:TITLE\\|SUBTITLE\\): +")
                  (match-end 0))
                 ((looking-at "+\\(?:CREATED\\|SINCE\\|AUTHOR\\|EMAIL\\|DATE\\): +")
                  (throw 'abort nil))
                 ((or (eq (char-after) ?\s)
                      (looking-at "+\\(begin\\|end\\)_comment"))
                  (line-beginning-position 2))
                 ((looking-at "+\\(?:begin\\|end\\)_\\([^ \n]+\\)")
                  (line-end-position))
                 ((line-beginning-position 2)))
           doom-docs-mode t))))))

(defun doom--docs-hide-drawers-h ()
  "Hide all property drawers."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-drawer-regexp nil t)
      (let ((beg (1- (match-beginning 0)))
            (end (re-search-forward org-drawer-regexp nil t)))
        (when (save-excursion (goto-char beg) (bobp))
          (cl-incf end))
        (org-flag-region beg end doom-docs-mode t)))))

(defun doom--docs-hide-tags-h ()
  "Hide tags in org headings."
  (goto-char (point-min))
  (while (re-search-forward org-heading-regexp nil t)
    (when-let (tags (org-get-tags nil t))
      (when (or (member "noorg" tags)
                (member "unfold" tags))
        ;; prevent `org-ellipsis' around hidden regions
        (org-show-entry))
      (if (member "noorg" tags)
          (org-flag-region (line-end-position 0)
                           (save-excursion
                             (org-end-of-subtree t)
                             (forward-line 1)
                             (if (and (bolp) (eolp))
                                 (line-beginning-position)
                               (line-end-position 0)))
                           doom-docs-mode t)
        (org-flag-region (save-excursion
                           (goto-char (line-beginning-position))
                           (re-search-forward " +:[^ ]" (line-end-position))
                           (match-beginning 0))
                         (line-end-position)
                         doom-docs-mode t)))))

(defvar doom--docs-babel-cache nil)
(defun doom--docs-hide-src-blocks-h ()
  "Hide babel blocks (and/or their results) depending on their :exports arg."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (make-local-variable 'doom--docs-babel-cache)
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (let* ((beg (match-beginning 0))
               (end (save-excursion (goto-char (match-end 0))
                                    (skip-chars-forward "\n")
                                    (point)))
               (exports
                (save-excursion
                  (goto-char beg)
                  (and (re-search-forward " :exports \\([^ \n]+\\)" (line-end-position) t)
                       (match-string-no-properties 1))))
               (results (org-babel-where-is-src-block-result)))
          (save-excursion
            (when (and (if (stringp exports)
                           (member exports '("results" "both"))
                         org-export-use-babel)
                       (not results)
                       doom-docs-mode)
              (cl-pushnew beg doom--docs-babel-cache)
              (quiet! (org-babel-execute-src-block))
              (setq results (org-babel-where-is-src-block-result))
              (org-element-cache-refresh beg)
              (restore-buffer-modified-p nil)))
          (save-excursion
            (when results
              (when (member exports '("code" "both" "t"))
                (setq beg results))
              (when (member exports '("none" "code"))
                (setq end (progn (goto-char results)
                                 (goto-char (org-babel-result-end))
                                 (skip-chars-forward "\n")
                                 (point))))))
          (org-flag-region beg end doom-docs-mode t)))
      (unless doom-docs-mode
        (save-excursion
          (dolist (pos doom--docs-babel-cache)
            (goto-char pos)
            (org-babel-remove-result)
            (org-element-cache-refresh pos))
          (restore-buffer-modified-p nil))))))

(defvar doom--docs-macro-cache nil)
(defun doom--docs-expand-macros-h ()
  "Expand {{{macros}}} with their value."
  (save-excursion
    (goto-char (point-min))
    (make-local-variable 'doom--docs-macro-cache)
    (while (re-search-forward "{{{[^}]+}}}" nil t)
      (with-silent-modifications
        (if doom-docs-mode
            (when-let* ((element (org-element-context))
                        (key (org-element-property :key element))
                        (cachekey (org-element-property :value element))
                        (template (cdr (assoc-string key org-macro-templates t))))
              (let ((value (or (cdr (assoc-string cachekey doom--docs-macro-cache))
                               (setf (alist-get cachekey doom--docs-macro-cache nil nil 'equal)
                                     (org-macro-expand element org-macro-templates)))))
                (add-text-properties (match-beginning 0)
                                     (match-end 0)
                                     `(display ,value))))
          (remove-text-properties (match-beginning 0)
                                  (match-end 0)
                                  '(display))))
      (org-element-cache-refresh (point)))))

(defvar doom--docs-kbd-alist
  (let ((evilp (featurep! :editor evil)))
    `(("<leader>"      . ,(if evilp "SPC"   "C-c"))
      ("<localleader>" . ,(if evilp "SPC m" "C-c l"))
      ("<help>"        . ,(if evilp "SPC h" "C-h")))))
(defun doom--docs-expand-kbd-h ()
  "Replace special keywords in [[kbd:][...]] links."
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[kbd:.*\\]\\[\\(.*<[^>]+>.*\\)\\]\\]" nil t)
        (let ((beg (match-beginning 1))
              (end (match-end 1)))
          (if doom-docs-mode
              (add-text-properties
               beg end `(display
                         ,(let ((kbd (match-string-no-properties 1)))
                            (dolist (rep doom--docs-kbd-alist kbd)
                              (setq kbd (replace-regexp-in-string (car rep) (cdr rep) kbd))))))
            (remove-text-properties beg end '(display)))
          (org-element-cache-refresh beg)))
      (restore-buffer-modified-p nil))))

(defun doom--docs-realign-tables-h ()
  "Realign tables, as they may have changed."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-table-line-regexp nil t)
      (let ((inhibit-read-only t))
        (org-table-align)))
    (org-element-cache-refresh (point))
    (restore-buffer-modified-p nil)))

(defvar doom-docs-mode-alist
  '((flyspell-mode . -1)
    (spell-fu-mode . -1)
    (visual-line-mode . -1)
    (mixed-pitch-mode . -1)
    (variable-pitch-mode . -1))
  "An alist of minor modes to activate or deactivate in `doom-docs-mode'.

The CAR is the minor mode symbol, and CDR should be either +1 or -1,
depending.")

(defvar doom--docs-initial-values nil)
(defvar doom--docs-cookies nil)
;;;###autoload
(define-minor-mode doom-docs-mode
  "Hides metadata, tags, & drawers and activates all org-mode pretiffications.
This primes `org-mode' for reading."
  :lighter " Doom Docs"
  :after-hook (org-restart-font-lock)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an org mode buffer"))
  (mapc (lambda (sym)
          (if doom-docs-mode
              (set (make-local-variable sym) t)
            (kill-local-variable sym)))
        `(org-pretty-entities
          org-hide-emphasis-markers
          org-hide-macro-markers))
  (when doom-docs-mode
    (make-local-variable 'doom--docs-initial-values))
  (mapc (fn! ((face . plist))
          (if doom-docs-mode
              (push (apply #'face-remap-add-relative face plist) doom--docs-cookies)
            (mapc #'face-remap-remove-relative doom--docs-cookies)))
        '((org-document-title :weight bold :height 1.4)
          (org-document-info  :weight normal :height 1.15)))
  (mapc (fn! ((mode . state))
          (if doom-docs-mode
              (if (and (boundp mode) (symbol-value mode))
                  (unless (> state 0)
                    (setf (alist-get mode doom--docs-initial-values) t)
                    (funcall mode -1))
                (unless (< state 0)
                  (setf (alist-get mode doom--docs-initial-values) nil)
                  (funcall mode +1)))
            (when-let (old-val (assq mode doom--docs-initial-values))
              (funcall mode (if old-val +1 -1)))))
        doom-docs-mode-alist)
  (unless doom-docs-mode
    (kill-local-variable 'doom--docs-initial-values)))

(add-hook! 'doom-docs-mode-hook
           #'doom--docs-hide-meta-h
           #'doom--docs-hide-tags-h
           #'doom--docs-hide-drawers-h
           #'doom--docs-expand-macros-h
           #'doom--docs-expand-kbd-h
           #'doom--docs-realign-tables-h
           #'doom--docs-hide-src-blocks-h)

(defun doom--docs-toggle-read-only-h ()
  (doom-docs-mode (if buffer-read-only +1 -1)))


(defvar doom--docs-id-locations nil)
(defvar doom--docs-id-files nil)
;;;###autoload
(defun doom/reload-docs (&optional force)
  "Reload the ID locations in Doom's documentation and open docs buffers."
  (interactive (list 'interactive))
  (with-temp-buffer
    (let ((org-id-locations-file
           (doom-path (file-truename doom-cache-dir) "doom-docs-org-ids"))
          (org-id-track-globally t)
          org-agenda-files
          org-id-extra-files
          org-id-files
          org-id-locations
          org-id-extra-files
          (org-inhibit-startup t)
          org-mode-hook)
      (if force
          (org-id-update-id-locations
           (doom-files-in (list doom-docs-dir doom-modules-dir)
                          :match "/[^.].+\\.org$"))
        (org-id-locations-load))
      (setq doom--docs-id-files org-id-files
            doom--docs-id-locations org-id-locations)))
  (dolist (buf (doom-buffers-in-mode 'doom-docs-org-mode))
    (with-current-buffer buf
      (setq-local org-id-files doom--docs-id-files
                  org-id-locations doom--docs-id-locations))))


;;
;;; `doom-docs-org-mode'

(defvar doom-docs-link-alist
  '(("doom-tag"                . "https://github.com/hlissner/doom-emacs/releases/tag/%s")
    ("doom-contrib-core"       . "id:9ac0c15c-29e7-43f8-8926-5f0edb1098f0")
    ("doom-contrib-docs"       . "id:31f5a61d-d505-4ee8-9adb-97678250f4e2")
    ("doom-contrib-maintainer" . "id:e71e9595-a297-4c49-bd11-f238329372db")
    ("doom-contrib-module"     . "id:b461a050-8702-4e63-9995-c2ef3a78f35d")
    ("doom-faq"                . "id:5fa8967a-532f-4e0c-8ae8-25cd802bf9a9")
    ("doom-help"               . "id:9bb17259-0b07-45a8-ae7a-fc5e0b16244e")
    ("doom-help-changelog"     . "id:7c56cc08-b54b-4f4b-b106-a76e2650addd")
    ("doom-help-modules"       . "id:1ee0b650-f09b-4454-8690-cc145aadef6e")
    ("doom-index"              . "id:3051d3b6-83e2-4afa-b8fe-1956c62ec096")
    ("doom-module-index"       . "id:12d2de30-c569-4b8e-bbc7-85dd5ccc4afa")
    ("doom-module-issues"      . "https://github.com/orgs/doomemacs/projects/2/views/1?filterQuery=label:\"%s\"")
    ("doom-report"             . "https://github.com/hlissner/doom-emacs/issues/new/choose")
    ("doom-source"             . "https://github.com/hlissner/doom-emacs/tree/develop/%s")
    ("doom-suggest-edit"       . "id:31f5a61d-d505-4ee8-9adb-97678250f4e2")
    ("doom-suggest-faq"        . "id:aa28b732-0512-49ed-a47b-f20586c0f051")
    ("github"                  . "https://github.com/%s")))

;;;###autoload
(define-derived-mode doom-docs-org-mode org-mode "Doom Docs"
  "A derivative of `org-mode' for Doom's documentation files.

Keeps track of its own IDs in `doom-docs-dir' and toggles `doom-docs-mode' when
`read-only-mode' is activated."
  :after-hook (visual-line-mode -1)
  (require 'org-id)
  (require 'ob)
  (setq-local org-id-link-to-org-use-id t
              org-id-method 'uuid
              org-id-track-globally t
              org-id-locations-file (doom-path doom-cache-dir "doom-docs-org-ids")
              org-id-locations doom--docs-id-locations
              org-id-files doom--docs-id-files
              org-num-max-level 3
              org-footnote-define-inline nil
              org-footnote-auto-label t
              org-footnote-auto-adjust t
              wgrep-change-readonly-file t
              org-link-abbrev-alist-local (append org-link-abbrev-alist-local doom-docs-link-alist)
              org-babel-default-header-args
              (append '((:eval . "no") (:tangle . "no"))
                      org-babel-default-header-args))
  (unless org-inhibit-startup
    (doom/reload-docs)
    (unless (local-variable-p 'org-startup-with-inline-images)
      (org-display-inline-images))
    (unless (local-variable-p 'org-startup-indented)
      (org-indent-mode +1))
    (unless (local-variable-p 'org-startup-numerated)
      (when (bound-and-true-p org-num-mode)
        (org-num-mode -1))
      (org-num-mode +1))
    (unless (local-variable-p 'org-startup-folded)
      (let ((org-startup-folded 'content))
        (org-set-startup-visibility))))
  (add-hook 'read-only-mode-hook #'doom--docs-toggle-read-only-h nil 'local))

;;;###autoload
(defun doom-docs-read-only-h ()
  "Activate `read-only-mode' if the current file exists and is non-empty."
  ;; The rationale: if it's empty or non-existant, you want to write an org
  ;; file, not read it.
  (when (and buffer-file-name
             (> (buffer-size) 0)
             (not (string-prefix-p "." (file-name-base buffer-file-name)))
             (file-exists-p buffer-file-name))
    (read-only-mode +1)))

;;;###autoload (add-hook 'doom-docs-org-mode-hook #'doom-docs-read-only-h)


;;
;;; Custom links

;;;###autoload
(defun doom-docs-read-link-desc-at-point (&optional default context)
  "TODO"
  (if (and (stringp default) (not (string-empty-p default)))
      (string-trim default)
    (if-let* ((context (or context (org-element-context)))
              (context (org-element-lineage context '(link) t))
              (beg (org-element-property :contents-begin context))
              (end (org-element-property :contents-end context)))
        (unless (= beg end)
          (replace-regexp-in-string
           "[ \n]+" " " (string-trim (buffer-substring-no-properties beg end)))))))


;;;###autoload
(defun doom-docs-doom-module-link-follow-fn (link)
  (cl-destructuring-bind (&key category module flag)
      (doom--docs-read-module-link link)
    (when category
      (let ((doom-modules-dirs (list doom-modules-dir)))
        (if-let* ((path (doom-module-locate-path category module))
                  (path (or (car (doom-glob path "README.org"))
                            path)))
            (find-file path)
          (user-error "Can't find Doom module '%s'" link))))
    (when flag
      (goto-char (point-min))
      (when (and (re-search-forward "^\\*+ \\(?:TODO \\)?Module flags")
                 (re-search-forward (format "^\\s-*- \\+%s ::[ \n]"
                                            (substring (symbol-name flag) 1))
                                    (save-excursion (org-get-next-sibling)
                                                    (point))))
        (org-show-entry)
        (recenter)))))

;;;###autoload
(defun doom-docs-doom-module-link-face-fn (link)
  (cl-destructuring-bind (&key category module flag)
      (doom--docs-read-module-link link)
    (if (doom-module-locate-path category module)
        `(:inherit org-priority
          :weight bold)
      'error)))

;;;###autoload
(defun doom-docs-doom-package-link-follow-fn (link)
  "TODO"
  (doom/describe-package
   (intern-soft
    (doom-docs-read-link-desc-at-point link))))

;;;###autoload
(defun doom-docs-make-symbol-link (fn)
  "TODO"
  (lambda (link)
    (let ((desc (doom-docs-read-link-desc-at-point link)))
      (funcall
       fn (or (intern-soft desc)
              (user-error "Can't find documentation for %S" desc))))))

(defun doom--docs-describe-kbd (keystr)
  (dolist (key `(("<leader>" . ,doom-leader-key)
                 ("<localleader>" . ,doom-localleader-key)
                 ("<prefix>" . ,(if (bound-and-true-p evil-mode)
                                    (concat doom-leader-key " u")
                                  "C-u"))
                 ("<help>" . ,(if (bound-and-true-p evil-mode)
                                  (concat doom-leader-key " h")
                                "C-h"))
                 ("\\<M-" . "alt-")
                 ("\\<S-" . "shift-")
                 ("\\<s-" . "super-")
                 ("\\<C-" . "ctrl-")))
    (setq keystr
          (replace-regexp-in-string (car key) (cdr key)
                                    keystr t t)))
  keystr)

;;;###autoload
(defun doom-docs-read-kbd-at-point (&optional default context)
  "TODO"
  (doom--docs-describe-kbd
   (doom-docs-read-link-desc-at-point default context)))

;;;###autoload
(after! org
  ;; Add "lookup" links for packages and keystrings; useful for Emacs
  ;; documentation -- especially Doom's!
  (org-link-set-parameters
   "kbd"
   :follow (lambda (_) (minibuffer-message "%s" (doom--display-docs-link-in-eldoc-a)))
   :help-echo #'doom-docs-read-kbd-at-point
   :face 'help-key-binding)
  (org-link-set-parameters
   "var"
   :follow (doom-docs-make-symbol-link #'helpful-variable)
   :face '(font-lock-variable-name-face underline))
  (org-link-set-parameters
   "fn"
   :follow (doom-docs-make-symbol-link #'helpful-callable)
   :face '(font-lock-function-name-face underline))
  (org-link-set-parameters
   "doom-ref"
   :follow (lambda (link)
             (let ((link (doom-docs-read-link-desc-at-point link))
                   (url "https://github.com")
                   (doom-repo "hlissner/doom-emacs"))
               (save-match-data
                 (browse-url
                  (cond ((string-match "^\\([^/]+\\(?:/[^/]+\\)?\\)?#\\([0-9]+\\(?:#.*\\)?\\)" link)
                         (format "%s/%s/issues/%s" url
                                 (or (match-string 1 link)
                                     doom-repo)
                                 (match-string 2 link)))
                        ((string-match "^\\([^/]+\\(?:/[^/]+\\)?@\\)?\\([a-z0-9]\\{7,\\}\\(?:#.*\\)?\\)" link)
                         (format "%s/%s/commit/%s" url
                                 (or (match-string 1 link)
                                     doom-repo)
                                 (match-string 2 link)))
                        ((user-error "Invalid doom-ref link: %S" link)))))))
   :face (lambda (link)
           (let ((link (doom-docs-read-link-desc-at-point link)))
             (if (or (string-match "^\\([^/]+\\(?:/[^/]+\\)?\\)?#\\([0-9]+\\(?:#.*\\)?\\)" link)
                     (string-match "^\\([^/]+\\(?:/[^/]+\\)?@\\)?\\([a-z0-9]\\{7,\\}\\(?:#.*\\)?\\)" link))
                 'org-link
               'error))))
  (org-link-set-parameters
   "doom-user"
   :follow (lambda (link)
             (browse-url
              (format "https://github.com/%s"
                      (string-remove-prefix
                       "@" (doom-docs-read-link-desc-at-point link)))))
   :face (lambda (_) 'org-priority))
  (org-link-set-parameters
   "doom-package"
   :follow #'doom-docs-doom-package-link-follow-fn
   :face (lambda (_) '(:inherit org-priority :slant italic)))
  (org-link-set-parameters
   "doom-module"
   :follow #'doom-docs-doom-module-link-follow-fn
   :face #'doom-docs-doom-module-link-face-fn)
  (org-link-set-parameters
   "doom-changelog"
   :follow (lambda (link)
             (find-file (doom-path doom-docs-dir "changelog.org"))
             (org-match-sparse-tree nil link)))


  (add-to-list 'org-link-abbrev-alist '("doom-repo" . "https://github.com/hlissner/doom-emacs/%s"))

  (defadvice! doom--display-docs-link-in-eldoc-a (&rest _)
    "Display full doom-*: links in minibuffer when cursor/mouse is over it."
    :before-until #'org-eldoc-documentation-function
    (when-let* ((context (org-element-context))
                (path (org-element-property :path context)))
      (pcase (org-element-property :type context)
        ("kbd"
         (format "%s %s"
                 (propertize "Key sequence:" 'face 'bold)
                 (propertize (doom-docs-read-kbd-at-point path context)
                             'face 'help-key-binding)))
        ("doom-module"
         (format "%s %s"
                 (propertize "Doom module:" 'face 'bold)
                 (propertize (doom-docs-read-link-desc-at-point path)
                             'face 'org-priority)))
        ("doom-package"
         (format "%s %s"
                 (propertize "Doom package:" 'face 'bold)
                 (propertize (doom-docs-read-link-desc-at-point path)
                             'face 'org-priority)))))))

(defun doom--docs-read-module-link (link)
  (cl-destructuring-bind (category &optional module flag)
      (let ((desc (doom-docs-read-link-desc-at-point link)))
        (if (string-prefix-p "+" (string-trim-left desc))
            (list nil nil (intern desc))
          (mapcar #'intern (split-string desc " " nil))))
    (list :category category
          :module module
          :flag flag)))
