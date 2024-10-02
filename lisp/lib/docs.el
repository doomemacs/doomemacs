;;; core/lib/docs.el -- a reader mode for Doom's Org docs -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; This file defines `doom-docs-org-mode', a major mode derived from org-mode,
;; intended to make Doom's documentation more readable, insert virtual
;; navigation in the header, prettify org buffers (hiding syntax and special
;; tags), and defines special link types.
;;
;; This mode isn't only for Doom's docs, but also for docs found in other Doom
;; projects, like doomemacs/themes, doomemacs/snippets, and more. Most of its
;; variables can be customized in its root .dir-locals.el.
;;
;;; Code:

;;;###autoload
(defvar doom-docs-dir (file-name-concat doom-emacs-dir "docs/")
  "Where Doom's documentation files are stored. Must end with a slash.")

(defvar doom-docs-header-specs
  '(("/docs/index\\.org$"
     (:label "FAQ"
      :icon "nf-md-message_question_outline"
      :link "doom-faq:"
      :help-echo "Open the FAQ document"))
    (("/docs/[^/]+\\.org$" "/modules/README\\.org$")
     (:label "Back to index"
      :icon "nf-md-arrow_left"
      :link "doom-index"
      :help-echo "Navigate to the root index"))
    ("/modules/[^/]+/README\\.org$"
     (:label "Back to module index"
      :icon "nf-md-arrow_left"
      :link "doom-module-index:"))
    ("/modules/[^/]+/[^/]+/README\\.org$"
     (:label "Back to module index"
      :icon "nf-md-arrow_left"
      :link "doom-module-index:")
     (:label "History"
      :icon "nf-md-history"
      :icon-face font-lock-variable-name-face
      :link (lambda ()
              (cl-destructuring-bind (category . module) (doom-module-from-path (buffer-file-name))
                (format "doom-module-history:%s/%s" (doom-keyword-name category) module)))
      :help-echo "View the module history"
      :align right)
     (:label "Issues"
      :icon "nf-md-flag"
      :icon-face error
      :link (lambda ()
              (cl-destructuring-bind (category . module) (doom-module-from-path (buffer-file-name))
                (format "doom-module-issues::%s %s" category module)))
      :align right))
     (t
      (:label "Suggest edits"
       :icon "nf-md-account_edit"
       :icon-face warning
       :link "doom-suggest-edit"
       :align right)
      (:label "Help"
       :icon "nf-md-timeline_help_outline"
       :icon-face font-lock-function-name-face
       :link (lambda ()
               (let ((title (cadar (org-collect-keywords '("TITLE")))))
                 (cond ((equal title "Changelog") "doom-help-changelog:")
                       ((string-prefix-p ":" title) "doom-help-modules:")
                       (t "doom-help:"))))
       :align right))))

(defun doom-docs--make-header ()
  "Create a header string for the current buffer."
  (let* ((applicable-specs
          (cl-loop for (condition . specs) in doom-docs-header-specs
                   when (if (symbolp condition)
                            (symbol-value condition)
                          (seq-some (doom-rpartial #'string-match-p (buffer-file-name))
                                    (ensure-list condition)))
                   append specs))
         (left-specs
          (cl-remove-if-not
           (lambda (s) (memq (plist-get s :align) '(nil left)))
           applicable-specs))
         (right-specs
          (cl-remove-if-not
           (lambda (s) (eq (plist-get s :align) 'right))
           applicable-specs))
         (left-string
          (mapconcat #'doom-docs--make-header-link left-specs " "))
         (right-string
          (mapconcat #'doom-docs--make-header-link right-specs " ")))
    (if (string-empty-p right-string)
        (concat " " left-string)
      (concat " " left-string
              (make-string (max (- (window-width)
                                   (length left-string)
                                   (length right-string)
                                   4)
                                1)
                           ?\s)
              right-string))))

(defun doom-docs--make-header-link (spec)
  "Create a header link according to SPEC."
  (let ((icon (and (plist-get spec :icon)
                   (with-demoted-errors "DOCS ERROR: %s"
                     (funcall (or (plist-get spec :icon-function)
                                  #'nerd-icons-mdicon)
                              (plist-get spec :icon)))))
        (label (pcase (plist-get spec :label)
                 ((and (pred functionp) lab)
                  (funcall lab))
                 ((and (pred stringp) lab)
                  lab)))
        (link (pcase (plist-get spec :link)
                ((and (pred functionp) link)
                 (funcall link))
                ((and (pred stringp) link)
                 link))))
    (propertize
     (concat
      (and icon
           (propertize icon 'face
                       (cadr (or (plist-member spec :icon-face)
                                 (plist-member spec :face)))))
      (and icon label " ")
      (and label
           (propertize label 'face (cadr (or (plist-get spec :face)
                                             '(nil link))))))
     'doom-docs-link link
     'keymap doom-docs--header-link-keymap
     'help-echo (or (plist-get spec :help-echo)
                    (format "LINK: %s" link))
     'mouse-face 'highlight)))

(setq doom-docs--header-link-keymap
  (let ((km (make-sparse-keymap)))
    (define-key km [header-line mouse-2] 'doom-docs--open-header-link)
    (define-key km [mouse-2] 'doom-docs--open-header-link)
    (define-key km [follow-link] 'mouse-face)
    km))

(defun doom-docs--open-header-link (ev)
  "Open the header link which is the target of the event EV."
  (interactive "e")
  (let* ((string-and-pos (posn-string (event-start ev)))
         (docs-buf (window-buffer (posn-window (event-start ev))))
         (link (get-pos-property (cdr string-and-pos)
                                 'doom-docs-link
                                 (car string-and-pos)))
         (parent-link-abbrevs
          (buffer-local-value 'org-link-abbrev-alist-local docs-buf)))
    (with-temp-buffer
      (setq buffer-file-name (buffer-file-name docs-buf))
      (let ((org-inhibit-startup t))
        (org-mode))
      (setq-local org-link-abbrev-alist-local parent-link-abbrevs)
      (insert "[[" link "]]")
      (set-buffer-modified-p nil)
      (org-link-open (org-element-context)))))

;; DEPRECATED Will be renamed once docs "framework" is generalized
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
    ("doom-module-issues"      . "https://github.com/doomemacs/doomemacs/labels/%s")
    ("doom-module-history"     . "https://github.com/doomemacs/doomemacs/commits/master/modules/%s")
    ("doom-report"             . "https://github.com/doomemacs/doomemacs/issues/new/choose")
    ("doom-suggest-edit"       . "id:31f5a61d-d505-4ee8-9adb-97678250f4e2")
    ("doom-suggest-faq"        . "id:aa28b732-0512-49ed-a47b-f20586c0f051")
    ("github"                  . "https://github.com/%s")

    ;; TODO Implement later, once docs are generalized
    ;; ("github-release"          . (lambda (link)
    ;;                                (format "%s/releases/tag/%s"
    ;;                                        doom-docs-this-repo
    ;;                                        link)))
    ;; ("github-label"            . (lambda (link)
    ;;                                (format "%s/labels/%s"
    ;;                                        doom-docs-this-repo
    ;;                                        link)))
    ;; ("github-commits"          . (lambda (link)
    ;;                                (format "%s/commits/%s/modules/%s"
    ;;                                        doom-docs-this-repo
    ;;                                        "master"
    ;;                                        link))"github-repo:/commits/%b/modules/%%s")
    ;; ("github-report"           . "github-repo:/issues/new/choose")
    ))


;;
;;; `doom-docs-mode'

(defun doom-docs--display-menu-h ()
  "Toggle virtual menu line at top of buffer."
  (setq header-line-format
        (and buffer-read-only
             (doom-docs--make-header)))
  (add-hook 'window-state-change-hook #'doom-docs--display-menu-h nil t))

(defun doom-docs--hide-meta-h ()
  "Hide all meta or comment lines."
  (org-with-wide-buffer
   (goto-char (point-min))
   (save-match-data
     (let ((case-fold-search t))
       (while (re-search-forward "^[ \t]*\\#" nil t)
         (unless (org-in-src-block-p t)
           (catch 'abort
             (org-fold-core-region
              (line-beginning-position)
              (cond ((looking-at "+\\(?:title\\|subtitle\\): +")
                     (match-end 0))
                    ((looking-at "+\\(?:created\\|since\\|author\\|email\\|date\\): +")
                     (throw 'abort nil))
                    ((or (eq (char-after) ?\s)
                         (looking-at "+\\(begin\\|end\\)_comment"))
                     (line-beginning-position 2))
                    ((looking-at "+\\(?:begin\\|end\\)_\\([^ \n]+\\)")
                     (line-end-position))
                    ((line-beginning-position 2)))
              doom-docs-mode 'doom-doc-hidden))))))))

(defun doom-docs--hide-drawers-h ()
  "Hide all property drawers."
  (let (pt)
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (looking-at-p org-drawer-regexp)
       (setq pt (org-element-property :end (org-element-at-point))))
     (while (re-search-forward org-drawer-regexp nil t)
       (when-let ((el (org-element-at-point))
                  (beg (max (point-min) (1- (org-element-property :begin el))))
                  (end (org-element-property :end el))
                  ((memq (org-element-type el) '(drawer property-drawer))))
         (when (org-element-property-inherited :level el)
           (cl-decf end))
         (org-fold-core-region beg end doom-docs-mode 'doom-doc-hidden))))
    ;; FIX: If the cursor remains within a newly folded region, that folk will
    ;;   come undone, so we move it.
    (if pt (goto-char pt))))

(defun doom-docs--hide-tags-h ()
  "Hide tags in org headings."
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward org-heading-regexp nil t)
     (when-let (tags (org-get-tags nil t))
       (when (or (member "noorg" tags)
                 (member "unfold" tags))
         ;; prevent `org-ellipsis' around hidden regions
         (org-show-entry))
       (if (member "noorg" tags)
           (org-fold-core-region (line-end-position 0)
                                 (save-excursion
                                   (org-end-of-subtree t)
                                   (forward-line 1)
                                   (if (and (bolp) (eolp))
                                       (line-beginning-position)
                                     (line-end-position 0)))
                                 doom-docs-mode 'doom-doc-hidden)
         (org-fold-core-region (save-excursion
                                 (goto-char (line-beginning-position))
                                 (re-search-forward " +:[^ ]" (line-end-position))
                                 (match-beginning 0))
                               (line-end-position)
                               doom-docs-mode 'doom-doc-hidden))))))

(defun doom-docs--hide-stars-h ()
  "Update invisible property to VISIBILITY for markers in the current buffer."
  (org-with-wide-buffer
   (goto-char (point-min))
   (with-silent-modifications
     (while (re-search-forward "^\\(\\*[ \t]\\|\\*\\*+\\)" nil t)
       (org-fold-core-region (match-beginning 0)
                             (match-end 0)
                             doom-docs-mode
                             'doom-doc-hidden)))))

(defvar doom-docs--babel-cache nil)
(defun doom-docs--hide-src-blocks-h ()
  "Hide babel blocks (and/or their results) depending on their :exports arg."
  (org-with-wide-buffer
   (let ((inhibit-read-only t))
     (goto-char (point-min))
     (make-local-variable 'doom-docs--babel-cache)
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
             (cl-pushnew beg doom-docs--babel-cache)
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
         (unless (member exports '(nil "both" "code" "t"))
           (org-fold-core-region beg end doom-docs-mode 'doom-doc-hidden))))
     (unless doom-docs-mode
       (save-excursion
         (dolist (pos doom-docs--babel-cache)
           (goto-char pos)
           (org-babel-remove-result)
           (org-element-cache-refresh pos))
         (kill-local-variable 'doom-docs--babel-cache)
         (restore-buffer-modified-p nil))))))

(defvar doom-docs--macro-cache nil)
(defun doom-docs--expand-macros-h ()
  "Expand {{{macros}}} with their value."
  (org-with-wide-buffer
    (goto-char (point-min))
    (make-local-variable 'doom-docs--macro-cache)
    (while (re-search-forward "{{{[^}]+}}}" nil t)
      (with-silent-modifications
        (if doom-docs-mode
            (when-let* ((element (org-element-context))
                        (key (org-element-property :key element))
                        (cachekey (org-element-property :value element))
                        (template (cdr (assoc-string key org-macro-templates t))))
              (let ((value (or (cdr (assoc-string cachekey doom-docs--macro-cache))
                               (setf (alist-get cachekey doom-docs--macro-cache nil nil 'equal)
                                     (org-macro-expand element org-macro-templates)))))
                (add-text-properties (match-beginning 0)
                                     (match-end 0)
                                     `(display ,value))))
          (remove-text-properties (match-beginning 0)
                                  (match-end 0)
                                  '(display))))
      (org-element-cache-refresh (point)))))

(defvar doom-docs-mode-alist
  '((flyspell-mode . -1)
    (spell-fu-mode . -1)
    (visual-line-mode . -1)
    (mixed-pitch-mode . -1)
    (variable-pitch-mode . -1))
  "An alist of minor modes to activate or deactivate in `doom-docs-mode'.

The CAR is the minor mode symbol, and CDR should be either +1 or -1,
depending.")

(defvar doom-docs--initial-values nil)
(defvar doom-docs--cookies nil)
;;;###autoload
(define-minor-mode doom-docs-mode
  "Hides metadata, tags, & drawers and activates all org-mode prettifications.
This primes `org-mode' for reading."
  :lighter " Doom Docs"
  :after-hook (org-restart-font-lock)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an org mode buffer"))
  (org-fold-add-folding-spec
   'doom-doc-hidden '(:visible nil
                      :ellipsis nil
                      :isearch-ignore t))
  (mapc (lambda (sym)
          (if doom-docs-mode
              (set (make-local-variable sym) t)
            (kill-local-variable sym)))
        '(org-pretty-entities
          org-hide-emphasis-markers
          org-hide-macro-markers))
  (when doom-docs-mode
    (make-local-variable 'doom-docs--initial-values))
  (mapc (lambda! ((face . plist))
          (if doom-docs-mode
              (push (apply #'face-remap-add-relative face plist) doom-docs--cookies)
            (mapc #'face-remap-remove-relative doom-docs--cookies)))
        '((org-document-title :weight bold :height 1.4)
          (org-document-info  :weight normal :height 1.15)))
  (mapc (lambda! ((mode . state))
          (if doom-docs-mode
              (if (and (boundp mode) (symbol-value mode))
                  (unless (> state 0)
                    (setf (alist-get mode doom-docs--initial-values) t)
                    (funcall mode -1))
                (unless (< state 0)
                  (setf (alist-get mode doom-docs--initial-values) nil)
                  (funcall mode +1)))
            (when-let (old-val (assq mode doom-docs--initial-values))
              (funcall mode (if old-val +1 -1)))))
        doom-docs-mode-alist)
  (unless doom-docs-mode
    (kill-local-variable 'doom-docs--initial-values)))

(add-hook! 'doom-docs-mode-hook
           #'doom-docs--display-menu-h
           #'doom-docs--hide-meta-h
           #'doom-docs--hide-tags-h
           #'doom-docs--hide-drawers-h
           ;; #'doom-docs--hide-stars-h
           #'doom-docs--expand-macros-h
           #'doom-docs--hide-src-blocks-h)

(defun doom-docs--toggle-read-only-h ()
  (doom-docs-mode (if buffer-read-only +1 -1)))


(defvar doom-docs--id-locations nil)
(defvar doom-docs--id-files nil)
(defvar doom-docs--id-location-file (file-name-concat doom-cache-dir "doom-docs-org-ids"))
;;;###autoload
(defun doom/reload-docs (&optional force)
  "Reload the ID locations in Doom's documentation and open docs buffers."
  (interactive (list 'interactive))
  (with-temp-buffer
    (let ((org-id-locations-file doom-docs--id-location-file)
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
      (setq doom-docs--id-files org-id-files
            doom-docs--id-locations org-id-locations)))
  (dolist (buf (doom-buffers-in-mode 'doom-docs-org-mode))
    (with-current-buffer buf
      (setq-local org-id-files doom-docs--id-files
                  org-id-locations doom-docs--id-locations))))


;;
;;; `doom-docs-org-mode'

;;;###autoload
(defun doom-docs-generate-id (&optional force?)
  "Generate an ID for a `doom-docs-org-mode' buffer."
  (let ((org-id-link-to-org-use-id t)
        (org-id-method 'uuid)
        (org-id-track-globally t)
        (org-id-locations-file doom-docs--id-location-file)
        (org-id-locations doom-docs--id-locations)
        (org-id-files doom-docs--id-files))
    (doom/reload-docs)
    (when-let (fname (buffer-file-name (buffer-base-buffer)))
      (let ((id (org-id-new)))
        (org-id-add-location id fname)
        id))))

(defconst doom-docs-org-font-lock-keywords
  '(("^\\( *\\)#\\+begin_quote\n\\1 \\([󰝗󱌣󰐃󰔓󰟶󰥔]\\) "
     2 (pcase (match-string 2)
         ("󰝗" 'font-lock-comment-face)
         ("󱌣" 'font-lock-comment-face)
         ("󰐃" 'error)
         ("󰔓" 'success)
         ("󰟶" 'font-lock-keyword-face)
         ("󰥔" 'font-lock-constant-face)
         ("" 'warning))))
  "Extra font-lock keywords for Doom documentation.")

(defvar doom-docs-org-mode-map
  (let ((map (make-sparse-keymap))
        (cmd (cmds! buffer-read-only #'kill-current-buffer)))
    (define-key map "q" cmd)
    (define-key map [remap evil-record-macro] cmd)
    map))

;;;###autoload
(define-derived-mode doom-docs-org-mode org-mode "Doom Docs"
  "A derivative of `org-mode' for Doom's documentation files.

Keeps track of its own IDs in `doom-docs-dir' and toggles `doom-docs-mode' when
`read-only-mode' is activated."
  :after-hook (visual-line-mode -1)
  (font-lock-add-keywords nil doom-docs-org-font-lock-keywords)
  (let ((gc-cons-threshold most-positive-fixnum)
        (gc-cons-percentage 1.0))
    (require 'org-id)
    (require 'ob)
    (setq-local org-id-link-to-org-use-id t
                org-id-method 'uuid
                org-id-track-globally t
                org-id-locations-file doom-docs--id-location-file
                org-id-locations doom-docs--id-locations
                org-id-files doom-docs--id-files
                org-num-max-level 3
                org-footnote-define-inline nil
                org-footnote-auto-label t
                org-footnote-auto-adjust t
                org-footnote-section nil
                wgrep-change-readonly-file t
                org-link-abbrev-alist-local (append org-link-abbrev-alist-local doom-docs-link-alist)
                org-babel-default-header-args
                (append '((:eval . "no") (:tangle . "no"))
                        org-babel-default-header-args)
                save-place-ignore-files-regexp ".")
    (unless org-inhibit-startup
      (doom/reload-docs)
      (unless (local-variable-p 'org-startup-with-inline-images)
        (setq org-display-remote-inline-images 'cache)
        (org-display-inline-images))
      (unless (local-variable-p 'org-startup-indented)
        (org-indent-mode +1))
      (unless (local-variable-p 'org-startup-numerated)
        (when (bound-and-true-p org-num-mode)
          (org-num-mode -1))
        (org-num-mode +1))
      (unless (local-variable-p 'org-startup-folded)
        (let ((org-startup-folded 'content)
              org-cycle-hide-drawer-startup)
          (org-set-startup-visibility))))
    (add-hook 'read-only-mode-hook #'doom-docs--toggle-read-only-h nil 'local)))

(defun doom-docs-init-glossary-h ()
  "Activates `org-glossary-mode', if it's available."
  (when (require 'org-glossary nil t)
    (setq-local org-glossary-global-terms (doom-glob doom-docs-dir "appendix.org"))
    (org-glossary-mode +1)))
(add-hook 'doom-docs-org-mode-hook #'doom-docs-init-glossary-h)

;;;###autoload
(defun doom-docs-read-only-h ()
  "Activate `read-only-mode' if the current file exists and is non-empty."
  ;; The rationale: if it's empty or non-existant, you want to write an org
  ;; file, not read it.
  (let ((file-name (buffer-file-name (buffer-base-buffer))))
    (when (and file-name
               (> (buffer-size) 0)
               (not (string-prefix-p "." (file-name-base file-name)))
               (file-exists-p file-name))
      (read-only-mode +1))))

(add-hook 'doom-docs-org-mode-hook #'doom-docs-read-only-h)

(provide 'doom-lib '(docs))
;;; docs.el ends here
