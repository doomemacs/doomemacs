;;; lang/org/cli.el -*- lexical-binding: t; -*-

(defcli! () ()
  "Commands to invoke Org's powerful capabilities."
  :partial t)


(defcli! (tangle)
  ((all?    ("-a" "--all") "Tangle all src blocks, unconditionally")
   (print?  ("-p" "--print") "Print the tangled results to stdout (implies -q/--quiet)")
   (quiet?  ("-q" "--quiet") "Don't log any status messages to stdout")
   (lang    ("-l" "--lang" lang))
   &multiple
   (tags    ("-t" "--tag" "--and" "--or" tag) "Target blocks under headers with specific tags")
   &args paths)
  "Tangle an org file in `PATHS'.

`PATHS' can be files or folders (which are searched for org files,
recursively).

EXAMPLES:
  %p %c some-file.org
  %p %c literate/config/
  %p %c `-p' `-l' sh scripts.org > script.sh
  %p %c `-p' `-l' python `-t' tagA `-t' tagB file.org | python"
  (unless paths
    (user-error "No paths to org files provided."))
  ;; Prefer module's version of org, if available.
  ;; TODO: Handle this upstream.
  (add-to-list
   'load-path
   (cl-find-if #'file-exists-p
               (list (doom-path straight-base-dir "straight" straight-build-dir "org")
                     (doom-path straight-base-dir "straight" "repos" "org"))))
  (require 'org)
  (require 'ox)
  (require 'ob-tangle)
  (letf! ((defun org-babel-tangle-collect-blocks (&optional language tangle-file)
            "Ignore blocks that are in trees with the :notangle: tag."
            (let ((counter 0) last-heading-pos blocks)
              (org-babel-map-src-blocks (buffer-file-name)
                (let ((current-heading-pos
                       (if (org-element--cache-active-p)
                           (or (org-element-property :begin (org-element-lineage (org-element-at-point) '(headline) t)) 1)
                         (org-with-wide-buffer
                          (org-with-limited-levels (outline-previous-heading))))))
                  (if (eq last-heading-pos current-heading-pos) (cl-incf counter)
                    (setq counter 1)
                    (setq last-heading-pos current-heading-pos)))
                (unless (or (org-in-commented-heading-p)
                            (org-in-archived-heading-p))
                  (let* ((tags (org-get-tags-at))
                         (info (org-babel-get-src-block-info 'no-eval))
                         (src-lang (nth 0 info))
                         (src-tfile (cdr (assq :tangle (nth 2 info)))))
                    (cond ((member "notangle" tags))

                          ((let* ((tags (seq-group-by (fn! (equal (car %) "--or")) tags))
                                  (or-tags  (mapcar #'cdr (cdr (assq t tags))))
                                  (and-tags (mapcar #'cdr (cdr (assq nil tags))))
                                  (all-tags (append or-tags and-tags)))
                             (and (or or-tags and-tags)
                                  (or (not and-tags)
                                      (let ((a (cl-intersection and-tags all-tags :test #'string=))
                                            (b and-tags))
                                        (not (or (cl-set-difference a b :test #'equal)
                                                 (cl-set-difference b a :test #'equal)))))
                                  (or (not or-tags)
                                      (cl-intersection or-tags all-tags :test #'string=))
                                  t)))

                          ((or (not src-tfile)
                               (string= src-tfile "no")  ; tangle blocks by default
                               (if tangle-file (not (equal tangle-file src-tfile)))
                               (if language (not (string= language src-lang)))))

                          ;; Add the spec for this block to blocks under its language.
                          ((let* ((block (org-babel-tangle-single-block counter))
                                  (src-tfile (cdr (assq :tangle (nth 4 block))))
                                  (file-name (org-babel-effective-tangled-filename
                                              (nth 1 block) src-lang src-tfile))
                                  (by-fn (assoc file-name blocks)))
                             (if by-fn
                                 (setcdr by-fn (cons (cons src-lang block) (cdr by-fn)))
                               (push (cons file-name (list (cons src-lang block)))
                                     blocks))))))))
              ;; Ensure blocks are in the correct order.
              (mapcar (lambda (b) (cons (car b) (nreverse (cdr b))))
                      (nreverse blocks))))
          (success nil))
    (if print? (setq quiet? t))
    (when (and all? (not quiet?))
      (print! (warn "Tangling all blocks, unconditionally...")))
    (dolist (file (cl-loop for path in (mapcar #'expand-file-name paths)
                           if (file-directory-p path)
                           append (doom-files-in path :type 'files :match "\\.org\\'")
                           else if (file-exists-p path)
                           collect path
                           else do (print! (error "Can't find %s. Skipping..." (path path))))
                  (or success (exit! 1)))
      (unless quiet?
        (print! (start "Reading %s...") (path file)))
      (let ((backup (make-temp-file (file-name-base file) nil ".backup.org"))
            ;; Prevent slow initialization from interfering
            (org-startup-indented nil)
            (org-startup-folded nil)
            (vc-handled-backends nil)
            ;; Prevent unwanted entries in recentf, or formatters, or
            ;; anything that could be on these hooks, really. Nothing else
            ;; should be touching these files (particularly in interactive
            ;; sessions).
            (write-file-functions nil)
            (before-save-hook nil)
            (after-save-hook nil)
            ;; Prevent infinite recursion due to recompile-on-save hooks
            ;; later, and speed up `org-mode' init.
            (org-mode-hook nil)
            (org-inhibit-startup t)
            ;; Allow evaluation of src blocks at tangle-time (would abort
            ;; them otherwise). This is a security hazard, but Doom will
            ;; trust that you know what you're doing!
            (org-confirm-babel-evaluate nil)
            ;; Tangle everything by default.
            (org-babel-default-header-args (copy-sequence org-babel-default-header-args)))
        (when all?
          (setf (alist-get :tangle org-babel-default-header-args) "yes"))
        (unwind-protect
            (progn
              ;; Do the ol' switcheroo because `org-babel-tangle' writes changes
              ;; to the current file, which would be imposing on the user.
              (copy-file file backup t)
              (with-current-buffer (delay-mode-hooks (find-file-noselect file))
                ;; Tangling doesn't expand #+INCLUDE directives, so we do it
                ;; ourselves, since includes are so useful for literate configs!
                (org-export-expand-include-keyword)
                (if-let* ((results (reverse (org-babel-tangle nil nil lang))))
                    (dolist (file results)
                      (if (not quiet?)
                          (print-group!
                            (setq success t)
                            (print! (success "Tangled to %s") (path file)))
                        (when print?
                          (print! "%s" (doom-file-read file))
                          (delete-file file))))
                  (unless quiet?
                    (print-group!
                      (print! (warn "Nothing to tangle from %s") (path file)))))))
          (ignore-errors (copy-file backup file t))
          (ignore-errors (delete-file backup)))))))
