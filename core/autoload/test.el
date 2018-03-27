;;; core/autoload/test.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;;###autoload
(defun doom//run-tests (&optional modules)
  "Run all loaded tests, specified by MODULES (a list of module cons cells) or
command line args following a double dash (each arg should be in the
'module/submodule' format).

If neither is available, run all tests in all enabled modules."
  (interactive)
  (let ((doom-modules (make-hash-table :test #'equal)))
    ;; ensure DOOM is initialized
    (let (noninteractive)
      ;; Core libraries aren't fully loaded in a noninteractive session, so
      ;; we reload it with `noninteractive' set to nil to force them to.
      (load (expand-file-name "core.el" doom-core-dir) nil t t))
    (condition-case-unless-debug ex
        (let ((target-paths
               ;; Convert targets (either from MODULES or `argv') into a list of
               ;; string paths, pointing to the root directory of modules
               (cond ((string= (car argv) "--") ; command line
                      (save-match-data
                        (cl-loop for arg in (cdr argv)
                                 if (string= arg "core") collect doom-core-dir
                                 else if (string-match-p "/" arg)
                                 nconc (cl-loop for dir in doom-modules-dirs
                                                collect (expand-file-name arg dir))
                                 else
                                 nconc (cl-loop for dir in doom-modules-dirs
                                                for path = (expand-file-name arg dir)
                                                if (file-directory-p path)
                                                nconc
                                                (cl-remove-if-not
                                                 #'file-directory-p
                                                 (directory-files path t "^[^.]" t)))
                                 finally do (setq argv nil))))

                     (modules ; cons-cells given to MODULES
                      (cl-loop for (module . submodule) in modules
                               if (doom-module-find-path module submodule)
                               collect it))

                     ((let (noninteractive)
                        (load (expand-file-name "init.test.el" user-emacs-directory) nil t)
                        (append (list doom-core-dir) (doom-module-load-path)))))))
          ;; Load all the unit test files...
          (dolist (path target-paths)
            (let ((test-path (expand-file-name "test/" path)))
              (when (file-directory-p test-path)
                (dolist (test-file (reverse (doom-packages--files test-path "\\.el$")))
                  (load test-file nil :noerror)))))
          ;; ... then run them
          (if noninteractive
              (ert-run-tests-batch-and-exit)
            (call-interactively #'ert-run-tests-interactively)))
      ('error
       (lwarn 'doom-test :error
              "%s -> %s"
              (car ex) (error-message-string ex))))))


;; --- Test helpers -----------------------

(defmacro def-test! (name &rest body)
  "Define a namespaced ERT test."
  (declare (indent defun) (doc-string 2))
  (let (plist)
    (while (keywordp (car body))
      (push (pop body) plist)
      (push (pop body) plist))
    (setq plist (reverse plist))
    (when-let* ((modes (doom-enlist (plist-get plist :minor-mode))))
      (dolist (mode modes)
        (setq body `((with-minor-mode! ,mode ,@body)))))
    (when-let* ((before (plist-get plist :before)))
      (setq body `(,@before ,@body)))
    (when-let* ((after (plist-get plist :after)))
      (setq body `(,@body @after)))
    `(ert-deftest
         ,(intern (format "%s::%s"
                          (if (file-in-directory-p load-file-name doom-core-dir)
                              (format "core/%s" (file-name-base load-file-name))
                            (replace-regexp-in-string "^.*/modules/\\([^/]+\\)/\\([^/]+\\)/test/" "\\1/\\2:"
                                                      (file-name-sans-extension load-file-name)))
                          name))
         ()
       ,(if (plist-get plist :skip)
            `(ert-skip ,(plist-get plist :skip))
          `(with-temp-buffer
             (save-mark-and-excursion
               (save-window-excursion
                 ,@body)))))))

(defmacro should-buffer! (initial expected &rest body)
  "Test that a buffer with INITIAL text, run BODY, then test it against EXPECTED.

INITIAL will recognize cursor markers in the form {[0-9]}. A {0} marker marks
where the cursor should be after setup. Otherwise, the cursor will be placed at
`point-min'.

EXPECTED will recognize one (optional) cursor marker: {|}, this is the
'expected' location of the cursor after BODY is finished, and will be tested
against."
  (declare (indent 2))
  `(with-temp-buffer
     (cl-loop for line in ',initial
              do (insert line "\n"))
     (goto-char (point-min))
     (let (marker-list)
       (save-excursion
         (while (re-search-forward "{\\([0-9]\\)}" nil t)
           (push (cons (match-string 1)
                       (set-marker (make-marker) (match-beginning 0)))
                 marker-list)
           (replace-match "" t t))
         (if (not marker-list)
             (goto-char (point-min))
           (sort marker-list
                 (lambda (m1 m2) (< (marker-position m1)
                               (marker-position m2))))
           (when (equal (caar marker-list) "0")
             (goto-char! 0)))
         ,@body
         (let ((result-text (buffer-substring-no-properties (point-min) (point-max)))
               (point (point))
               same-point
               expected-text)
           (with-temp-buffer
             (cl-loop for line in ',expected
                      do (insert line "\n"))
             (save-excursion
               (goto-char 1)
               (when (re-search-forward "{|}" nil t)
                 (setq same-point (= point (match-beginning 0)))
                 (replace-match "" t t)))
             (setq expected-text (buffer-substring-no-properties (point-min) (point-max)))
             (should (equal expected-text result-text))
             (should same-point)))))))

(defmacro goto-char! (index)
  "Meant to be used with `should-buffer!'. Will move the cursor to one of the
cursor markers. e.g. Go to marker {2} with (goto-char! 2)."
  `(goto-char (point! ,index)))

(defmacro point! (index)
  "Meant to be used with `should-buffer!'. Returns the position of a cursor
marker. e.g. {2} can be retrieved with (point! 2)."
  `(cdr (assoc ,(cond ((numberp index) (number-to-string index))
                      ((symbolp index) (symbol-name index))
                      ((stringp index) index))
               marker-list)))

(defmacro with-minor-mode! (mode &rest body)
  "Activate a minor mode while in BODY, deactivating it after."
  (declare (indent defun))
  `(progn (,mode +1)
          ,@body
          (,mode -1)))

(defmacro let-advice! (binds &rest body)
  "Temporarily bind advice in BINDS while in BODY.

e.g. (old-fn :before advice-fn)
     (old-fn :around advice-fn)"
  (declare (indent defun))
  `(progn
     ,@(cl-loop for (target type advice) in binds
                collect `(advice-add #',target ,type #',advice))
     ,@body
     ,@(cl-loop for (target type advice) in binds
                collect `(advice-remove #',target #',advice))))
