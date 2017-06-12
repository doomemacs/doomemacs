;;; test/modules/feature/test-evil.el

(require! :feature evil t)

(defalias 'do-it '+evil*ex-replace-special-filenames)

(defmacro do-files! (src dest &rest body)
  (declare (indent defun))
  `(let ((it ,src)
         (other ,dest))
     (with-temp-file it
       (insert "Hello world"))

     (unwind-protect
         (progn
           (should (file-exists-p it))
           (find-file-literally it)
           (should (equal (buffer-string) "Hello world"))
           (should (equal (buffer-file-name) it))
           (let ((inhibit-message (not doom-debug-mode)))
             ,@body))
       (ignore-errors (delete-file it))
       ,(if dest `(ignore-errors (delete-file other))))))

;;
(def-test-group! feature/evil
  (ert-deftest custom-file-modifiers ()
    (let ((buffer-file-name  "~/.emacs.d/test/modules/feature/test-evil.el")
          (default-directory "~/.emacs.d/test/modules/"))
      (should (equal (do-it "%")   "feature/test-evil.el"))
      (should (equal (do-it "%:r") "feature/test-evil"))
      (should (equal (do-it "%:r.elc") "feature/test-evil.elc"))
      (should (equal (do-it "%:e") "el"))
      (should (equal (do-it "%:p") (expand-file-name buffer-file-name)))
      (should (equal (do-it "%:h") "feature"))
      (should (equal (do-it "%:t") "test-evil.el"))
      (should (equal (do-it "%:.") "feature/test-evil.el"))
      (should (equal (do-it "%:~") "~/.emacs.d/test/modules/feature/test-evil.el"))
      (should (equal (do-it "%:s?e?x?") "fxature/test-evil.el"))
      (should (equal (do-it "%:gs?e?x?") "fxaturx/txst-xvil.xl"))
      (should (equal (file-truename (do-it "%:p"))
                     (file-truename buffer-file-name)))))

  (ert-deftest custom-nested-file-modifiers ()
    (let ((buffer-file-name  "~/vim/src/version.c")
          (default-directory "~/vim/"))
      (should (equal (do-it "%:p")     (expand-file-name "~/vim/src/version.c")))
      (should (equal (do-it "%:p:.")   "src/version.c"))
      (should (equal (do-it "%:p:~")   "~/vim/src/version.c"))
      (should (equal (do-it "%:h")     "src"))
      (should (equal (do-it "%:p:h")   (expand-file-name "~/vim/src")))
      (should (equal (do-it "%:p:h:h") (expand-file-name "~/vim")))
      (should (equal (do-it "%:t")     "version.c"))
      (should (equal (do-it "%:p:t")   "version.c"))
      (should (equal (do-it "%:r")     "src/version"))
      (should (equal (do-it "%:p:r")   (expand-file-name "~/vim/src/version")))
      (should (equal (do-it "%:t:r")   "version"))))

  (ert-deftest custom-empty-file-modifiers ()
    (let (buffer-file-name default-directory)
      (should (equal (do-it "%")   ""))
      (should (equal (do-it "%:r") ""))
      (should (equal (do-it "%:e") ""))
      (should (equal (do-it "%:h") ""))
      (should (equal (do-it "%:t") ""))
      (should (equal (do-it "%:.") ""))
      (should (equal (do-it "%:~") ""))
      (should (equal (do-it "%:s?e?x?") ""))
      (should (equal (do-it "%:gs?e?x?") ""))
      (should (equal (do-it "%:P") ""))))

  (ert-deftest move-this-file ()
    (do-files! "/tmp/doom-buffer" "/tmp/doom-buffer-new"
      (should-error (+evil:move-this-file it))
      (should (+evil:move-this-file other t))
      (should (file-exists-p other))
      (should (not (file-exists-p it)))))

  (ert-deftest copy-this-file ()
    (do-files! "/tmp/doom-buffer-2" "/tmp/doom-buffer-2-new"
      (should-error (+evil:copy-this-file it))
      (should (+evil:copy-this-file other t))
      (should (file-exists-p other))
      (should (file-exists-p it))))

  (ert-deftest delete-this-file ()
    (do-files! "/tmp/doom-buffer-3" nil
      (should-error (+evil:delete-this-file "this-file-does-not-exist"))
      (should (+evil:delete-this-file nil t))
      (should (not (file-exists-p it))))))
