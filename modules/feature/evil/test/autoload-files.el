;; -*- no-byte-compile: t; -*-
;;; feature/evil/test/autoload-files.el

(defmacro -with-temp-files! (src dest &rest body)
  "Run FORMS in the context of a temporary package setup (as in, it won't
affects your Emacs packages)."
  (declare (indent 2) (doc-string 3))
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
(def-test! move-this-file
  ":mv"
  (-with-temp-files! "/tmp/doom-buffer" "/tmp/doom-buffer-new"
    (should-error (+evil:move-this-file it))
    (should (+evil:move-this-file other t))
    (should (file-exists-p other))
    (should (not (file-exists-p it)))))

(def-test! copy-this-file
  ":cp"
  (-with-temp-files! "/tmp/doom-buffer-2" "/tmp/doom-buffer-2-new"
    (should-error (+evil:copy-this-file it))
    (should (+evil:copy-this-file other t))
    (should (file-exists-p other))
    (should (file-exists-p it))))

(def-test! delete-this-file
  ":rm"
  (-with-temp-files! "/tmp/doom-buffer-3" nil
    (should-error (+evil:delete-this-file "this-file-does-not-exist"))
    (should (+evil:delete-this-file nil t))
    (should (not (file-exists-p it)))))

