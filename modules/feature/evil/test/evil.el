;; -*- no-byte-compile: t; -*-
;;; feature/evil/test/evil.el

(require! :feature evil)

;; `+evil*ex-replace-special-filenames'
(def-test! file-modifiers
  (cl-flet ((do-it #'+evil*ex-replace-special-filenames))
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
                     (file-truename buffer-file-name))))))

(def-test! nested-file-modifiers
  (cl-flet ((do-it #'+evil*ex-replace-special-filenames))
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
      (should (equal (do-it "%:t:r")   "version")))))

(def-test! empty-file-modifiers
  (cl-flet ((do-it #'+evil*ex-replace-special-filenames))
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
      (should (equal (do-it "%:P") "")))))

