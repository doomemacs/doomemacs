;; -*- no-byte-compile: t; -*-
;;; feature/evil/test/evil.el

(require! :feature evil)

;;
;; `evil-ex-replace-special-filenames'
;; NOTE The majority of this function is tested in core/test/core-lib.el, this
;; only tests the evil-mode-specific functionality.
(def-test! file-modifiers
  (cl-flet ((do-it #'evil-ex-replace-special-filenames))
    (let ((buffer-file-name  "~/.emacs.d/test/modules/feature/test-evil.el")
          (default-directory "~/.emacs.d/test/modules/"))
      (should (equal (do-it "%:s?e?x?") "fxature/test-evil.el"))
      (should (equal (do-it "%:gs?e?x?") "fxaturx/txst-xvil.xl")))))

(def-test! empty-file-modifiers
  (cl-flet ((do-it #'evil-ex-replace-special-filenames))
    (let (buffer-file-name default-directory)
      (should (equal (do-it "%:s?e?x?") ""))
      (should (equal (do-it "%:gs?e?x?") "")))))

