;; -*- no-byte-compile: t; -*-
;;; email/notmuch/packages.el

(package! notmuch
  :recipe (:pre-build
           (with-temp-file "emacs/notmuch-version.el"
             (insert-file-contents "emacs/notmuch-version.el.tmpl")
             (re-search-forward "%VERSION%")
             (replace-match
              (format "\"%s+%s~%.7s\""
                      (with-temp-buffer (insert-file-contents "version.txt")
                                        (string-trim (buffer-string)))
                      (save-match-data
                        (let ((desc (doom-call-process "git" "describe" "--abbrev=7" "--match" "[0-9.]*")))
                          (if (zerop (car desc))
                              (car (last (split-string (cdr desc) "-") 2))
                            "??")))
                      (cdr (doom-call-process "git" "rev-parse" "HEAD")))
              t t)))
  :pin "b6f144abe1f5aa3519240cf52f4cb9907fefcd0e")

(when (modulep! +org)
  (package! org-mime :pin "d368bd4119bfcf2997a6a23bbf5f41e043164d29"))
(when (modulep! :lang org)
  (package! ol-notmuch :pin "781c3518a537da2a8b5e8a4424f9441df463a147"))
(when (modulep! :completion ivy)
  (package! counsel-notmuch :pin "a4a1562935e4180c42524c51609d1283e9be0688"))
(when (modulep! :completion helm)
  (package! helm-notmuch :pin "97a01497e079a7b6505987e9feba6b603bbec288"))
(when (modulep! :completion vertico)
  (package! consult-notmuch :pin "d0d4129d45ccceddaeeaa3631eb42d5dd09a758b"))
