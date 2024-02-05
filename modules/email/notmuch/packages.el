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
  :pin "2f0320c5f24adfee026e938ebc379ca90e3045d3")

(when (modulep! +org)
  (package! org-mime :pin "9d4584651d89806b79d5993b286d32d6f70499a9"))
(when (modulep! :lang org)
  (package! ol-notmuch :pin "881991d94a1ad750633fcf1f2d8a9e0616979be3"))
(when (modulep! :completion ivy)
  (package! counsel-notmuch :pin "a4a1562935e4180c42524c51609d1283e9be0688"))
(when (modulep! :completion helm)
  (package! helm-notmuch :pin "97a01497e079a7b6505987e9feba6b603bbec288"))
(when (modulep! :completion vertico)
  (package! consult-notmuch :pin "d8022e2ddc67ed4e89cc6f5bbe664fdb04e1e815"))
