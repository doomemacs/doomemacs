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
  :pin "63413a5563450bdedee4c077f2f998578e75083a")

(when (featurep! +org)
  (package! org-mime :pin "eb21c02ba8f97fe69c14dc657a7883b982664649"))
(when (featurep! :lang org)
  (package! ol-notmuch :pin "126fb446d8fa9e54cf21103afaf506fd81273c02"))
(when (featurep! :completion ivy)
  (package! counsel-notmuch :pin "a4a1562935e4180c42524c51609d1283e9be0688"))
(when (featurep! :completion helm)
  (package! helm-notmuch :pin "97a01497e079a7b6505987e9feba6b603bbec288"))
(when (featurep! :completion vertico)
  (package! consult-notmuch :pin "a5133b9e1f19b6d51e51dd5c5e3a4f236ca29b57"))
