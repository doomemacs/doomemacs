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
  :pin "dfc800c26e7bee1e42a8d96c300508ed9d5a109b")

(when (modulep! +org)
  (package! org-mime :pin "9571c148eed5e86fdd54eb6bf2814947c2c745a6"))
(when (modulep! :lang org)
  (package! ol-notmuch :pin "9a69506a3f9ed31e2f1f967dfaa600702089be45"))
(when (modulep! :completion ivy)
  (package! counsel-notmuch :pin "a4a1562935e4180c42524c51609d1283e9be0688"))
(when (modulep! :completion helm)
  (package! helm-notmuch :pin "97a01497e079a7b6505987e9feba6b603bbec288"))
(when (modulep! :completion vertico)
  (package! consult-notmuch :pin "17d2a4ea8c180acd4fb805dcc1b17d9c6a1a4044"))
