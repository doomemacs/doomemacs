;; -*- no-byte-compile: t; -*-
;;; lang/latex/packages.el

(package! auctex
  :recipe (:files ("*.el" "*.info" "dir"
                   "doc" "etc" "images" "latex" "style")
           ;; HACK: Auctex has a post-install step that generates tex-site.el
           ;;   and *.texi documentation files, which largely assumes the user
           ;;   is on a Linux system with various shell tools like sed, gnu
           ;;   make, tex, etc. -- an assumption I can't safely make about
           ;;   Doom's users, so reinvent its 'make tex-site.el' task in elisp:
           :pre-build
           (with-temp-file "tex-site.el"
             (insert-file-contents "tex-site.el.in")
             (while (re-search-forward "@\\(\\(?:AUCTEX\\|lisp\\)[^@]+\\)@" nil t)
               (pcase (match-string 1)
                 ("AUCTEXVERSION"
                  (replace-match (with-temp-buffer
                                   (insert-file-contents "auctex.el" nil 0 1024)
                                   (save-match-data
                                     (if (re-search-forward "^;; Version: \\([^\n-]+\\)" nil t)
                                         (match-string-no-properties 1)
                                       "Unknown")))
                                 t t))
                 ("AUCTEXDATE"
                  (when-let* ((time (cdr (doom-call-process "git" "log" "-n1" "--pretty=tformat:%ct")))
                              (time (string-to-number time)))
                    (replace-match (format-time-string "%Y-%m-%d %T" time) t t)))
                 ("lispautodir"
                  (replace-match
                   (prin1-to-string
                    `(if (file-writable-p "/usr/local/var/auctex")
                         "/usr/local/var/auctex"
                       ,(file-name-concat doom-data-dir "auctex")))
                   t t))
                 ((or "lisppackagedatadir" "lisppackagelispdir")
                  (replace-match "(directory-file-name (file-name-directory load-file-name))" t t))
                 (it (error "Unknown substitution variable in tex-site.el.in: %s" it))))))
  :pin "764a53c8e93150f0edd169593a4d453810792abe")
(package! adaptive-wrap :pin "dea4e32c18d285a6ca9f72b1eadd61e27a555ed3")
(package! latex-preview-pane :pin "5297668a89996b50b2b62f99cba01cc544dbed2e")
(when (modulep! :editor evil +everywhere)
  (package! evil-tex :pin "2a3177c818f106e6c11032ac261f8691f5e11f74"))

;; Optional module features.

(when (modulep! +cdlatex)
  (package! cdlatex :pin "33770dec73138909714711b05a63e79da5a19ccd"))

;; Features according to other user selected options.

(when (modulep! :completion company)
  (package! company-auctex :pin "9400a2ec7459dde8cbf1a5d50dfee4e300ed7e18")
  (package! company-reftex :pin "42eb98c6504e65989635d95ab81b65b9d5798e76")
  (package! company-math :pin "3eb006874e309ff4076d947fcbd61bb6806aa508"))
