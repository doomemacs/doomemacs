;;; lang/org/init.el -*- lexical-binding: t; -*-

;; HACK A necessary hack because org requires a compilation step after being
;; cloned, and during that compilation a org-version.el is generated with these
;; two functions, which return the output of a 'git describe ...' call in the
;; repo's root. Of course, this command won't work in a sparse clone, and more
;; than that, initiating these compilation step is a hassle, so...
(fset 'org-release #'ignore)
(fset 'org-git-version #'ignore)
