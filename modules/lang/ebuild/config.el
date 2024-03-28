;;; lang/ebuild/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(after! ebuild-mode
  (set-electric! 'ebuild-mode :words '("else" "elif" "fi" "done" "then" "do" "esac" ";;"))
  (set-formatter! 'shfmt '("shfmt" "-ci"
                           (unless indent-tabs-mode
                             (list "-i" (number-to-string tab-width))))
    :modes '(ebuild-mode))
  (map! :map ebuild-mode-map
        :localleader
        :desc "Run ebuild"                      "e" #'ebuild-run-command
        :desc "Manipulate keyword"              "k" #'ebuild-mode-keyword
        :desc "Manipulate ekeyword"             "y" #'ebuild-mode-ekeyword
        :desc "Run pkgdev"                      "p" #'ebuild-mode-run-pkgdev
        :desc "Run pkgcheck"                    "q" #'ebuild-mode-run-pkgcheck
        :desc "Insert skeletion"                "n" #'ebuild-mode-insert-skeleton
        :desc "Insert tag line"                 "t" #'ebuild-mode-insert-tag-line
        :desc "Find workdir"                    "d" #'ebuild-mode-find-workdir
        :desc "Unstabilize all keywords"        "b" #'ebuild-mode-all-keywords-unstable))

(use-package! company-ebuild
  :when (modulep! :completion company)
  :after ebuild-mode
  :config
  (set-company-backend! 'ebuild-mode 'company-ebuild))

(use-package! flycheck-pkgcheck
  :when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
  :hook (ebuild-mode . flycheck-pkgcheck-setup))
