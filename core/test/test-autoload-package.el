;; -*- no-byte-compile: t; -*-
;;; core/test/test-autoload-package.el

(describe "core/autoload/packages"
  :var (package-alist
        package-archive-contents
        package-selected-packages
        doom-packages
        quelpa-cache
        quelpa-initialized-p
        doom-packages-dir
        doom-core-packages
        package-user-dir
        quelpa-dir
        pkg)

  (before-all
    (fset 'pkg
          (lambda (name version &optional reqs)
            (package-desc-create
             :name name :version version :reqs reqs
             :dir (expand-file-name (format "%s/" name) package-user-dir))))
    (require 'package)
    (require 'quelpa)
    (setq doom-packages-dir (expand-file-name "packages/" (file-name-directory load-file-name))
          package-user-dir (expand-file-name "elpa" doom-packages-dir)
          quelpa-dir (expand-file-name "quelpa" doom-packages-dir)
          quelpa-initialized-p t
          doom-core-packages nil)
    (spy-on #'package--user-installed-p :and-call-fake (lambda (_p) t))
    (spy-on #'doom-initialize-packages :and-call-fake (lambda (&optional _)))
    (spy-on #'package-refresh-contents :and-call-fake (lambda (&optional _)))
    (spy-on #'quelpa-checkout :and-call-fake
            (lambda (rcp _dir)
              (when (eq (car rcp) 'doom-quelpa-dummy)
                "20170405.1234"))))

  (after-all
    (unload-feature 'package t)
    (unload-feature 'quelpa t))

  (before-each
    (setq package-alist
          `((doom-dummy ,(pkg 'doom-dummy '(20160405 1234)))
            (doom-uptodate-dummy ,(pkg 'doom-uptodate-dummy '(20160605 1234)))
            (doom-unwanted-dummy ,(pkg 'doom-unwanted-dummy '(20160605 1234)))
            (doom-quelpa-dummy ,(pkg 'doom-quelpa-dummy '(20160405 1234)))
            (doom-noquelpa-dummy ,(pkg 'doom-noquelpa-dummy '(20160405 1234))))
          package-archive-contents
          `((doom-dummy ,(pkg 'doom-dummy '(20170405 1234)))
            (doom-uptodate-dummy ,(pkg 'doom-uptodate-dummy '(20160605 1234))))
          doom-packages
          '((doom-dummy)
            (doom-uptodate-dummy)
            (doom-missing-dummy)
            (doom-noquelpa-dummy)
            (doom-disabled-dummy :disable t)
            (doom-private-dummy :private t)
            (doom-disabled-private-dummy :private t :disable t)
            (doom-quelpa-dummy :recipe (doom-quelpa-dummy :fetcher github :repo "hlissner/does-not-exist")))
          quelpa-cache
          '((doom-quelpa-dummy :fetcher github :repo "hlissner/does-not-exist")
            (doom-noquelpa-dummy :fetcher github :repo "hlissner/does-not-exist-3")
            (doom-new-quelpa-dummy :fetcher github :repo "hlissner/does-not-exist-2"))
          package-selected-packages (mapcar #'car doom-packages)))

  (describe "package-backend"
    (it "determines the correct backend of a package"
      (expect (doom-package-backend 'doom-dummy) :to-be 'elpa)
      (expect (doom-package-backend 'doom-quelpa-dummy) :to-be 'quelpa)
      (expect (doom-package-backend 'org) :to-be 'emacs))
    (it "errors out if package isn't installed"
      (expect (doom-package-backend 'xyz) :to-throw)))

  (describe "package-outdated-p (elpa)"
    (it "detects outdated ELPA packages and returns both versions"
      (expect (doom-package-outdated-p 'doom-dummy)
              :to-equal '(doom-dummy (20160405 1234) (20170405 1234))))
    (it "ignores up-to-date ELPA packages"
      (expect (doom-package-outdated-p 'doom-uptodate-dummy) :to-be nil))

    (it "detects outdated QUELPA packages and returns both versions"
      (expect (doom-package-outdated-p 'doom-quelpa-dummy)
              :to-equal '(doom-quelpa-dummy (20160405 1234) (20170405 1234))))
    (it "ignores up-to-date QUELPA packages"
      (expect (doom-package-outdated-p 'doom-uptodate-dummy) :to-be nil))

    (it "returns nil if package isn't installed"
      (expect (doom-package-outdated-p 'xyz) :to-be nil)))

  (describe "get-packages"
    (before-all
      ;; In addition to `package-installed-p', `doom-package-installed-p' does
      ;; file existence checks which won't work here, so we simplify it
      (spy-on #'doom-package-installed-p :and-call-fake #'package-installed-p))

    (it "returns all packages"
      (expect (mapcar #'car (doom-get-packages))
              :to-have-same-items-as
              (mapcar #'car doom-packages)))
    (it "returns only disabled packages"
      (expect (mapcar #'car (doom-get-packages :disabled t))
              :to-have-same-items-as
              '(doom-disabled-dummy doom-disabled-private-dummy)))
    (it "returns only non-disabled packages"
      (expect (mapcar #'car (doom-get-packages :disabled nil))
              :to-have-same-items-as
              '(doom-dummy doom-uptodate-dummy doom-quelpa-dummy doom-missing-dummy doom-noquelpa-dummy doom-private-dummy)))
    (it "returns only installed packages"
      (expect (mapcar #'car (doom-get-packages :disabled nil :installed t))
              :to-have-same-items-as
              '(doom-dummy doom-uptodate-dummy doom-quelpa-dummy doom-noquelpa-dummy)))
    (it "returns only non-installed packages"
      (expect (mapcar #'car (doom-get-packages :disabled nil :installed nil))
              :to-have-same-items-as
              '(doom-missing-dummy doom-private-dummy)))
    (it "returns only private packages"
      (expect (mapcar #'car (doom-get-packages :private t))
              :to-have-same-items-as
              '(doom-private-dummy doom-disabled-private-dummy)))
    (it "returns only disabled and private packages"
      (expect (mapcar #'car (doom-get-packages :disabled t :private t))
              :to-have-same-items-as
              '(doom-disabled-private-dummy))))

  (describe "get-orphaned-packages"
    (it "returns orphaned packages"
      (expect (doom-get-orphaned-packages) :to-contain 'doom-unwanted-dummy))
    (it "returns packages that have changed backends"
      (expect (doom-get-orphaned-packages) :to-contain 'doom-noquelpa-dummy)))

  (describe "get-missing-packages"
    (it "returns packages that haven't been installed"
      (expect (mapcar #'car (doom-get-missing-packages))
              :to-contain 'doom-missing-dummy))
    (it "returns packages that have changed backends"
      (expect (mapcar #'car (doom-get-missing-packages))
              :to-contain 'doom-noquelpa-dummy))))
