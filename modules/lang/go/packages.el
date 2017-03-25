;; -*- no-byte-compile: t; -*-
;;; lang/go/packages.el

(package! go-mode)
(package! go-eldoc)
(package! go-guru)
(package! gorepl-mode)

(when (featurep! :completion company)
  (package! company-go))

;;
(def-bootstrap! go
  (let ((gobin  (executable-find "go"))
        (gopath (getenv "GOPATH"))
        changed)
    (unless gobin
      (pcase (doom-system-os)
        ('arch
         (sudo "pacman --noconfirm -S go"))
        ('debian) ;; TODO
        ('macos
         (sh "brew install go")))
      (unless (executable-find "go")
        (error "Go isn't installed (%s)" gobin)))
    (unless (file-directory-p gopath)
      (error "GOPATH isn't set up (%s)" gopath))
    (mapc (lambda (url)
          (unless (file-directory-p (expand-file-name (concat "src/" url) gopath))
            (sh "%s get -u '%s'" gobin url)
            (setq changed t)))
        '("github.com/nsf/gocode"
          "github.com/motemen/gore"
          "golang.org/x/tools/cmd/guru"
          "golang.org/x/tools/cmd/gorename"))))
