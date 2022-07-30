;; -*- no-byte-compile: t; -*-
;;; core/test/test-autoload-files.el

(describe "core/autoload/files"

  (load! "autoload/files" doom-core-dir)

  (describe "library"
    (describe "file-exists-p!"
      (it "is a (quasi) drop-in replacement for `file-exists-p'"
        (let ((default-directory doom-emacs-dir)
              (init-file "init.el"))
          (expect (file-exists-p "init.el"))
          (expect (and (file-exists-p! "init.el")
                       (file-exists-p "init.el")))
          (expect (and (file-exists-p! init-file)
                       (file-exists-p init-file)))
          (expect (and (file-exists-p! doom-emacs-dir)
                       (file-exists-p doom-emacs-dir)))
          (expect (and (not (file-exists-p! "/cant/possibly/exist/please/dont/exist"))
                       (not (file-exists-p "/cant/possibly/exist/please/dont/exist"))))))

      (it "returns the file path if it exists"
        (expect (file-exists-p! "init.example.el"
                                doom-emacs-dir)
                :to-equal (expand-file-name "init.example.el" doom-emacs-dir)))

      (it "understands compound statements"
        (let ((default-directory doom-emacs-dir))
          (expect (file-exists-p! (and "init.el" "init.example.el")))
          (expect (file-exists-p! (or "doesnotexist" "init.example.el")))
          (expect (not (file-exists-p! (or "doesnotexist" "DOESNOTEXIST")))))
        (expect (file-exists-p! (and "init.el" "init.example.el")
                                doom-emacs-dir))
        (expect (file-exists-p! (and "init.el" "init.example.el")
                                doom-emacs-dir))
        (expect (file-exists-p! (or "doesnotexist" "init.example.el")
                                doom-emacs-dir))
        (expect (not (file-exists-p! (or "doesnotexist" "DOESNOTEXIST")
                                     doom-emacs-dir))))

      (it "understands nested compound statements"
        (expect (file-exists-p! (and "init.el" "init.example.el"
                                     (or "doesnotexist" "LICENSE"))
                                doom-emacs-dir))
        (expect (file-exists-p! (and "init.el" "init.example.el"
                                     (and "LICENSE" "README.md"
                                          (or "doesnotexist"
                                              "early-init.el")))
                                doom-emacs-dir))
        (expect (file-exists-p! (and "init.el" "init.example.el"
                                     (or "edoesnotexist" "DOESNOTEXIST"
                                         (and "idontexist"
                                              "doanyofusexist?")))
                                doom-emacs-dir)
                :to-be nil))

      (it "returns the last form if a compound file check succeeds"
        (expect (file-exists-p! (and "init.el" "init.example.el"
                                     (or "doesnotexist" "LICENSE"))
                                doom-emacs-dir)
                :to-equal (expand-file-name "LICENSE" doom-emacs-dir))
        (expect (file-exists-p! (and "init.el" "init.example.el"
                                     (or (or "doesnotexist" "DOESNOTEXIST")
                                         "doanyofusreallyexist"
                                         (or "cantexist" "LICENSE")))
                                doom-emacs-dir)
                :to-equal (expand-file-name "LICENSE" doom-emacs-dir)))

      (it "disregards the directory argument if given absolute path"
        (expect (file-exists-p! "/tmp" "/directory/that/doesnt/exist"))
        (expect (file-exists-p! doom-core-dir "/directory/that/doesnt/exist"))
        (expect (file-exists-p! (and "/tmp" doom-core-dir) "/directory/that/doesnt/exist"))
        (expect (file-exists-p! (or "/tmp" doom-core-dir) "/directory/that/doesnt/exist")))

      (it "interpolates variables"
        (let ((file-1 "init.el")
              (file-2 "init.example.el")
              (file-3 "LICENSE")
              (file-404 "doesnotexistlikenoreally"))
          (expect (file-exists-p! file-1 doom-emacs-dir))
          (expect (file-exists-p! (and file-1 file-2) doom-emacs-dir))
          (expect (file-exists-p! (and file-1 (or file-404 file-2)) doom-emacs-dir))
          (expect (file-exists-p! (or (and file-404 file-2) (and file-3 file-1))
                                  doom-emacs-dir))))

      (it "interpolates forms"
        (cl-letf (((symbol-function 'getfilename)
                   (lambda () "init.example.el")))
          (expect (file-exists-p! (and (or (if nil "init.el" "doesnotexist")
                                           (getfilename))
                                       "LICENSE")
                                  doom-emacs-dir)
                  :to-equal (expand-file-name "LICENSE" doom-emacs-dir)))))

    ;; TODO
    (xdescribe "doom-glob")
    (xdescribe "doom-path")
    (xdescribe "doom-dir")
    (xdescribe "doom-files-in")
    (xdescribe "doom-file-size")
    (xdescribe "doom-directory-size")
    (xdescribe "doom-file-cookie-p"))

  (describe "interactive file operations"
    :var (src dest projectile-projects-cache-time projectile-projects-cache)

    (require 'core-projects)
    (require 'projectile)

    (before-each
      (setq src      (make-temp-file "test-src")
            existing (make-temp-file "test-existing")
            dest     (expand-file-name "test-dest" temporary-file-directory))
      (quiet! (find-file-literally src))
      (spy-on 'y-or-n-p :and-return-value nil)
      (projectile-mode +1))

    (after-each
      (projectile-mode -1)
      (switch-to-buffer (doom-fallback-buffer))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file existing))
      (ignore-errors (delete-file dest)))

    (describe "move-this-file"
      (it "won't move to itself"
        (expect (quiet! (doom/move-this-file src)) :to-throw))
      (it "will move to another file"
        (expect (quiet! (doom/move-this-file dest t)))
        (expect (file-exists-p dest))
        (expect (file-exists-p src) :to-be nil))
      (it "will prompt if overwriting a file"
        (quiet! (doom/move-this-file existing))
        (expect 'y-or-n-p :to-have-been-called-times 1)
        (expect (file-exists-p src))))

    (describe "copy-this-file"
      (it "refuses to copy to itself"
        (expect (quiet! (doom/copy-this-file src)) :to-throw))
      (it "copies to another file"
        (expect (quiet! (doom/copy-this-file dest t)))
        (expect (file-exists-p! src dest)))
      (it "prompts if overwriting a file"
        (quiet! (doom/copy-this-file existing))
        (expect 'y-or-n-p :to-have-been-called-times 1)))

    (describe "delete-this-file"
      (it "fails gracefully on non-existent files"
        (expect (quiet! (doom/delete-this-file dest)) :to-throw))
      (it "deletes existing files"
        (quiet! (doom/delete-this-file existing t))
        (expect (file-exists-p existing) :to-be nil))
      (it "prompts to delete any existing file"
        (quiet! (doom/delete-this-file existing))
        (expect 'y-or-n-p :to-have-been-called-times 1))))

  (xdescribe "sudo {this,find} file"
    (before-each
      (spy-on 'find-file :and-return-value nil)
      (spy-on 'find-alternate-file :and-return-value nil))

    (describe "doom/sudo-find-file")
    (describe "doom/sudo-this-file")))
