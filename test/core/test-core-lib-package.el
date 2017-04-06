;;; ../test/test-core-lib-package.el

(defun test-package-new (name version &optional reqs)
  (package-desc-create :name name :version version :reqs reqs))

;; TODO
(defmacro with-temp-packages! (&rest forms)
  "Run FORMS in the context of a temporary package setup (as in, it won't
affects your Emacs packages)."
  `(let* ((doom-local-dir ,(expand-file-name "test/.local/" doom-emacs-dir))
          (doom-packages-dir (concat doom-local-dir "packages/"))
          (doom-etc-dir (concat doom-local-dir "etc/"))
          (doom-cache-dir (concat doom-local-dir "cache/"))
          (package-user-dir (expand-file-name "elpa" doom-packages-dir))
          package-alist
          package-archive-contents
          package-initialize)
     (package-initialize)
     ,@forms))


;;
;; Tests
;;

(def-test-group! core-lib-package
  (ert-deftest backend-detection ()
    "TODO"
    (let ((package-alist `((doom-dummy ,(test-package-new 'doom-dummy '(20160405 1234)))))
          (quelpa-cache '((doom-quelpa-dummy :fetcher github :repo "hlissner/does-not-exist")))
          (quelpa-initialized-p t))
      (should (eq (doom-package-backend 'doom-dummy) 'elpa))
      (should (eq (doom-package-backend 'doom-quelpa-dummy) 'quelpa))))

  (ert-deftest elpa-outdated-detection ()
    "TODO"
    (cl-letf (((symbol-function 'package-refresh-contents) (lambda (&rest _))))
      (let* ((doom--last-refresh (current-time))
             (package-alist
              `((doom-dummy ,(test-package-new 'doom-dummy '(20160405 1234)))))
             (package-archive-contents
              `((doom-dummy ,(test-package-new 'doom-dummy '(20170405 1234)))))
             (outdated (doom-package-outdated-p 'doom-dummy)))
        (should outdated)
        (should (equal outdated '(doom-dummy (20160405 1234) (20170405 1234)))))))

  ;; TODO quelpa-outdated-detection

  (ert-deftest get-packages ()
    "TODO"
    (let ((quelpa-initialized-p t)
          (doom-packages '((doom-dummy)))
          (package-alist
           `((doom-dummy nil)
             (doom-dummy-dep nil)))
          doom-protected-packages)
      (cl-letf (((symbol-function 'doom-initialize-packages) (lambda (&rest _))))
        (should (equal (doom-get-packages) '((doom-dummy)))))))

  (ert-deftest orphaned-packages ()
    "Test `doom-get-orphaned-packages', which gets a list of packages that are
no longer enabled or depended on."
    (let ((doom-packages '((doom-dummy)))
          (package-alist
           `((doom-dummy ,(test-package-new 'doom-dummy '(20160405 1234) '((doom-dummy-dep (1 0)))))
             (doom-dummy-unwanted ,(test-package-new 'doom-dummy-unwanted '(20160601 1234)))
             (doom-dummy-dep ,(test-package-new 'doom-dummy-dep '(20160301 1234)))))
          doom-protected-packages)
      (cl-letf (((symbol-function 'doom-initialize-packages) (lambda (&rest _))))
        (should (equal (doom-get-orphaned-packages) '(doom-dummy-unwanted))))))

  (ert-deftest missing-packages ()
    "Test `doom-get-missing-packages, which gets a list of enabled packages that
aren't installed."
    (let ((doom-packages '((doom-dummy) (doom-dummy-installed)))
          (package-alist `((doom-dummy-installed ,(test-package-new 'doom-dummy-installed '(20160405 1234)))))
          doom-protected-packages)
      (cl-letf (((symbol-function 'doom-initialize-packages) (lambda (&rest _))))
        (should (equal (doom-get-missing-packages) '((doom-dummy))))))))
