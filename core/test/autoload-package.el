;; -*- no-byte-compile: t; -*-
;;; core/test/autoload-package.el

(defun -new-package (name version &optional reqs)
  (package-desc-create :name name :version version :reqs reqs))

(defmacro -with-temp-packages! (&rest forms)
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

(defmacro -with-packages! (packages package-descs &rest body)
  `(let ((doom-packages ,packages)
         (package-alist ,package-descs)
         doom-core-packages)
     (cl-letf (((symbol-function 'doom-initialize-packages) (lambda (&rest _)))
               ((symbol-function 'package-installed-p) (lambda (name &rest _) (assq name package-alist))))
       ,@body)))


;;
;; Tests
;;

(def-test! backend-detection
  (let ((package-alist `((doom-dummy ,(-new-package 'doom-dummy '(20160405 1234)))))
        (quelpa-cache '((doom-quelpa-dummy :fetcher github :repo "hlissner/does-not-exist")))
        (quelpa-initialized-p t))
    (should (eq (doom-package-backend 'doom-dummy) 'elpa))
    (should (eq (doom-package-backend 'doom-quelpa-dummy) 'quelpa))))

(def-test! elpa-outdated-detection
  (let* ((doom--last-refresh (current-time))
         (package-alist
          `((doom-dummy ,(-new-package 'doom-dummy '(20160405 1234)))))
         (package-archive-contents
          `((doom-dummy ,(-new-package 'doom-dummy '(20170405 1234))))))
    (cl-letf (((symbol-function 'package-refresh-contents) (lambda (&rest _))))
      (should (equal (doom-package-outdated-p 'doom-dummy)
                     '(doom-dummy (20160405 1234) (20170405 1234)))))))

;; TODO quelpa-outdated-detection

(def-test! get-packages
  (let ((quelpa-initialized-p t))
    (-with-packages!
     '((doom-dummy))
     '((doom-dummy          nil)
       (doom-dummy-unwanted nil)
       (doom-dummy-dep      nil))
     (should (equal (doom-get-packages) '((doom-dummy)))))))

(def-test! orphaned-packages
  "Test `doom-get-orphaned-packages', which gets a list of packages that are
no longer enabled or depended on."
  (-with-packages!
   '((doom-dummy))
   `((doom-dummy          ,(-new-package 'doom-dummy '(20160405 1234) '((doom-dummy-dep (1 0)))))
     (doom-dummy-unwanted ,(-new-package 'doom-dummy-unwanted '(20160601 1234)))
     (doom-dummy-dep      ,(-new-package 'doom-dummy-dep '(20160301 1234))))
   (should (equal (doom-get-orphaned-packages) '(doom-dummy-unwanted)))))

(def-test! missing-packages
  "Test `doom-get-missing-packages, which gets a list of enabled packages that
aren't installed."
  (-with-packages!
   '((doom-dummy) (doom-dummy-installed))
   `((doom-dummy-installed ,(-new-package 'doom-dummy-installed '(20160405 1234))))
   (should (equal (doom-get-missing-packages) '((doom-dummy))))))
