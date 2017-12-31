;; -*- no-byte-compile: t; -*-
;;; core/test/autoload-package.el

(defun -pkg (name version &optional reqs)
  (package-desc-create :name name :version version :reqs reqs))

(defmacro with-packages!! (packages package-descs &rest body)
`(let* ((doom-packages-dir ,(expand-file-name "packages/" (file-name-directory load-file-name)))
        (package-user-dir ,(expand-file-name "elpa" doom-packages-dir))
        (quelpa-dir ,(expand-file-name "quelpa" doom-packages-dir)))
   ;; (make-directory doom-packages-dir t)
   (let ((doom-packages ,packages)
         (package-alist ,package-descs)
         doom-core-packages)
     (cl-letf (((symbol-function 'doom-initialize-packages) (lambda (&rest _))))
       ,@body))
   ;; (delete-directory doom-packages-dir t)
   ))


;;
;; Tests
;;

(def-test! backend-detection
  (let ((package-alist `((doom-dummy ,(-pkg 'doom-dummy '(20160405 1234)))))
        (quelpa-cache '((doom-quelpa-dummy :fetcher github :repo "hlissner/does-not-exist")))
        (quelpa-initialized-p t))
    (should (eq (doom-package-backend 'doom-dummy) 'elpa))
    (should (eq (doom-package-backend 'doom-quelpa-dummy) 'quelpa))
    (should (eq (doom-package-backend 'org) 'emacs))))

(def-test! elpa-outdated-detection
  (let* ((doom--last-refresh (current-time))
         (package-alist
          `((doom-dummy ,(-pkg 'doom-dummy '(20160405 1234)))))
         (package-archive-contents
          `((doom-dummy ,(-pkg 'doom-dummy '(20170405 1234))))))
    (cl-letf (((symbol-function 'package-refresh-contents) (lambda (&rest _))))
      (should (equal (doom-package-outdated-p 'doom-dummy)
                     '(doom-dummy (20160405 1234) (20170405 1234)))))))

;; TODO quelpa-outdated-detection

(def-test! get-packages
  (let ((quelpa-initialized-p t))
    (with-packages!!
     '((doom-dummy))
     '((doom-dummy          nil)
       (doom-dummy-unwanted nil)
       (doom-dummy-dep      nil))
     (should (equal (doom-get-packages) '((doom-dummy)))))))

(def-test! orphaned-packages
  "Test `doom-get-orphaned-packages', which gets a list of packages that are
no longer enabled or depended on."
  (with-packages!!
   '((doom-dummy))
   `((doom-dummy          ,(-pkg 'doom-dummy '(20160405 1234) '((doom-dummy-dep (1 0)))))
     (doom-dummy-unwanted ,(-pkg 'doom-dummy-unwanted '(20160601 1234)))
     (doom-dummy-dep      ,(-pkg 'doom-dummy-dep '(20160301 1234))))
   (should (equal (doom-get-orphaned-packages) '(doom-dummy-unwanted)))))

(def-test! missing-packages
  "Test `doom-get-missing-packages, which gets a list of enabled packages that
aren't installed."
  (with-packages!!
   '((doom-dummy) (doom-dummy-installed))
   `((doom-dummy-installed ,(-pkg 'doom-dummy-installed '(20160405 1234))))
   (should (equal (doom-get-missing-packages) '((doom-dummy))))))
