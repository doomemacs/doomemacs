;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; test/lisp/lib/test-packages.el --- Unit tests for lisp/lib/packages.el

;;; Commentary:
;; Unit tests for Doom's package management functions.
;; Run with: emacs --batch -l early-init.el -l test/lisp/lib/test-packages.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

;; Load the packages library
(doom-require 'doom-lib 'packages)

;;; Test fixtures and helpers

(defvar test-packages--mock-packages
  '((pkg-a :recipe (:host github :repo "user/pkg-a") :modules ((:core)))
    (pkg-b :recipe (:host gitlab :repo "user/pkg-b") :pin "abc1234" :modules ((:lang . python)))
    (pkg-c :disable t :modules ((:ui)))
    (pkg-d :ignore t)
    (pkg-e :type core :modules ((:doom))))
  "Mock packages for testing.")

(defvar test-packages--original-doom-packages nil
  "Storage for original doom-packages during tests.")

(defmacro with-mock-packages (&rest body)
  "Execute BODY with mock packages in `doom-packages'."
  `(let ((test-packages--original-doom-packages doom-packages))
     (unwind-protect
         (progn
           (setq doom-packages test-packages--mock-packages)
           ,@body)
       (setq doom-packages test-packages--original-doom-packages))))


;;; ============================================================
;;; Tests for Package Property Functions
;;; ============================================================

(ert-deftest doom-package-get/returns-plist-when-no-prop ()
  "doom-package-get returns full plist when PROP is nil."
  (with-mock-packages
   (let ((result (doom-package-get 'pkg-a)))
     (should (listp result))
     (should (plist-get result :recipe))
     (should (plist-get result :modules)))))

(ert-deftest doom-package-get/returns-specific-property ()
  "doom-package-get returns specific property value."
  (with-mock-packages
   (should (equal (doom-package-get 'pkg-b :pin) "abc1234"))))

(ert-deftest doom-package-get/returns-nil-for-missing-property ()
  "doom-package-get returns nil-value for missing properties."
  (with-mock-packages
   (should (null (doom-package-get 'pkg-a :nonexistent)))
   (should (equal (doom-package-get 'pkg-a :nonexistent 'default) 'default))))

(ert-deftest doom-package-get/returns-nil-for-unknown-package ()
  "doom-package-get returns nil for unknown packages."
  (with-mock-packages
   (should (null (doom-package-get 'nonexistent-package)))))

(ert-deftest doom-package-set/sets-property ()
  "doom-package-set sets a property on a package."
  (with-mock-packages
   (doom-package-set 'pkg-a :pin "newpin123")
   (should (equal (doom-package-get 'pkg-a :pin) "newpin123"))))

(ert-deftest doom-package-set/adds-new-property ()
  "doom-package-set can add a new property."
  (with-mock-packages
   (should (null (doom-package-get 'pkg-a :custom)))
   (doom-package-set 'pkg-a :custom "value")
   (should (equal (doom-package-get 'pkg-a :custom) "value"))))


;;; ============================================================
;;; Tests for Package Query Functions
;;; ============================================================

(ert-deftest doom-package-built-in-p/detects-built-in ()
  "doom-package-built-in-p correctly identifies built-in packages."
  (cl-letf (((symbol-function 'doom-package-build-recipe)
             (lambda (pkg &optional prop nil-val)
               (if (eq pkg 'emacs)
                   (if prop (when (eq prop :type) 'built-in) '(:type built-in))
                 nil))))
    (should (doom-package-built-in-p 'emacs))
    (should (null (doom-package-built-in-p 'pkg-a)))))

(ert-deftest doom-package-is-type-p/checks-type ()
  "doom-package-is-type-p checks if package has given type."
  (with-mock-packages
   (should (doom-package-is-type-p 'pkg-e 'core))
   (should (null (doom-package-is-type-p 'pkg-a 'core)))))

(ert-deftest doom-package-in-module-p/checks-module ()
  "doom-package-in-module-p checks if package belongs to module."
  (with-mock-packages
   (should (doom-package-in-module-p 'pkg-a :core))
   (should (doom-package-in-module-p 'pkg-b :lang 'python))
   (should (null (doom-package-in-module-p 'pkg-a :lang 'python)))))


;;; ============================================================
;;; Tests for Utility Functions
;;; ============================================================

(ert-deftest doom-packages--same-commit-p/matches-prefixes ()
  "doom-packages--same-commit-p matches commit prefixes."
  (should (doom-packages--same-commit-p "abc1234" "abc1234567890"))
  (should (doom-packages--same-commit-p "abc" "abc1234567890"))
  (should (null (doom-packages--same-commit-p "xyz" "abc1234567890")))
  (should (null (doom-packages--same-commit-p nil "abc1234")))
  (should (null (doom-packages--same-commit-p "abc" nil))))

(ert-deftest doom-packages--abbrev-commit/truncates ()
  "doom-packages--abbrev-commit truncates commits to 7 characters."
  (should (equal (doom-packages--abbrev-commit "abc1234567890") "abc1234"))
  (should (equal (doom-packages--abbrev-commit "1234567") "1234567")))

(ert-deftest doom-packages--abbrev-commit/full-when-requested ()
  "doom-packages--abbrev-commit returns full commit when FULL is t."
  (should (equal (doom-packages--abbrev-commit "abc1234567890" t) "abc1234567890")))


;;; ============================================================
;;; Tests for ELN/ELC Functions
;;; ============================================================

(ert-deftest doom-packages--eln-output-file/constructs-path ()
  "doom-packages--eln-output-file constructs correct path."
  (let ((doom-packages--eln-output-path "/tmp/eln-cache"))
    (should (string-match-p "eln-cache.*test\\.eln$"
                            (doom-packages--eln-output-file "test.eln")))))

(ert-deftest doom-packages--eln-error-file/constructs-path ()
  "doom-packages--eln-error-file constructs error file path."
  (let ((doom-packages--eln-output-path "/tmp/eln-cache"))
    (let ((result (doom-packages--eln-error-file "test.eln")))
      (should (string-match-p "eln-cache" result))
      (should (string-match-p "test\\.eln" result))
      (should (string-suffix-p ".error" result)))))

(ert-deftest doom-packages--elc-file-outdated-p/detects-outdated ()
  "doom-packages--elc-file-outdated-p detects when .elc is older than .el."
  (let ((temp-dir (make-temp-file "doom-test-" t)))
    (unwind-protect
        (let* ((el-file (expand-file-name "test.el" temp-dir))
               (elc-file (expand-file-name "test.elc" temp-dir)))
          ;; Create .elc first (older)
          (with-temp-file elc-file (insert ";; compiled"))
          (sleep-for 0.1)
          ;; Create .el second (newer)
          (with-temp-file el-file (insert "(message \"test\")"))
          ;; .elc should be outdated
          (should (doom-packages--elc-file-outdated-p el-file)))
      (delete-directory temp-dir t))))

(ert-deftest doom-packages--elc-file-outdated-p/not-outdated-when-newer ()
  "doom-packages--elc-file-outdated-p returns nil when .elc is newer."
  (let ((temp-dir (make-temp-file "doom-test-" t)))
    (unwind-protect
        (let* ((el-file (expand-file-name "test.el" temp-dir))
               (elc-file (expand-file-name "test.elc" temp-dir)))
          ;; Create .el first (older)
          (with-temp-file el-file (insert "(message \"test\")"))
          (sleep-for 0.1)
          ;; Create .elc second (newer)
          (with-temp-file elc-file (insert ";; compiled"))
          ;; .elc should not be outdated
          (should (null (doom-packages--elc-file-outdated-p el-file))))
      (delete-directory temp-dir t))))


;;; ============================================================
;;; Tests for Package List Functions
;;; ============================================================

(ert-deftest doom-package-pinned-alist/returns-alist ()
  "doom-package-pinned-alist returns an alist of pinned packages."
  (cl-letf (((symbol-function 'doom-package-recipe-repo)
             (lambda (pkg) (symbol-name pkg))))
    (with-mock-packages
     (let ((result (doom-package-pinned-alist)))
       (should (listp result))
       ;; pkg-b has a pin "abc1234"
       (let ((entry (assoc "pkg-b" result)))
         (should entry)
         (should (equal (cdr entry) "abc1234")))))))

(ert-deftest doom-package-recipe-alist/returns-alist ()
  "doom-package-recipe-alist returns an alist of package recipes."
  (defvar straight--recipe-cache)
  (let ((straight--recipe-cache (make-hash-table :test 'eq)))
    ;; Add mock entries
    (puthash 'pkg-a '(:package pkg-a :local-repo "pkg-a" :type git) straight--recipe-cache)
    (puthash 'pkg-b '(:package pkg-b :local-repo "pkg-b" :type git) straight--recipe-cache)
    (puthash 'builtin-pkg '(:package builtin-pkg :type built-in) straight--recipe-cache)
    (with-mock-packages
     (let ((result (doom-package-recipe-alist)))
       (should (listp result))
       (should (> (length result) 1))))))


;;; ============================================================
;;; Tests for doom-packages--read
;;; ============================================================

(ert-deftest doom-packages--read/parses-declarations ()
  "doom-packages--read parses package! declarations from file."
  (let ((temp-file (make-temp-file "doom-test-packages-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "(package! test-pkg :pin \"abc123\")\n(package! another-pkg)"))
          (let ((doom-packages nil))
            (doom-packages--read temp-file t)
            (should (assq 'test-pkg doom-packages))
            (should (assq 'another-pkg doom-packages))))
      (delete-file temp-file))))

(ert-deftest doom-packages--read/handles-missing-files ()
  "doom-packages--read handles missing files gracefully with noerror."
  (should (null (doom-packages--read "/nonexistent/file.el" nil t))))

(ert-deftest doom-packages--read/ignores-comments ()
  "doom-packages--read ignores package declarations in comments."
  (let ((temp-file (make-temp-file "doom-test-packages-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert ";; (package! commented-pkg)\n(package! real-pkg)"))
          (let ((doom-packages nil))
            (doom-packages--read temp-file t)
            (should (assq 'real-pkg doom-packages))
            (should (null (assq 'commented-pkg doom-packages)))))
      (delete-file temp-file))))


;;; ============================================================
;;; Tests for Build/Purge Functions
;;; ============================================================

(ert-deftest doom-packages--purge-build/removes-directory ()
  "doom-packages--purge-build removes a build directory."
  (let ((temp-dir (make-temp-file "doom-test-build-" t)))
    (unwind-protect
        (progn
          ;; Create a fake build directory
          (with-temp-file (expand-file-name "test.el" temp-dir)
            (insert "(provide 'test)"))
          (should (file-directory-p temp-dir))
          ;; Mock straight--build-dir
          (cl-letf (((symbol-function 'straight--build-dir)
                     (lambda (_) temp-dir)))
            (doom-packages--purge-build "test-pkg")
            (should (null (file-directory-p temp-dir)))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest doom-packages--purge-repo/removes-directory ()
  "doom-packages--purge-repo removes a repository directory."
  (let ((temp-dir (make-temp-file "doom-test-repo-" t))
        (temp-modified-file (make-temp-file "doom-test-modified-")))
    (unwind-protect
        (progn
          ;; Create a fake .git directory
          (make-directory (expand-file-name ".git" temp-dir))
          (should (file-directory-p temp-dir))
          ;; Mock straight functions
          (cl-letf (((symbol-function 'straight--repos-dir)
                     (lambda (_) temp-dir))
                    ((symbol-function 'straight--modified-file)
                     (lambda (_) temp-modified-file))
                    ((symbol-function 'print!)
                     (lambda (&rest _) nil)))
            (doom-packages--purge-repo "test-repo")
            (should (null (file-directory-p temp-dir)))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t))
      (when (file-exists-p temp-modified-file)
        (delete-file temp-modified-file)))))


;;; ============================================================
;;; Tests for doom-packages--barf-if-incomplete
;;; ============================================================

(ert-deftest doom-packages--barf-if-incomplete/signals-error ()
  "doom-packages--barf-if-incomplete signals error when packages are incomplete."
  (cl-letf (((symbol-function 'straight-check-all)
             (lambda () (error "Incomplete"))))
    (should-error (doom-packages--barf-if-incomplete) :type 'user-error)))

(ert-deftest doom-packages--barf-if-incomplete/succeeds-when-complete ()
  "doom-packages--barf-if-incomplete succeeds when packages are complete."
  (cl-letf (((symbol-function 'straight-check-all)
             (lambda () nil)))
    (should (null (doom-packages--barf-if-incomplete)))))


;;; ============================================================
;;; Tests for Package Dependencies
;;; ============================================================

(ert-deftest doom-package-dependencies/returns-list ()
  "doom-package-dependencies returns a list of dependencies."
  (cl-letf (((symbol-function 'straight-dependencies)
             (lambda (_) '("dep1" "dep2" ("nested" "dep")))))
    (should (equal (doom-package-dependencies 'test-pkg nil) '("dep1" "dep2")))
    (should (equal (doom-package-dependencies 'test-pkg t) '("dep1" "dep2" "nested" "dep")))
    (should (equal (doom-package-dependencies 'test-pkg 'tree) '("dep1" "dep2" ("nested" "dep"))))))

(ert-deftest doom-package-dependencies/validates-symbol ()
  "doom-package-dependencies validates that PACKAGE is a symbol."
  (should-error (doom-package-dependencies "not-a-symbol")))

(ert-deftest doom-package-depending-on/returns-dependents ()
  "doom-package-depending-on returns packages that depend on PACKAGE."
  (cl-letf (((symbol-function 'straight-dependents)
             (lambda (_) '("dependent1" "dependent2")))
            ((symbol-function 'doom-package-build-recipe)
             (lambda (&rest _) t)))
    (should (equal (doom-package-depending-on 'test-pkg) '("dependent1" "dependent2")))))


;;; ============================================================
;;; Tests for doom-package-backend
;;; ============================================================

(ert-deftest doom-package-backend/detects-straight ()
  "doom-package-backend detects straight-managed packages."
  (defvar straight--build-cache)
  (let ((straight--build-cache (make-hash-table :test 'equal)))
    (puthash "test-pkg" '(t nil nil) straight--build-cache)
    (should (equal (doom-package-backend 'test-pkg) 'straight))))

(ert-deftest doom-package-backend/detects-builtin ()
  "doom-package-backend detects built-in packages."
  (defvar straight--build-cache)
  (let ((straight--build-cache (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'doom-package-built-in-p)
               (lambda (pkg) (eq pkg 'emacs))))
      (should (equal (doom-package-backend 'emacs) 'builtin)))))


;;; ============================================================
;;; Tests for Package Homepage
;;; ============================================================

(ert-deftest doom-package-homepage/returns-cached-value ()
  "doom-package-homepage returns cached value when available."
  (put 'cached-pkg 'homepage "https://cached.example.com")
  (cl-letf (((symbol-function 'doom-initialize-packages)
             (lambda (&rest _) nil)))
    (should (equal (doom-package-homepage 'cached-pkg) "https://cached.example.com")))
  ;; Clean up
  (put 'cached-pkg 'homepage nil))


(provide 'test-packages)
;;; test-packages.el ends here
