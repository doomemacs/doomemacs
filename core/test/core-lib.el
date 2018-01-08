;; -*- no-byte-compile: t; -*-
;;; core/test/core-lib.el

;; --- Helpers ----------------------------

;; `doom--resolve-path-forms'
(def-test! resolve-path-forms
  (should
   (equal (doom--resolve-path-forms '(and "fileA" "fileB"))
          '(and (file-exists-p (expand-file-name "fileA" (doom-project-root)))
                (file-exists-p (expand-file-name "fileB" (doom-project-root)))))))

;; `doom--resolve-hook-forms'
(def-test! resolve-hook-forms
  (should (equal (doom--resolve-hook-forms '(js2-mode haskell-mode))
                 '(js2-mode-hook haskell-mode-hook)))
  (should (equal (doom--resolve-hook-forms '(quote (js2-mode-hook haskell-mode-hook)))
                 '(js2-mode-hook haskell-mode-hook))))

;; `doom-unquote'
(def-test! unquote
  (should (equal (doom-unquote '(quote (a b c))) '(a b c)))
  ;; nested
  (should (equal (doom-unquote '(quote (quote (a b c)))) '(a b c)))
  ;; sub-quote
  (should (equal (doom-unquote '(quote (a (quote b) c))) '(a (quote b) c)))
  ;; function
  (should (equal (doom-unquote '(function a)) 'a)))

;; `doom-enlist'
(def-test! enlist
  (should (equal (doom-enlist 'a) '(a)))
  (should (equal (doom-enlist '(a)) '(a))))


;; --- Macros -----------------------------

;; `add-hook!'
(def-test! add-one-to-one-hook
  (let ((hooks '(old-hook)))
    (add-hook! 'hooks 'a-hook)
    (should (equal hooks '(a-hook old-hook)))))

(def-test! add-many-to-one-hook
  (let ((hooks '(hook-x)))
    (add-hook! 'hooks '(hook-a hook-b hook-c))
    (should (equal hooks '(hook-a hook-b hook-c hook-x)))))

(def-test! add-one-to-many-hooks
  (let (hooks-a hooks-b hooks-c)
    (add-hook! '(hooks-a hooks-b hooks-c) 'a-hook)
    (should (equal hooks-a '(a-hook)))
    (should (equal hooks-b '(a-hook)))
    (should (equal hooks-c '(a-hook)))))

(def-test! add-many-to-many-hooks
  (let (hooks-a hooks-b hooks-c)
    (add-hook! '(hooks-a hooks-b hooks-c) '(hook-a hook-b hook-c))
    (should (equal hooks-a '(hook-a hook-b hook-c)))
    (should (equal hooks-b '(hook-a hook-b hook-c)))
    (should (equal hooks-c '(hook-a hook-b hook-c)))))

(def-test! add-non-literal-hooks
  (let (some-mode-hook)
    (add-hook! some-mode 'a-hook)
    (should (equal some-mode-hook '(a-hook)))))

;; `remove-hook!'
(def-test! remove-hooks
  (let ((hooks-a '(hook-c hook-b hook-a))
        (hooks-b '(hook-c hook-b hook-a))
        (hooks-c '(hook-c hook-b hook-a)))
    (remove-hook! '(hooks-a hooks-b hooks-c) '(hook-a hook-b hook-c))
    (should (null hooks-a))
    (should (null hooks-b))
    (should (null hooks-c))))

(def-test! remove-hook-forms
  (let (hooks)
    (add-hook! 'hooks (message "Hello world"))
    (should hooks)
    (remove-hook! 'hooks (message "Hello world"))
    (should (null hooks))))

;; `add-transient-hook!'
(def-test! transient-hooks
  (let (hooks value)
    (add-transient-hook! 'hooks (setq value t))
    (run-hooks 'hooks)
    (should (eq value t))
    (should (null hooks))))

(def-test! transient-function
  (let (value)
    (add-transient-hook! #'ignore (setq value (not value)))
    (ignore t)
    (should (eq value t))
    ;; repeat to ensure it was only run once
    (ignore t)
    (should (eq value t))))


;; TODO `associate!'


;; --- Settings ---------------------------

(def-test! set
  (eval-and-compile
    (let (doom-settings)
      (def-setting! :-test-setting (x) `(setq result ,x))
      (should (assq :-test-setting doom-settings))
      (let ((inhibit-message t)
            result)
        (set! :-test-setting t)
        (should result)
        (set! :non-existant-setting (error "This shouldn't trigger"))))))
