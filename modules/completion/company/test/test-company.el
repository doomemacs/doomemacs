;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; completion/company/test/test-company.el

(describe "completion/company"
  (before-all
    (load! "../autoload"))

  (describe ":company-backend"
    :var (a +company-backend-alist backends)
    (before-each
      (setq-default company-backends '(t))
      (setq +company-backend-alist nil
            a (get-buffer-create "x"))
      (fset 'backends
            (lambda (mode)
              (let ((major-mode mode))
                (+company--backends))))
      (set-buffer a)
      (spy-on 'require))
    (after-each
      (kill-buffer a))

    ;;
    (it "sets backends for a major mode"
      (set-company-backend! 'text-mode 'a)
      (expect (backends 'text-mode) :to-equal '(a t)))

    (it "sets backends for a derived-mode"
      (set-company-backend! 'prog-mode 'a)
      (expect (backends 'prog-mode) :to-equal '(a t))
      (expect (backends 'emacs-lisp-mode) :to-equal '(a t)))

    (it "sets multiple backends for exact major modes"
      (set-company-backend! '(text-mode emacs-lisp-mode) 'a 'b)
      (expect (backends 'text-mode) :to-equal (backends 'emacs-lisp-mode)))

    (it "sets cumulative backends"
      (set-company-backend! 'prog-mode '(a b c))
      (set-company-backend! 'emacs-lisp-mode 'd 'e)
      (expect (backends 'emacs-lisp-mode) :to-equal '(d e (a b c) t)))

    (it "sets cumulative backends with a minor mode"
      (set-company-backend! 'prog-mode '(a b c))
      (set-company-backend! 'emacs-lisp-mode 'd 'e)
      (set-company-backend! 'some-minor-mode 'x 'y)
      (setq-local some-minor-mode t)
      (expect (backends 'emacs-lisp-mode) :to-equal '(x y d e (a b c) t)))

    (it "overwrites past backends"
      (set-company-backend! 'text-mode 'old 'backends)
      (set-company-backend! 'text-mode 'new 'backends)
      (expect (backends 'text-mode) :to-equal '(new backends t)))

    (it "unsets past backends"
      (set-company-backend! 'text-mode 'old)
      (set-company-backend! 'text-mode nil)
      (expect (backends 'text-mode) :to-equal (default-value 'company-backends)))

    (it "unsets past parent backends"
      (set-company-backend! 'prog-mode 'old)
      (set-company-backend! 'emacs-lisp-mode 'child)
      (set-company-backend! 'prog-mode nil)
      (expect (backends 'emacs-lisp-mode) :to-equal '(child t)))

    (it "overwrites past cumulative backends"
      (set-company-backend! 'prog-mode 'base)
      (set-company-backend! 'emacs-lisp-mode 'old)
      (set-company-backend! 'emacs-lisp-mode 'new)
      (expect (backends 'emacs-lisp-mode) :to-equal '(new base t)))

    (it "overwrites past parent backends"
      (set-company-backend! 'prog-mode 'base)
      (set-company-backend! 'emacs-lisp-mode 'child)
      (set-company-backend! 'prog-mode 'new)
      (expect (backends 'emacs-lisp-mode) :to-equal '(child new t)))))
