;; -*- no-byte-compile: t; -*-
;;; core/test/test-core-lib.el

(describe "core-lib"
  (before-all
    (require 'core-lib))

  ;; --- Helpers ----------------------------
  (describe "doom-unquote"
    (it "unquotes a quoted form"
      (expect (doom-unquote '(quote hello)) :to-be 'hello))
    (it "unquotes nested quoted forms"
      (expect (doom-unquote '(quote (quote (a b c)))) :to-equal '(a b c)))
    (it "unquotes function-quoted forms"
      (expect (doom-unquote '(function a)) :to-be 'a))
    (it "does nothing to unquoted forms"
      (expect (doom-unquote 'hello) :to-be 'hello)
      (expect (doom-unquote 5) :to-be 5)
      (expect (doom-unquote t) :to-be t)))

  (describe "doom-enlist"
    (it "returns nil if given nil"
      (expect (doom-enlist nil) :to-be nil))
    (it "creates a list out of non-lists"
      (expect (doom-enlist 'a) :to-equal '(a)))
    (it "returns lists as-is"
      (expect (doom-enlist '(a)) :to-equal '(a))))

  (describe "doom-keyword-intern"
    (it "returns a keyword"
      (expect (doom-keyword-intern "test") :to-equal :test))
    (it "errors if given anything but a string"
      (expect (doom-keyword-intern t) :to-throw 'wrong-type-argument)))

  (describe "doom-keyword-name"
    (it "returns the string name of a keyword"
      (expect (doom-keyword-name :test) :to-equal "test"))
    (it "errors if given anything but a keyword"
      (expect (doom-keyword-name "test") :to-throw 'wrong-type-argument)))

  (describe "doom-partial"
    (it "returns a closure"
      (expect (functionp (doom-partial #'+ 1))))
    (it "returns a partial closure"
      (expect (funcall (doom-partial #'+ 1) 2) :to-be 3)))

  (describe "doom-rpartial"
    (it "returns a closure"
      (expect (functionp (doom-rpartial #'+ 1))))
    (it "returns a partial closure with right-aligned arguments"
      (expect (funcall (doom-rpartial #'/ 2) 10) :to-be 5)))


  ;; --- Sugars -----------------------------
  (describe "lambda!"
    (it "returns an interactive function"
      (expect (commandp (lambda!)))
      (expect (funcall (lambda! 5)) :to-equal 5)))

  (describe "lambda!!"
    (it "returns an interactive function with a prefix argument"
      (expect (commandp (lambda! #'ignore t)))
      (expect (funcall (lambda!! (lambda (arg)
                                   (interactive "P")
                                   arg)
                                 5))
              :to-equal 5)))

  (describe "file!"
    (it "returns the executing file"
      (expect (eval-and-compile (file!))
              :to-equal
              (eval-and-compile load-file-name))))

  (describe "dir!"
    (it "returns the executing directory"
      (expect (eval-and-compile (dir!))
              :to-equal
              (eval-and-compile
                (directory-file-name (file-name-directory load-file-name))))))

  (describe "pushnew!"
    (it "pushes values onto a list symbol, in order"
      (let ((a '(1 2 3)))
        (expect (pushnew! a 9 8 7)
                :to-equal '(7 8 9 1 2 3))))
    (it "only adds values that aren't already in the list"
      (let ((a '(1 symbol 3.14 "test")))
        (expect (pushnew! a "test" 'symbol 3.14 1)
                :to-equal '(1 symbol 3.14 "test")))))

  (describe "prependq!"
    (it "prepends a list to a list symbol"
      (let ((list '(a b c)))
        (expect (prependq! list '(d e f))
                :to-equal '(d e f a b c)))))

  (describe "append!"
    (it "appends a list to a list symbol"
      (let ((list '(a b c)))
        (expect (appendq! list '(d e f))
                :to-equal '(a b c d e f)))))

  (describe "nconcq!"
    (it "nconc's a list to a list symbol"
      (let ((list '(a b c)))
        (expect (nconcq! list '(d e f))
                :to-equal '(a b c d e f)))))

  (describe "delq!"
    (it "delete's a symbol from a list"
      (let ((list '(a b c)))
        (delq! 'b list)
        (expect list :to-equal '(a c))))
    (it "delete's an element from an alist by key"
      (let ((alist '((a 1) (b 2) (c 3))))
        (delq! 'b alist 'assq)
        (expect alist :to-equal '((a 1) (c 3))))))

  (describe "hooks"
    (describe "add-hook!"
      :var (fake-mode-hook other-mode-hook some-mode-hook)
      (before-each
        (setq fake-mode-hook '(first-hook)
              other-mode-hook nil
              some-mode-hook '(first-hook second-hook)))

      (it "resolves quoted hooks literally"
        (expect '(add-hook! 'fake-mode-hook #'ignore) :to-expand-into
                `(add-hook 'fake-mode-hook #'ignore nil nil)))
      (it "resolves unquoted modes to their hook variables"
        (expect '(add-hook! fake-mode #'ignore) :to-expand-into
                `(add-hook 'fake-mode-hook #'ignore nil nil)))

      (it "adds one-to-one hook"
        (add-hook! fake-mode #'hook-2)
        (add-hook! 'fake-mode-hook #'hook-1)
        (expect fake-mode-hook :to-equal '(hook-1 hook-2 first-hook)))

      (it "adds one-to-many hook"
        (add-hook! (fake-mode other-mode some-mode) #'hook-2)
        (add-hook! '(fake-mode-hook other-mode-hook some-mode-hook) #'hook-1)
        (add-hook! (fake-mode other-mode some-mode) :append #'last-hook)
        (expect fake-mode-hook  :to-equal '(hook-1 hook-2 first-hook last-hook))
        (expect other-mode-hook :to-equal '(hook-1 hook-2 last-hook))
        (expect some-mode-hook  :to-equal '(hook-1 hook-2 first-hook second-hook last-hook)))

      (it "adds many-to-many hooks and preserve provided order"
        (add-hook! (fake-mode other-mode some-mode) #'(hook-3 hook-4))
        (add-hook! '(fake-mode-hook other-mode-hook some-mode-hook) #'(hook-1 hook-2))
        (add-hook! '(fake-mode-hook other-mode-hook some-mode-hook) :append #'(last-hook-1 last-hook-2))
        (expect fake-mode-hook  :to-equal '(hook-1 hook-2 hook-3 hook-4 first-hook last-hook-1 last-hook-2))
        (expect other-mode-hook :to-equal '(hook-1 hook-2 hook-3 hook-4 last-hook-1 last-hook-2))
        (expect some-mode-hook  :to-equal '(hook-1 hook-2 hook-3 hook-4 first-hook second-hook last-hook-1 last-hook-2)))

      (it "adds implicit lambda to one hook"
        (add-hook! fake-mode (progn))
        (add-hook! 'other-mode-hook (ignore))
        (add-hook! 'some-mode-hook :append (ignore))
        (expect (caar fake-mode-hook) :to-be 'lambda)
        (expect (caar other-mode-hook) :to-be 'lambda)
        (expect (caar (last other-mode-hook)) :to-be 'lambda))

      (it "handles inline defuns as hook symbols"
        (add-hook! fake-mode (defun hook-a ()))
        (add-hook! 'other-mode-hook
          (defun hook-b ())
          (defun hook-c ()))
        (expect (car fake-mode-hook) :to-be 'hook-a)
        (expect other-mode-hook :to-equal '(hook-b hook-c))))

    (describe "remove-hook!"
      :var (fake-mode-hook)
      (before-each
        (setq fake-mode-hook '(first-hook second-hook third-hook fourth-hook)))
      (it "removes one hook"
        (remove-hook! fake-mode #'third-hook)
        (remove-hook! 'fake-mode-hook #'second-hook)
        (expect fake-mode-hook :to-equal '(first-hook fourth-hook)))
      (it "removes multiple hooks"
        (remove-hook! fake-mode #'(first-hook third-hook))
        (remove-hook! 'fake-mode-hook #'(second-hook fourth-hook))
        (expect fake-mode-hook :to-be nil)))

    (describe "add-transient-hook!"
      (it "adds a transient function to hooks"
        (let (hooks value)
          (add-transient-hook! 'hooks (setq value t))
          (run-hooks 'hooks)
          (expect value)
          (expect hooks :to-be nil)))
      (it "advises a function with a transient advisor"
        (let (value)
          (add-transient-hook! #'ignore (setq value (not value)))
          (ignore t)
          (expect value)
          ;; repeat to ensure it was only run once
          (ignore t)
          (expect value))))

    (describe "(un)setq-hook!"
      :var (fake-hook x y z)
      (before-each
        (setq x 10 y 20 z 30))

      (it "sets variables buffer-locally"
        (setq-hook! 'fake-hook x 1)
        (with-temp-buffer
          (run-hooks 'fake-hook)
          (expect (local-variable-p 'x))
          (expect (= x 1)))
        (expect (= x 10)))

      (it "overwrites earlier hooks"
        (setq-hook! 'fake-hook x 1 y 0)
        (setq-hook! 'fake-hook x 5 y -1)
        (with-temp-buffer
          (run-hooks 'fake-hook)
          (expect (= x 5))
          (expect (= y -1))))

      (it "unset setq hooks"
        (setq-hook! 'fake-hook x 1 y 0)
        (unsetq-hook! 'fake-hook y)
        (with-temp-buffer
          (run-hooks 'fake-hook)
          (expect (local-variable-p 'x))
          (expect (= x 1))
          (expect (not (local-variable-p 'y)))
          (expect (= y 20))))))

  (describe "load!"
    (before-each
      (spy-on 'load :and-return-value t))

    (it "loads a file relative to the current directory"
      (load! "path")
      (expect 'load :to-have-been-called)
      (expect 'load :to-have-been-called-with
              (expand-file-name "path" (eval-when-compile (dir!))) nil 'nomessage))

    (it "loads a file relative to a specified directory"
      (load! "path" doom-etc-dir)
      (expect 'load :to-have-been-called-with
              (expand-file-name "path" doom-etc-dir) nil 'nomessage)))

  (describe "quiet!"
    :var (doom-debug-mode)
    (before-each
      (setq doom-debug-mode nil))

    (it "suppresses output from message"
      (expect (message "hello world") :to-output "hello world\n")
      (expect (message "hello world") :to-output)
      (let (doom-interactive-mode)
        (expect (quiet! (message "hello world")) :not :to-output))
      (let ((doom-interactive-mode t))
        (expect (quiet! inhibit-message))
        (expect (quiet! save-silently))))

    (it "suppresses load messages from `load' & `load-file'"
      (let ((tmpfile (make-temp-file "test" nil ".el")))
        (with-temp-file tmpfile)
        (let (doom-interactive-mode)
          (expect (load-file tmpfile) :to-output (format "Loading %s (source)...\n" tmpfile))
          (expect (quiet! (load-file tmpfile)) :not :to-output))
        (delete-file tmpfile)))

    (it "won't suppress output in debug mode"
      (let ((doom-debug-mode t)
            (tmpfile (make-temp-file "test" nil ".el")))
        (dolist (doom-interactive-mode (list t nil))
          (expect (quiet! (message "hello world"))
                  :to-output "hello world\n")
          (with-temp-file tmpfile)
          (expect (quiet! (load-file tmpfile))
                  :to-output (format "Loading %s (source)...\n" tmpfile)))))))
