;; -*- no-byte-compile: t; -*-
;;; editor/format/test/test-format.el

(load! "../autoload/settings")
(load! "../autoload/format")
(require! :editor format)
(require 'format-all)

;;
(describe "editor/format"
  :var (format-all-format-table
        format-all-mode-table)

  (before-each
    (setq format-all-format-table (make-hash-table)
          format-all-mode-table (make-hash-table)))

  (describe "set-formatter!"
    (before-each
      (set-formatter! 'test (lambda () (interactive))))

    (it "defines a formatter"
      (set-formatter! 'new (lambda () (interactive)))
      (expect (gethash 'new format-all-mode-table) :to-equal nil)
      (expect (functionp (gethash 'new format-all-format-table))))

    (it "defines a formatter with modes"
      (set-formatter! 'new (lambda () (interactive))
        :modes '(a-mode (b-mode "x")))
      (expect (gethash 'a-mode format-all-mode-table)
              :to-equal '((new)))
      (expect (gethash 'b-mode format-all-mode-table)
              :to-equal '((new . (lambda () "x")))))

    (it "replaces a pre-existing formatter"
      (let ((old-fn (gethash 'test format-all-format-table)))
        (set-formatter! 'test "echo")
        (expect (gethash 'test format-all-format-table) :not :to-equal old-fn)))

    (it "unsets a pre-existing formatter"
      (set-formatter! 'test nil)
      (expect (gethash 'test format-all-format-table) :to-be nil))

    (it "errors when unsetting non-existent formatter"
      (expect (set-formatter! 'doesnt-exist nil) :to-throw)))


  ;; TODO
  (xdescribe "hooks"
    (describe "format|enable-on-save-maybe")
    (describe "format|enable-on-save"))


  ;; TODO
  (xdescribe "formatting"
    (before-each
      (set-formatter! 'command
        (lambda ()
          (interactive)
          (let ((first-line (car (split-string (buffer-string) "\n"))))
            (erase-buffer)
            (insert first-line)))
        :modes '(text-mode))
      (set-formatter! 'faulty-command
        (lambda ()
          (interactive)
          (error "This is a test"))
        :modes '(text-mode))
      (set-formatter! 'function
        (lambda (input)
          (insert (car (split-string input "\n")))
          (list nil nil))
        :modes '(text-mode))
      (set-formatter! 'shellcmd "head -n 1"
        :modes '(text-mode))
      (set-formatter! 'cmdlist '("head" "-n" "1")
        :modes '(text-mode)))

    (describe "with an interactive command"
      (it "formats a buffer" )
      (it "formats a region" )
      (it "no-ops if no change" )
      (it "doesn't modify the buffer in case of errors" )
      (it "preserves indentation" ))

    (describe "with a function"
      (it "formats a buffer" )
      (it "formats a region" )
      (it "no-ops if no change" )
      (it "doesn't modify the buffer in case of errors" )
      (it "preserves indentation" ))

    (describe "with a shell command")

    (describe "with a shell command list"
      (it "formats a buffer" )
      (it "formats a region" )
      (it "no-ops if no change" )
      (it "doesn't modify the buffer in case of errors" )
      (it "preserves indentation" )

      (it "interpolates non-strings into format strings" )
      (it "conditionally appends sublisted options" ))))
