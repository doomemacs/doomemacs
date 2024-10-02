;;; lisp/lib/sandbox.el -*- lexical-binding: t; -*-

(defvar doom-sandbox-buffer-name "*doom:sandbox*"
  "Name of the Doom sandbox buffer.")

(defvar doom-sandbox-dir
  (expand-file-name "doom-sandbox" (temporary-file-directory))
  "TODO")

(defvar doom-sandbox-preamble
  ";; Welcome to the sandbox!
;;
;; This is a test bed for running Emacs Lisp in another instance of Emacs that
;; has varying amounts of Doom loaded:
;;
;; - vanilla Emacs (nothing loaded)         \\[doom--run-vanilla-emacs]
;; - vanilla Doom (only Doom core)          \\[doom--run-vanilla-doom]
;; - Doom + modules - your private config   \\[doom--run-vanilla-doom+]
;; - Doom + modules + your private config   \\[doom--run-full-doom]
;;
;; This is done without sacrificing access to installed packages. Use the sandbox
;; to reproduce bugs and determine if Doom is to blame.\n\n"
  "TODO")

(defun doom--sandbox-launch (args forms)
  (require 'package)
  (require 'restart-emacs)
  (let* ((sandbox-file (expand-file-name "init.el" doom-sandbox-dir))
         (args (append args (list "-l" sandbox-file))))
    (delete-directory doom-sandbox-dir 'recursive)
    (make-directory doom-sandbox-dir 'parents)
    (with-temp-file sandbox-file
      (prin1 forms (current-buffer)))
    (condition-case-unless-debug e
        (cond ((display-graphic-p)
               (if (memq system-type '(windows-nt ms-dos))
                   (restart-emacs--start-gui-on-windows args)
                 (restart-emacs--start-gui-using-sh args)))
              ((memq system-type '(windows-nt ms-dos))
               (user-error "Cannot start another Emacs from Windows shell."))
              ((suspend-emacs
                (format "%s %s -nw; fg"
                        (shell-quote-argument (restart-emacs--get-emacs-binary))
                        (mapconcat #'shell-quote-argument args " ")))))
      (error
       (delete-directory doom-sandbox-dir 'recursive)
       (signal (car e) (cdr e))))))


(defun doom--sandbox-run (&optional mode)
  "TODO"
  (letenv! (("DOOMDIR" (if (eq mode 'vanilla-doom+)
                           (expand-file-name "___does_not_exist___" temporary-file-directory)
                         doom-user-dir)))
    (doom--sandbox-launch
     (unless (memq mode '(doom vanilla-doom+)) '("-Q"))
     (let ((forms
            (read (format "(progn\n%s\n)"
                          (buffer-substring-no-properties
                           (point-min)
                           (point-max))))))
       (if (memq mode '(doom vanilla-doom+))
           forms
         `(progn
            ;; doom variables
            (setq init-file-debug t
                  doom-log-level 2
                  doom-emacs-dir ,doom-emacs-dir
                  doom-cache-dir ,(expand-file-name "cache/" doom-sandbox-dir)
                  doom-data-dir  ,(expand-file-name "data/" doom-sandbox-dir))
            (define-advice locate-user-emacs-file (:around (fn &rest args) restrict-to-data-dir)
              (let ((user-emacs-directory doom-data-dir))
                (apply fn args)))
            ;; emacs essential variables
            (setq before-init-time (current-time)
                  after-init-time nil
                  init-file-debug init-file-debug
                  noninteractive nil
                  process-environment (get 'process-environment 'initial-value)
                  exec-path (get 'exec-path 'initial-value)
                  load-path ',load-path
                  user-init-file load-file-name)
            ;; package.el
            (setq package--init-file-ensured t
                  package-user-dir ,package-user-dir
                  package-archives ',package-archives)
            (with-eval-after-load 'doom
              (run-hooks 'doom-before-init-hook))
            (with-eval-after-load 'undo-tree
              ;; HACK `undo-tree' sometimes throws errors because
              ;;      `buffer-undo-tree' isn't correctly initialized.
              (setq-default buffer-undo-tree (make-undo-tree)))
            ;; Then launch as much about Emacs as we can
            (defun --run-- () ,forms)
            ,(pcase mode
               (`vanilla-doom  ; only Doom core
                `(let ((doom-user-dir "/tmp/does/not/exist"))
                   (require 'doom ,(expand-file-name "doom.el" doom-core-dir))
                   (let ((doom-module-init-file "__does-not-exist__"))
                     (require 'doom-start))
                   (setq doom-modules (make-hash-table :test 'equal))
                   (--run--)))
               (`vanilla       ; nothing loaded
                `(progn
                   (setq native-comp-deferred-compilation nil
                         native-comp-deferred-compilation-deny-list ',(bound-and-true-p native-comp-async-env-modifier-form)
                         native-comp-async-env-modifier-form ',(bound-and-true-p native-comp-async-env-modifier-form)
                         native-comp-eln-load-path ',(bound-and-true-p native-comp-eln-load-path))
                   (package-initialize t)
                   (--run--))))
            (with-eval-after-load 'doom
              ;; Then rerun Emacs' startup hooks to simulate a fresh Emacs session,
              ;; because they've already fired.
              (fset 'doom-run-hook  #',(symbol-function #'doom-run-hook))
              (fset 'doom-run-hooks #',(symbol-function #'doom-run-hooks))
              (fset 'doom-run-all-startup-hooks-h #',(symbol-function #'doom-run-all-startup-hooks-h))
              ;; (doom-run-all-startup-hooks-h)
              (unless (default-toplevel-value 'mode-line-format)
                (setq-default mode-line-format (get 'mode-line-format 'initial-value))))))))))

(fset 'doom--run-vanilla-emacs (cmd! (doom--sandbox-run 'vanilla)))
(fset 'doom--run-vanilla-doom  (cmd! (doom--sandbox-run 'vanilla-doom)))
(fset 'doom--run-vanilla-doom+ (cmd! (doom--sandbox-run 'vanilla-doom+)))
(fset 'doom--run-full-doom     (cmd! (doom--sandbox-run 'doom)))

(defvar doom-sandbox-emacs-lisp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'doom--run-vanilla-emacs)
    (define-key map (kbd "C-c C-d") #'doom--run-vanilla-doom)
    (define-key map (kbd "C-c C-p") #'doom--run-vanilla-doom+)
    (define-key map (kbd "C-c C-f") #'doom--run-full-doom)
    (define-key map (kbd "C-c C-k") #'kill-current-buffer)
    map))

(define-derived-mode doom-sandbox-emacs-lisp-mode emacs-lisp-mode "Sandbox Elisp"
  "TODO")

;;;###autoload
(defun doom/sandbox ()
  "Open the Emacs Lisp sandbox.

This is a test bed for running Emacs Lisp in another instance of Emacs with
varying amounts of Doom loaded, including:

  a) vanilla Emacs (nothing loaded),
  b) vanilla Doom (only Doom core),
  c) Doom + modules - your private config or
  c) Doom + modules + your private config (a complete Doom session)

This is done without sacrificing access to installed packages. Use the sandbox
to reproduce bugs and determine if Doom is to blame."
  (interactive)
  (pop-to-buffer
   (with-current-buffer (get-buffer-create doom-sandbox-buffer-name)
     (doom-sandbox-emacs-lisp-mode)
     (setq-local default-directory doom-emacs-dir)
     (and (buffer-live-p (get-buffer doom-sandbox-buffer-name))
          (= (buffer-size) 0)
          (insert (substitute-command-keys doom-sandbox-preamble)))
     (goto-char (point-max))
     (current-buffer))))

(provide 'doom-lib '(sandbox))
;;; sandbox.el ends here
