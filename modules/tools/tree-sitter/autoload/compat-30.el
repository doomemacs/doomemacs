;;; tools/tree-sitter/autoload/compat-30.el -*- lexical-binding: t; -*-
;;;###if (and (fboundp 'treesit-available-p) (version< emacs-version "31.1"))
;;
;; Backported from 31.1
;;
;;; Code:

(autoload 'treesit-ready-p "treesit")

;;;###autoload
(unless (fboundp 'treesit-available-p)
  (defun treesit-available-p ()
    "Return non-nil if tree-sitter support is built-in and available."
    nil))

;;;###autoload
(defcustom treesit-auto-install-grammar 'ask
  "Whether to install tree-sitter language grammar libraries when needed.
This controls whether Emacs will install missing grammar libraries
when they are needed by some tree-sitter based mode.
If `ask', ask for confirmation before installing the required grammar library.
If `always', install the grammar library without asking.
If nil or `never' or anything else, don't install the grammar library
even while visiting a file in the mode that requires such grammar; this
might display a warning and/or fail to turn on the mode."
  :type '(choice (const :tag "Never install grammar libraries" never)
          (const :tag "Always automatically install grammar libraries"
                 always)
          (const :tag "Ask whether to install missing grammar libraries"
                 ask))
  :version "31.1"
  :group 'treesit)

;;;###autoload
(defun treesit-ensure-installed (lang)
  "Ensure that the grammar library for the language LANG is installed.
The option `treesit-auto-install-grammar' defines whether to install
the grammar library if it's unavailable."
  (or (treesit-ready-p lang t)
      (when (or (eq treesit-auto-install-grammar 'always)
                (and (eq treesit-auto-install-grammar 'ask)
                     (y-or-n-p (format "\
Tree-sitter grammar for `%s' is missing; install it?"
                                       lang))))
        (treesit-install-language-grammar lang)
        ;; Check that the grammar was installed successfully
        (treesit-ready-p lang))))

;;; Introduced in later commits of 31.X
;;;###autoload
(unless (boundp 'treesit-major-mode-remap-alist)
  (defvar treesit-major-mode-remap-alist nil))

;;;###autoload
(defcustom treesit-enabled-modes nil
  "Specify what treesit modes to enable by default.
The value can be either a list of ts-modes to enable,
or t to enable all ts-modes."
  :type `(choice
          (const :tag "Disable all automatic associations" nil)
          (const :tag "Enable all available ts-modes" t)
          (set :tag "List of enabled ts-modes"
               ,@(when (treesit-available-p)
                   (let ((items (mapcar (lambda (m) `(function-item ,m))
                                        (seq-uniq (mapcar #'cdr treesit-major-mode-remap-alist)))))
                     (if (= (car (func-arity 'sort)) 1)
                         (sort items)
                       (sort items #'value<))))))
  :initialize #'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (when (treesit-available-p)
           (dolist (m treesit-major-mode-remap-alist)
             (setq major-mode-remap-alist
                   (if (or (eq val t) (memq (cdr m) val))
                       (cons m major-mode-remap-alist)
                     (delete m major-mode-remap-alist))))))
  :version "31.1")

;;; compat-30.el ends here
