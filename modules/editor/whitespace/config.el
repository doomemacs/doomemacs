;;; editor/whitespace/config.el -*- lexical-binding: t; -*-

(defvar +whitespace-guess-excluded-modes
  '(pascal-mode
    so-long-mode
    ;; Variable-width indentation is superior in elisp. Otherwise, `dtrt-indent'
    ;; and `editorconfig' would force fixed indentation on elisp.
    emacs-lisp-mode
    ;; See #5823: indent detection is slow and inconclusive in these major modes
    ;; so they are disabled there.
    coq-mode
    ;; Automatic indent detection in org files is meaningless. Not to mention, a
    ;; non-standard `tab-width' causes an error in org-mode.
    org-mode)
  "A list of major modes where indentation shouldn't be auto-detected.")

(defvar +whitespace-guess-in-projects nil
  "If non-nil, indentation settings will be guessed in project files.

This is off by default because, generally, indent-guessing is less useful for
projects, which have many options for configuring editors (editorconfig,
.dir-locals.el, global settings, etc). While single files have fewer options and
are more likely to use varied styles (and would be a pain to accommodate on a
per-file basis).")

(defvar-local +whitespace-guess-inhibit nil
  "A buffer-local flag that indicates whether `dtrt-indent' should try to guess
indentation settings or not. This should be set by editorconfig if it
successfully sets indent_style/indent_size.")


;;
;;; Packages

(use-package! whitespace
  :defer t
  :config
  (setq whitespace-line-column nil
        whitespace-style
        '(face indentation tabs tab-mark spaces space-mark newline newline-mark
          trailing lines-tail)
        whitespace-display-mappings
        '((tab-mark ?\t [?› ?\t])
          (newline-mark ?\n [?¬ ?\n])
          (space-mark ?\  [?·] [?.])))

  ;; HACK: `whitespace-mode' inundates child frames with whitespace markers, so
  ;;   disable it to fix all that visual noise.
  (defun +whitespace--in-parent-frame-p () (null (frame-parameter nil 'parent-frame)))
  (add-function :before-while whitespace-enable-predicate #'+whitespace--in-parent-frame-p))


(use-package! dtrt-indent
  :when (modulep! +guess)
  ;; Automatic detection of indent settings
  :unless noninteractive
  ;; I'm not using `global-dtrt-indent-mode' because it has hard-coded and rigid
  ;; major mode checks, so I implement it in `+whitespace-guess-indentation-h'.
  :hook ((change-major-mode-after-body read-only-mode) . +whitespace-guess-indentation-h)
  :config
  (defun +whitespace-guess-indentation-h ()
    (unless (or (not after-init-time)
                doom-large-file-p
                +whitespace-guess-inhibit
                (eq major-mode 'fundamental-mode)
                (member (substring (buffer-name) 0 1) '(" " "*"))
                (apply #'derived-mode-p +whitespace-guess-excluded-modes)
                buffer-read-only
                (and (not +whitespace-guess-in-projects)
                     (doom-project-root)))
      ;; Don't display messages in the echo area, but still log them
      (let ((inhibit-message (not init-file-debug)))
        (dtrt-indent-mode +1))))

  ;; Enable dtrt-indent even in smie modes so that it can update `tab-width',
  ;; `standard-indent' and `evil-shift-width' there as well.
  (setq dtrt-indent-run-after-smie t)
  ;; Reduced from the default of 5000 for slightly faster analysis
  (setq dtrt-indent-max-lines 2000)

  ;; Always keep tab-width up-to-date
  (add-to-list 'dtrt-indent-hook-generic-mapping-list '(t tab-width))

  ;; Add missing language support
  ;; REVIEW: PR these upstream.
  (add-to-list 'dtrt-indent-hook-mapping-list '(gdscript-mode default gdscript-indent-offset))
  (add-to-list 'dtrt-indent-hook-mapping-list '(graphviz-mode graphviz-dot-indent-width))
  (add-to-list 'dtrt-indent-hook-mapping-list '(janet-mode janet janet-indent))

  (defadvice! +whitespace--guess-smie-modes-a (fn &optional arg)
    "Some smie modes throw errors when trying to guess their indentation, like
`nim-mode'. This prevents them from leaving Emacs in a broken state."
    :around #'dtrt-indent-mode
    (let ((dtrt-indent-run-after-smie dtrt-indent-run-after-smie))
      (letf! ((defun symbol-config--guess (beg end)
                (funcall symbol-config--guess beg (min end 10000)))
              (defun smie-config-guess ()
                (condition-case e (funcall smie-config-guess)
                  (error (setq dtrt-indent-run-after-smie t)
                         (message "[WARNING] Indent detection: %s"
                                  (error-message-string e))
                         (message ""))))) ; warn silently
        (funcall fn arg)))))


;; a less intrusive `delete-trailing-whitespaces' on save
(use-package! ws-butler
  :when (modulep! +trim)
  :hook (doom-first-buffer . ws-butler-global-mode)
  :config
  (pushnew! ws-butler-global-exempt-modes
            'special-mode
            'comint-mode
            'term-mode
            'eshell-mode
            'diff-mode))
