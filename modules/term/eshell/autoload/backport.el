;;; term/eshell/autoload/mode.el -*- lexical-binding: t; -*-
;;;###if (< emacs-major-version 28)

;; DEPRECATED Remove this when we drop Emacs 27 support.

;; HACK Eshell resets its keymap every time `eshell-mode' is enabled. This is
;;   fixed in Emacs 28+, but this file backports that fix for 27 users. This
;;   way, keys can be safely bound to `eshell-mode-map' and `eshell-command-map'
;;   like any normal keymap, rather than a hook.
;;
;;   Fun fact: there's a "FIXME What the hell?!" above the offending line in
;;   esh-mode.el.

;;;###autoload
(defvar eshell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c)] 'eshell-command-map)
    (define-key map "\r" #'eshell-send-input)
    (define-key map "\M-\r" #'eshell-queue-input)
    (define-key map [(meta control ?l)] #'eshell-show-output)
    (define-key map [(control ?a)] #'eshell-bol)
    map))

;;;###autoload
(defvar eshell-command-map
  (let ((map (define-prefix-command 'eshell-command-map)))
    (define-key map [(meta ?o)] #'eshell-mark-output)
    (define-key map [(meta ?d)] #'eshell-toggle-direct-send)
    (define-key map [(control ?a)] #'eshell-bol)
    (define-key map [(control ?b)] #'eshell-backward-argument)
    (define-key map [(control ?e)] #'eshell-show-maximum-output)
    (define-key map [(control ?f)] #'eshell-forward-argument)
    (define-key map [(control ?m)] #'eshell-copy-old-input)
    (define-key map [(control ?o)] #'eshell-kill-output)
    (define-key map [(control ?r)] #'eshell-show-output)
    (define-key map [(control ?t)] #'eshell-truncate-buffer)
    (define-key map [(control ?u)] #'eshell-kill-input)
    (define-key map [(control ?w)] #'backward-kill-word)
    (define-key map [(control ?y)] #'eshell-repeat-argument)
    map))

;;;###autoload
(after! esh-mode
  (define-derived-mode eshell-mode fundamental-mode "Eshell"
    "Emacs shell interactive mode."
    (setq-local eshell-mode t)

    (when eshell-status-in-mode-line
      (make-local-variable 'eshell-command-running-string)
      (let ((fmt (copy-sequence mode-line-format)))
        (setq-local mode-line-format fmt))
      (let ((mode-line-elt (memq 'mode-line-modified mode-line-format)))
        (if mode-line-elt
            (setcar mode-line-elt 'eshell-command-running-string))))

    (set (make-local-variable 'bookmark-make-record-function)
         'eshell-bookmark-make-record)
    (setq local-abbrev-table eshell-mode-abbrev-table)

    (set (make-local-variable 'list-buffers-directory)
         (expand-file-name default-directory))

    ;; always set the tab width to 8 in Eshell buffers, since external
    ;; commands which do their own formatting almost always expect this
    (set (make-local-variable 'tab-width) 8)

    ;; don't ever use auto-fill in Eshell buffers
    (setq auto-fill-function nil)

    ;; always display everything from a return value
    (if (boundp 'print-length)
        (set (make-local-variable 'print-length) nil))
    (if (boundp 'print-level)
        (set (make-local-variable 'print-level) nil))

    ;; set require-final-newline to nil; otherwise, all redirected
    ;; output will end with a newline, whether or not the source
    ;; indicated it!
    (set (make-local-variable 'require-final-newline) nil)

    (set (make-local-variable 'max-lisp-eval-depth)
         (max 3000 max-lisp-eval-depth))
    (set (make-local-variable 'max-specpdl-size)
         (max 6000 max-lisp-eval-depth))

    (set (make-local-variable 'eshell-last-input-start) (point-marker))
    (set (make-local-variable 'eshell-last-input-end) (point-marker))
    (set (make-local-variable 'eshell-last-output-start) (point-marker))
    (set (make-local-variable 'eshell-last-output-end) (point-marker))
    (set (make-local-variable 'eshell-last-output-block-begin) (point))

    (let ((modules-list (copy-sequence eshell-modules-list)))
      (make-local-variable 'eshell-modules-list)
      (setq eshell-modules-list modules-list))

    ;; This is to avoid making the paragraph base direction
    ;; right-to-left if the first word just happens to start with a
    ;; strong R2L character.
    (setq bidi-paragraph-direction 'left-to-right)

    ;; load extension modules into memory.  This will cause any global
    ;; variables they define to be visible, since some of the core
    ;; modules sometimes take advantage of their functionality if used.
    (dolist (module eshell-modules-list)
      (let ((module-fullname (symbol-name module))
            module-shortname)
        (if (string-match "^eshell-\\(.*\\)" module-fullname)
            (setq module-shortname
                  (concat "em-" (match-string 1 module-fullname))))
        (unless module-shortname
          (error "Invalid Eshell module name: %s" module-fullname))
        (unless (featurep (intern module-shortname))
          (load module-shortname))))

    (unless (file-exists-p eshell-directory-name)
      (eshell-make-private-directory eshell-directory-name t))

    ;; Load core Eshell modules, then extension modules, for this session.
    (dolist (module (append (eshell-subgroups 'eshell) eshell-modules-list))
      (let ((load-hook (intern-soft (format "%s-load-hook" module)))
            (initfunc (intern-soft (format "%s-initialize" module))))
        (when (and load-hook (boundp load-hook))
          (if (memq initfunc (symbol-value load-hook)) (setq initfunc nil))
          (run-hooks load-hook))
        ;; So we don't need the -initialize functions on the hooks (bug#5375).
        (and initfunc (fboundp initfunc) (funcall initfunc))))

    (if eshell-send-direct-to-subprocesses
        (add-hook 'pre-command-hook #'eshell-intercept-commands t t))

    (if eshell-scroll-to-bottom-on-input
        (add-hook 'pre-command-hook #'eshell-preinput-scroll-to-bottom t t))

    (when eshell-scroll-show-maximum-output
      (set (make-local-variable 'scroll-conservatively) 1000))

    (when eshell-status-in-mode-line
      (add-hook 'eshell-pre-command-hook #'eshell-command-started nil t)
      (add-hook 'eshell-post-command-hook #'eshell-command-finished nil t))

    (add-hook 'kill-buffer-hook #'eshell-kill-buffer-function t t)

    (if eshell-first-time-p
        (run-hooks 'eshell-first-time-mode-hook))
    (run-hooks 'eshell-post-command-hook)))
