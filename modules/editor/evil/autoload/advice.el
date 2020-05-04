;;; editor/evil/autoload/advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +evil-escape-a (&rest _)
  "Call `doom/escape' if `evil-force-normal-state' is called interactively."
  (when (called-interactively-p 'any)
    (call-interactively #'doom/escape)))

;;;###autoload
(defun +evil-replace-filename-modifiers-a (file-name)
  "Take a path and resolve any vim-like filename modifiers in it. This adds
support for most vim file modifiers, as well as:

  %:P   Resolves to `doom-project-root'.

See http://vimdoc.sourceforge.net/htmldoc/cmdline.html#filename-modifiers for
more information on modifiers."
  (let ((origin-buffer (current-buffer))
        case-fold-search)
    (with-temp-buffer
      (let ((buffer-file-name (buffer-file-name origin-buffer)))
        (save-excursion (insert file-name))
        (while (re-search-forward "\\(^\\|[^\\\\]\\)\\(\\([%#]\\)\\(:\\([PphtreS~.]\\|g?s\\)\\)*\\)" nil t)
          (if (null buffer-file-name)
              (replace-match (match-string 1) t t nil 2)
            (let ((beg (match-beginning 2))
                  (end (match-end 3))
                  (path (pcase (match-string 3)
                          ("%" (file-relative-name buffer-file-name default-directory))
                          ("#" (and (other-buffer origin-buffer)
                                    (buffer-file-name (other-buffer origin-buffer)))))))
              (save-match-data
                (goto-char beg)
                (while (re-search-forward ":\\([PphtreS~.]\\|g?s\\)" (+ (point) 3) t)
                  (let* ((modifier (match-string 1))
                         (global (string-prefix-p "gs" modifier)))
                    (when global
                      (setq modifier (substring modifier 1)))
                    (setq end (match-end 1)
                          path
                          (pcase (and path (substring modifier 0 1))
                            (`nil "")
                            ("p" (expand-file-name path))
                            ("~" (concat "~/" (file-relative-name path "~")))
                            ("." (file-relative-name path))
                            ("t" (file-name-nondirectory (directory-file-name path)))
                            ("r" (file-name-sans-extension path))
                            ("e" (file-name-extension path))
                            ("S" (shell-quote-argument path))
                            ("h"
                             (let ((parent (file-name-directory (expand-file-name path))))
                               (unless (file-equal-p path parent)
                                 (if (file-name-absolute-p path)
                                     (directory-file-name parent)
                                   (file-relative-name parent)))))
                            ("s"
                             (if (featurep 'evil)
                                 (when-let (args (evil-delimited-arguments (substring modifier 1) 2))
                                   (let ((pattern (evil-transform-vim-style-regexp (car args)))
                                         (replace (cadr args)))
                                     (replace-regexp-in-string
                                      (if global pattern (concat "\\(" pattern "\\).*\\'"))
                                      (evil-transform-vim-style-regexp replace) path t t
                                      (unless global 1))))
                               path))
                            ("P"
                             (let ((project-root (doom-project-root (file-name-directory (expand-file-name path)))))
                               (unless project-root
                                 (user-error "Not in a project"))
                               (abbreviate-file-name project-root)))))
                    ;; strip trailing slash, if applicable
                    (or (string-empty-p path)
                        (not (equal (substring path -1) "/"))
                        (setq path (substring path 0 -1))))))
              (replace-match path t t nil 2))))
        (replace-regexp-in-string "\\\\\\([#%]\\)" "\\1" (buffer-string) t)))))

(defun +evil--insert-newline (&optional above _noextranewline)
  (let ((pos (save-excursion (beginning-of-line-text) (point)))
        comment-auto-fill-only-comments)
    (require 'smartparens)
    (evil-narrow-to-field
      (if above
          (if (save-excursion (nth 4 (sp--syntax-ppss pos)))
              (evil-save-goal-column
                (setq evil-auto-indent nil)
                (goto-char pos)
                (let ((ws (abs (skip-chars-backward " \t"))))
                  ;; FIXME oh god why
                  (save-excursion
                    (if comment-line-break-function
                        (funcall comment-line-break-function nil)
                      (comment-indent-new-line))
                    (when (and (derived-mode-p 'c-mode 'c++-mode 'objc-mode 'java-mode 'js2-mode)
                               (eq (char-after) ?/))
                      (insert "*"))
                    (insert
                     (make-string (max 0 (+ ws (skip-chars-backward " \t")))
                                  32)))
                  (insert (make-string (max 1 ws) 32))))
            (evil-move-beginning-of-line)
            (insert (if use-hard-newlines hard-newline "\n"))
            (forward-line -1)
            (back-to-indentation))
        (evil-move-end-of-line)
        (cond ((sp-point-in-comment pos)
               (setq evil-auto-indent nil)
               (if comment-line-break-function
                   (funcall comment-line-break-function)
                 (comment-indent-new-line)))
              ;; TODO Find a better way to do this
              ((and (eq major-mode 'haskell-mode)
                    (fboundp 'haskell-indentation-newline-and-indent))
               (setq evil-auto-indent nil)
               (haskell-indentation-newline-and-indent))
              (t
               (insert (if use-hard-newlines hard-newline "\n"))
               (back-to-indentation)))))))

;;;###autoload
(defun +evil--insert-newline-below-and-respect-comments-a (orig-fn count)
  (if (or (not +evil-want-o/O-to-continue-comments)
          (not (eq this-command 'evil-open-below))
          (evil-insert-state-p))
      (funcall orig-fn count)
    (letf! (defun evil-insert-newline-below () (+evil--insert-newline))
      (let ((evil-auto-indent evil-auto-indent))
        (funcall orig-fn count)))))

;;;###autoload
(defun +evil--insert-newline-above-and-respect-comments-a (orig-fn count)
  (if (or (not +evil-want-o/O-to-continue-comments)
          (not (eq this-command 'evil-open-above))
          (evil-insert-state-p))
      (funcall orig-fn count)
    (letf! (defun evil-insert-newline-above () (+evil--insert-newline 'above))
      (let ((evil-auto-indent evil-auto-indent))
        (funcall orig-fn count)))))

;;;###autoload (autoload '+evil-window-split-a "editor/evil/autoload/advice" nil t)
(evil-define-command +evil-window-split-a (&optional count file)
  "Same as `evil-window-split', but correctly updates the window history."
  :repeat nil
  (interactive "P<f>")
  ;; HACK This ping-ponging between the destination and source windows is to
  ;;      update the window focus history, so that, if you close either split
  ;;      afterwards you won't be sent to some random window.
  (let ((doom-inhibit-switch-window-hooks t)
        (origwin (selected-window)))
    (select-window (split-window origwin count 'below))
    (unless evil-split-window-below
      (select-window origwin))
    (run-hooks 'doom-switch-window-hook))
  (recenter)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file (evil-edit file)))

;;;###autoload (autoload '+evil-window-vsplit-a "editor/evil/autoload/advice" nil t)
(evil-define-command +evil-window-vsplit-a (&optional count file)
  "Same as `evil-window-split', but correctly updates the window history."
  :repeat nil
  (interactive "P<f>")
  ;; HACK This ping-ponging between the destination and source windows is to
  ;;      update the window focus history, so that, if you close either split
  ;;      afterwards you won't be sent to some random window.
  (let ((doom-inhibit-switch-window-hooks t)
        (origwin (selected-window)))
    (select-window (split-window origwin count 'right))
    (unless evil-vsplit-window-right
      (select-window origwin))
    (run-hooks 'doom-switch-window-hook))
  (run-hooks)
  (recenter)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file (evil-edit file)))

;;;###autoload
(defun +evil--fix-dabbrev-in-minibuffer-h ()
  "Make `try-expand-dabbrev' from `hippie-expand' work in minibuffer. See
`he-dabbrev-beg', so we need to redefine syntax for '/'."
  (set-syntax-table (let* ((table (make-syntax-table)))
                      (modify-syntax-entry ?/ "." table)
                      table)))
