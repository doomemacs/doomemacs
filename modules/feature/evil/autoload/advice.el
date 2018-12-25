;;; feature/evil/autoload/advice.el -*- lexical-binding: t; -*-

(defun +evil--insert-newline (&optional above noextranewline)
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
                        (funcall comment-line-break-function)
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
              ;; Find a better way to do this
              ((and (eq major-mode 'haskell-mode)
                    (fboundp 'haskell-indentation-newline-and-indent))
               (setq evil-auto-indent nil)
               (haskell-indentation-newline-and-indent))
              (t
               (insert (if use-hard-newlines hard-newline "\n"))
               (back-to-indentation)))))))

;;;###autoload
(defun +evil*insert-newline-below-and-respect-comments (orig-fn count)
  (if (or (not +evil-want-o/O-to-continue-comments)
          (not (eq this-command 'evil-open-below))
          (evil-insert-state-p))
      (funcall orig-fn count)
    (cl-letf (((symbol-function 'evil-insert-newline-below)
               (lambda () (+evil--insert-newline))))
      (let ((evil-auto-indent evil-auto-indent))
        (funcall orig-fn count)))))

;;;###autoload
(defun +evil*insert-newline-above-and-respect-comments (orig-fn count)
  (if (or (not +evil-want-o/O-to-continue-comments)
          (not (eq this-command 'evil-open-above))
          (evil-insert-state-p))
      (funcall orig-fn count)
    (cl-letf (((symbol-function 'evil-insert-newline-above)
               (lambda () (+evil--insert-newline 'above))))
      (let ((evil-auto-indent evil-auto-indent))
        (funcall orig-fn count)))))

;;;###autoload
(defun +evil*static-reindent (orig-fn &rest args)
  "Don't move cursor on indent."
  (save-excursion (apply orig-fn args)))

;;;###autoload
(defun +evil*resolve-vim-path (file-name)
  "Take a path and resolve any vim-like filename modifiers in it. This adds
support for most vim file modifiers, as well as:

  %:P   Resolves to `doom-project-root'.

See http://vimdoc.sourceforge.net/htmldoc/cmdline.html#filename-modifiers for
more information on modifiers."
  (let* (case-fold-search
         (regexp (concat "\\(?:^\\|[^\\\\]\\)"
                         "\\([#%]\\)"
                         "\\(\\(?::\\(?:[PphtreS~.]\\|g?s[^:\t\n ]+\\)\\)*\\)"))
         (matches
          (cl-loop with i = 0
                   while (and (< i (length file-name))
                              (string-match regexp file-name i))
                   do (setq i (1+ (match-beginning 0)))
                   and collect
                   (cl-loop for j to (/ (length (match-data)) 2)
                            collect (match-string j file-name)))))
    (dolist (match matches)
      (let ((flags (split-string (car (cdr (cdr match))) ":" t))
            (path (and buffer-file-name
                       (pcase (car (cdr match))
                         ("%" (file-relative-name buffer-file-name))
                         ("#" (save-excursion (other-window 1) (file-relative-name buffer-file-name))))))
            flag global)
        (if (not path)
            (setq path "")
          (while flags
            (setq flag (pop flags))
            (when (string-suffix-p "\\" flag)
              (setq flag (concat flag (pop flags))))
            (when (string-prefix-p "gs" flag)
              (setq global t
                    flag (substring flag 1)))
            (setq path
                  (or (pcase (substring flag 0 1)
                        ("p" (expand-file-name path))
                        ("~" (concat "~/" (file-relative-name path "~")))
                        ("." (file-relative-name path default-directory))
                        ("t" (file-name-nondirectory (directory-file-name path)))
                        ("r" (file-name-sans-extension path))
                        ("e" (file-name-extension path))
                        ("S" (shell-quote-argument path))
                        ("h"
                         (let ((parent (file-name-directory (expand-file-name path))))
                           (unless (equal (file-truename path)
                                          (file-truename parent))
                             (if (file-name-absolute-p path)
                                 (directory-file-name parent)
                               (file-relative-name parent)))))
                        ("s"
                         (if (featurep 'evil)
                             (when-let* ((args (evil-delimited-arguments (substring flag 1) 2)))
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
                           (abbreviate-file-name project-root)))
                        (_ path))
                      "")))
          ;; strip trailing slash, if applicable
          (when (and (not (string= path "")) (equal (substring path -1) "/"))
            (setq path (substring path 0 -1))))
        (setq file-name
              (replace-regexp-in-string (format "\\(?:^\\|[^\\\\]\\)\\(%s\\)"
                                                (regexp-quote (string-trim-left (car match))))
                                        path file-name t t 1))))
    (replace-regexp-in-string regexp "\\1" file-name t)))

;;;###autoload (autoload '+evil*window-split "feature/evil/autoload/advice" nil t)
(evil-define-command +evil*window-split (&optional count file)
  "Same as `evil-window-split', but focuses (and recenters) the new split."
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count
                (if evil-split-window-below 'above 'below))
  (call-interactively
   (if evil-split-window-below
       #'evil-window-up
     #'evil-window-down))
  (recenter)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file (evil-edit file)))

;;;###autoload (autoload '+evil*window-vsplit "feature/evil/autoload/advice" nil t)
(evil-define-command +evil*window-vsplit (&optional count file)
  "Same as `evil-window-vsplit', but focuses (and recenters) the new split."
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count
                (if evil-vsplit-window-right 'left 'right))
  (call-interactively
   (if evil-vsplit-window-right
       #'evil-window-left
     #'evil-window-right))
  (recenter)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (if file (evil-edit file)))

;;;###autoload
(defun +evil*escape (&rest _)
  "Call `doom/escape' if `evil-force-normal-state' is called interactively."
  (when (called-interactively-p 'any)
    (call-interactively #'doom/escape)))
