;; config/default/autoload/default.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +default/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun +default/browse-project ()
  (interactive) (doom-project-browse (doom-project-root)))
;; NOTE No need for find-in-project, use `projectile-find-file'

;;;###autoload
(defun +default/browse-templates ()
  (interactive) (doom-project-browse +file-templates-dir))
;;;###autoload
(defun +default/find-in-templates ()
  (interactive) (doom-project-find-file +file-templates-dir))

;;;###autoload
(defun +default/browse-emacsd ()
  (interactive) (doom-project-browse doom-emacs-dir))
;;;###autoload
(defun +default/find-in-emacsd ()
  (interactive) (doom-project-find-file doom-emacs-dir))

;;;###autoload
(defun +default/browse-notes ()
  (interactive) (doom-project-browse org-directory))
;;;###autoload
(defun +default/find-in-notes ()
  (interactive) (doom-project-find-file org-directory))

;;;###autoload
(defun +default/compile (arg)
  "Runs `compile' from the root of the current project.

If a compilation window is already open, recompile that instead.

If ARG (universal argument), runs `compile' from the current directory."
  (interactive "P")
  (if (and (bound-and-true-p compilation-in-progress)
           (buffer-live-p compilation-last-buffer))
      (recompile)
    (call-interactively
     (if arg
         #'projectile-compile-project
       #'compile))))

;;;###autoload
(defun +default/man-or-woman ()
  "Invoke `man' if man is installed, otherwise use `woman'."
  (interactive)
  (call-interactively
   (if (executable-find "man")
       #'man
     #'woman)))

;;;###autoload
(defalias '+default/newline #'newline)

;;;###autoload
(defun +default/new-buffer ()
  "TODO"
  (interactive)
  (if (featurep! 'evil)
      (call-interactively #'evil-buffer-new)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (funcall (default-value 'major-mode))))))

;;;###autoload
(defun +default/project-tasks ()
  "Invokes `+ivy/tasks' or `+helm/tasks', depending on which is available."
  (interactive)
  (cond ((featurep! :completion ivy) (+ivy/tasks))
        ((featurep! :completion helm) (+helm/tasks))))

;;;###autoload
(defun +default/newline-above ()
  "Insert an indented new line before the current one."
  (interactive)
  (if (featurep 'evil)
      (call-interactively 'evil-open-above)
    (beginning-of-line)
    (save-excursion (newline))
    (indent-according-to-mode)))

;;;###autoload
(defun +default/newline-below ()
  "Insert an indented new line after the current one."
  (interactive)
  (if (featurep 'evil)
      (call-interactively 'evil-open-below)
    (end-of-line)
    (newline-and-indent)))

;;;###autoload
(defun +default/yank-pop ()
  "Interactively select what text to insert from the kill ring."
  (interactive)
  (call-interactively
   (cond ((fboundp 'counsel-yank-pop)    #'counsel-yank-pop)
         ((fboundp 'helm-show-kill-ring) #'helm-show-kill-ring)
         ((error "No kill-ring search backend available. Enable ivy or helm!")))))

;;;###autoload
(defun +default*newline-indent-and-continue-comments ()
  "A replacement for `newline-and-indent'.

Continues comments if executed from a commented line, with special support for
languages with weak native comment continuation support (like C-family
languages)."
  (interactive)
  (if (and (sp-point-in-comment)
           comment-line-break-function)
      (funcall comment-line-break-function)
    (newline nil t)
    (indent-according-to-mode)))

(defun doom--backward-delete-whitespace-to-column ()
  "Delete back to the previous column of whitespace, or as much whitespace as
possible, or just one char if that's not possible."
  (interactive)
  (let* ((context (ignore-errors (sp-get-thing)))
         (op (plist-get context :op))
         (cl (plist-get context :cl))
         open-len close-len)
    (cond ;; When in strings (sp acts weird with quotes; this is the fix)
          ;; Also, skip closing delimiters
          ((and op cl
                (string= op cl)
                (and (string= (char-to-string (or (char-before) 0)) op)
                     (setq open-len (length op)))
                (and (string= (char-to-string (or (char-after) 0)) cl)
                     (setq close-len (length cl))))
           (delete-char (- open-len))
           (delete-char close-len))

          ;; Delete up to the nearest tab column IF only whitespace between
          ;; point and bol.
          ((and (not indent-tabs-mode)
                (not (bolp))
                (not (sp-point-in-string))
                (save-excursion (>= (- (skip-chars-backward " \t")) tab-width)))
           (let ((movement (% (current-column) tab-width)))
             (when (= movement 0)
               (setq movement tab-width))
             (delete-char (- movement)))
           (unless (memq (char-before) (list ?\n ?\ ))
             (insert " ")))

          ;; Otherwise do a regular delete
          ((delete-char -1)))))

;;;###autoload
(defun +default*delete-backward-char (n &optional killflag)
  "Same as `delete-backward-char', but preforms these additional checks:

+ If point is surrounded by (balanced) whitespace and a brace delimiter ({} []
  ()), delete a space on either side of the cursor.
+ If point is at BOL and surrounded by braces on adjacent lines, collapse
  newlines:
  {
  |
  } => {|}
+ Otherwise, resort to `doom--backward-delete-whitespace-to-column'.
+ Resorts to `delete-char' if n > 1"
  (interactive "p\nP")
  (or (integerp n)
      (signal 'wrong-type-argument (list 'integerp n)))
  (cond ((and (use-region-p)
              delete-active-region
              (= n 1))
         ;; If a region is active, kill or delete it.
         (if (eq delete-active-region 'kill)
             (kill-region (region-beginning) (region-end) 'region)
           (funcall region-extract-function 'delete-only)))
        ;; In Overwrite mode, maybe untabify while deleting
        ((null (or (null overwrite-mode)
                   (<= n 0)
                   (memq (char-before) '(?\t ?\n))
                   (eobp)
                   (eq (char-after) ?\n)))
         (let ((ocol (current-column)))
           (delete-char (- n) killflag)
           (save-excursion
             (insert-char ?\s (- ocol (current-column)) nil))))
        ;;
        ((and (= n 1) (bound-and-true-p smartparens-mode))
         (cond ((and (memq (char-before) (list ?\  ?\t))
                     (save-excursion
                       (and (/= (skip-chars-backward " \t" (line-beginning-position)) 0)
                            (bolp))))
                (doom--backward-delete-whitespace-to-column))
               ((let* ((pair (ignore-errors (sp-get-thing)))
                       (op   (plist-get pair :op))
                       (cl   (plist-get pair :cl))
                       (beg  (plist-get pair :beg))
                       (end  (plist-get pair :end)))
                  (cond ((and end beg (= end (+ beg (length op) (length cl))))
                         (sp-backward-delete-char 1))
                        ((doom-surrounded-p pair 'inline 'balanced)
                         (delete-char -1 killflag)
                         (delete-char 1)
                         (when (= (point) (+ (length cl) beg))
                           (sp-backward-delete-char 1)
                           (sp-insert-pair op)))
                        ((and (bolp) (doom-surrounded-p pair nil 'balanced))
                         (delete-region beg end)
                         (sp-insert-pair op)
                         t)
                        ((run-hook-with-args-until-success 'doom-delete-backward-functions))
                        ((doom--backward-delete-whitespace-to-column)))))))
        ;; Otherwise, do simple deletion.
        ((delete-char (- n) killflag))))

;;;###autoload
(defun +default/search-from-cwd (&optional arg)
  "Conduct a text search in files under the current folder.
If prefix ARG is set, prompt for a directory to search from."
  (interactive "P")
  (let ((default-directory
          (if arg
              (read-directory-name "Switch to project: " default-directory)
            default-directory)))
    (call-interactively
     (cond ((featurep! :completion ivy)  #'+ivy/project-search-from-cwd)
           ((featurep! :completion helm) #'+helm/project-search-from-cwd)
           (#'projectile-grep)))))

;;;###autoload
(defun +default/search-project (&optional arg)
  "Conduct a text search in the current project root.
If prefix ARG is set, prompt for a known project to search from."
  (interactive "P")
  (let ((default-directory
          (if arg
              (if-let* ((projects (projectile-relevant-known-projects)))
                  (completing-read "Switch to project: " projects
                                   nil t nil nil (doom-project-root))
                (user-error "There are no known projects"))
            default-directory)))
    (call-interactively
     (cond ((featurep! :completion ivy)  #'+ivy/project-search)
           ((featurep! :completion helm) #'+helm/project-search)
           (#'rgrep)))))

;;;###autoload
(defun +default/search-project-for-symbol-at-point (&optional arg symbol)
  "Conduct a text search in the current project for symbol at point.
If prefix ARG is set, prompt for a known project to search from."
  (interactive
   (list current-prefix-arg (thing-at-point 'symbol t)))
  (let ((default-directory
          (if arg
              (if-let* ((projects (projectile-relevant-known-projects)))
                  (completing-read "Switch to project: " projects
                                   nil t nil nil (doom-project-root))
                (user-error "There are no known projects"))
            default-directory)))
    (cond ((featurep! :completion ivy)
           (+ivy/project-search nil (rxt-quote-pcre symbol)))
          ((featurep! :completion helm)
           (+helm/project-search nil (rxt-quote-pcre symbol)))
          ((rgrep (regexp-quote symbol))))))

;;;###autoload
(defun +default/search-notes-for-symbol-at-point (&optional arg symbol)
  "Conduct a text search in the current project for symbol at point. If prefix
ARG is set, prompt for a known project to search from."
  (interactive
   (list current-prefix-arg (thing-at-point 'symbol t)))
  (require 'org)
  (let ((default-directory org-directory))
    (+default/search-project-for-symbol-at-point
     nil symbol)))

;;;###autoload
(defun +default/org-notes-search ()
  "Perform a text search on `org-directory'."
  (interactive)
  (require 'org)
  (let ((default-directory org-directory))
    (+default/search-project-for-symbol-at-point nil "")))

;;;###autoload
(defun +default/org-notes-headlines ()
  "Jump to an Org headline in `org-agenda-files'."
  (interactive)
  (completing-read
   "Jump to org headline: "
   (doom-completing-read-org-headings org-agenda-files 3 t)))
