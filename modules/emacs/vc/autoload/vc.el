;;; emacs/vc/autoload/vc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +vc-git-root-url ()
  "Return the root git repo URL for the current file."
  (require 'git-link)
  (let* ((remote (git-link--select-remote))
         (remote-url (git-link--remote-url remote))
         (remote-info (if remote-url (git-link--parse-remote remote-url))))
    (if remote-info
        (format "https://%s/%s" (car remote-info) (cadr remote-info))
      (error  "Remote `%s' is unknown or contains an unsupported URL" remote))))

(defvar git-link-open-in-browser)
;;;###autoload
(defun +vc/git-browse (arg)
  "Open the website for the current version controlled file. Fallback to
repository root."
  (interactive "P")
  (require 'git-link)
  (cl-destructuring-bind (beg end)
      (if buffer-file-name (git-link--get-region))
    (let ((git-link-open-in-browser (not arg)))
      (git-link (git-link--select-remote) beg end))))

;;;###autoload
(defun +vc/git-browse-issues (arg)
  "Open the issues page for current repo."
  (interactive "P")
  (let ((url (format "%s/issues" (+vc-git-root-url))))
    (if arg
        (message "%s" (kill-new url))
      (browse-url url))))

;;;###autoload
(defun +vc/git-browse-pulls ()
  "Open the pull requests page for current repo."
  (interactive)
  (browse-url (format "%s/pulls" (+vc-git-root-url))))

;;;###autoload
(defun +vc*update-header-line (revision)
  "Show revision details in the header-line, instead of the minibuffer.

Sometimes I forget `git-timemachine' is enabled in a buffer. Putting revision
info in the `header-line-format' is a good indication."
  (let* ((date-relative (nth 3 revision))
         (date-full (nth 4 revision))
         (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
         (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
    (setq header-line-format
          (format "%s%s [%s (%s)]"
                  (propertize author 'face 'git-timemachine-minibuffer-author-face)
                  (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face)
                  date-full date-relative))))

;;;###autoload (autoload '+vc-smerge-hydra/body "emacs/vc/autoload" nil t)
(defhydra +vc-smerge-hydra (:hint nil
                            :pre (if (not smerge-mode) (smerge-mode 1))
                            ;; Disable `smerge-mode' when quitting hydra if
                            ;; no merge conflicts remain.
                            :post (smerge-auto-leave))
    "
                                                         [smerge]
  Movement   Keep           Diff              Other
  ╭─────────────────────────────────────────────────────────╯
     ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
     ^_C-k_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
     ^_k_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
     ^_j_ ↓^     [_a_] all        [_H_] hightlight
     ^_C-j_^     [_RET_] current  [_E_] ediff                 ╭──────────
     ^_G_^                                                │ [_q_] quit
"
    ("g" (progn (goto-char (point-min)) (smerge-next)))
    ("G" (progn (goto-char (point-max)) (smerge-prev)))
    ("C-j" smerge-next)
    ("C-k" smerge-prev)
    ("j" next-line)
    ("k" previous-line)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("H" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("R" smerge-kill-current)
    ("q" nil :color blue))
