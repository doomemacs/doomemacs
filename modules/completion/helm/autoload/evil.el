;;; completion/helm/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor evil)

;;;###autoload (autoload '+helm:project-search "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:project-search (all-files-p query)
  "Ex interface for `+helm/grep'"
  (interactive "<!><a>")
  (+helm/project-search all-files-p query))

;;;###autoload (autoload '+helm:project-search-from-cwd "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:project-search-from-cwd (query &optional recurse-p)
  "Ex interface for `+helm/grep-from-cwd'."
  (interactive "<a><!>")
  (+helm/project-search-from-cwd (not recurse-p) query))

;;;###autoload
(defun +helm--set-prompt-display (pos)
  "TODO"
  (let (beg state region-active m)
    (with-selected-window (minibuffer-window)
      (setq beg (save-excursion (vertical-motion 0 (helm-window)) (point))
            state evil-state
            region-active (region-active-p)
            m (mark t)))
    (when region-active
      (setq m (- m beg))
      ;; Increment pos to handle the space before prompt (i.e `pref').
      (put-text-property (1+ (min m pos)) (+ 2 (max m pos))
                         'face
                         (list :background (face-background 'region))
                         header-line-format))
    (put-text-property
     ;; Increment pos to handle the space before prompt (i.e `pref').
     (+ 1 pos) (+ 2 pos)
     'face
     (if (eq state 'insert)
         'underline
       ;; Don't just use 'cursor, this can hide the current character.
       (list :inverse-video t
             :foreground (face-background 'cursor)
             :background (face-background 'default)))
     header-line-format)))
