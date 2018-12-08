;;; completion/helm/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :feature evil)

;;
;; Project searching

;;;###autoload (autoload '+helm:pt "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:pt (all-files-p query)
  "Ex interface for `+helm/pt'"
  (interactive "<!><a>")
  (+helm/pt all-files-p query))

;;;###autoload (autoload '+helm:grep "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:grep (all-files-p query)
  "Ex interface for `+helm/grep'"
  (interactive "<!><a>")
  (+helm/grep all-files-p query))

;;;###autoload (autoload '+helm:ag "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:ag (all-files-p query)
  "Ex interface for `+helm/ag'"
  (interactive "<!><a>")
  (+helm/ag all-files-p query))

;;;###autoload (autoload '+helm:rg "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:rg (all-files-p query)
  "Ex interface for `+helm/rg'"
  (interactive "<!><a>")
  (+helm/rg all-files-p query))


;;;###autoload (autoload '+helm:pt-from-cwd "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:pt-from-cwd (query &optional recurse-p)
  "Ex interface for `+helm/pt-from-cwd'."
  (interactive "<a><!>")
  (+helm/pt-from-cwd (not recurse-p) query))

;;;###autoload (autoload '+helm:grep-from-cwd "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:grep-from-cwd (query &optional recurse-p)
  "Ex interface for `+helm/grep-from-cwd'."
  (interactive "<a><!>")
  (+helm/grep-from-cwd (not recurse-p) query))

;;;###autoload (autoload '+helm:ag-from-cwd "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:ag-from-cwd (query &optional recurse-p)
  "Ex interface for `+helm/ag-from-cwd'."
  (interactive "<a><!>")
  (+helm/ag-from-cwd (not recurse-p) query))

;;;###autoload (autoload '+helm:rg-from-cwd "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:rg-from-cwd (query &optional recurse-p)
  "Ex interface for `+helm/rg-from-cwd'."
  (interactive "<a><!>")
  (+helm/rg-from-cwd (not recurse-p) query))


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
