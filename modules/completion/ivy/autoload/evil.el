;; completion/ivy/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :feature evil)

;;;###autoload (autoload '+ivy:swiper "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:swiper (&optional search)
  "Invoke `swiper' with SEARCH, otherwise with the symbol at point."
  (interactive "<a>")
  (swiper search))

;;;###autoload (autoload '+ivy:todo "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:todo (&optional bang)
  "An ex wrapper around `+ivy/tasks'."
  (interactive "<!>")
  (+ivy/tasks bang))


;;
;; Project searching

;;;###autoload (autoload '+ivy:pt "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:pt (all-files-p query)
  "Ex interface for `+ivy/pt'"
  (interactive "<!><a>")
  (+ivy/pt all-files-p query))

;;;###autoload (autoload '+ivy:grep "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:grep (all-files-p query)
  "Ex interface for `+ivy/grep'"
  (interactive "<!><a>")
  (+ivy/grep all-files-p query))

;;;###autoload (autoload '+ivy:ag "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:ag (all-files-p query)
  "Ex interface for `+ivy/ag'"
  (interactive "<!><a>")
  (+ivy/ag all-files-p query))

;;;###autoload (autoload '+ivy:rg "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:rg (all-files-p query)
  "Ex interface for `+ivy/rg'"
  (interactive "<!><a>")
  (+ivy/rg all-files-p query))


;;;###autoload (autoload '+ivy:pt-from-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:pt-from-cwd (query &optional recurse-p)
  "Ex interface for `+ivy/pt-from-cwd'."
  (interactive "<a><!>")
  (+ivy/pt-from-cwd (not recurse-p) query))

;;;###autoload (autoload '+ivy:grep-from-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:grep-from-cwd (query &optional recurse-p)
  "Ex interface for `+ivy/grep-from-cwd'."
  (interactive "<a><!>")
  (+ivy/grep-from-cwd (not recurse-p) query))

;;;###autoload (autoload '+ivy:ag-from-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:ag-from-cwd (query &optional recurse-p)
  "Ex interface for `+ivy/ag-from-cwd'."
  (interactive "<a><!>")
  (+ivy/ag-from-cwd (not recurse-p) query))

;;;###autoload (autoload '+ivy:rg-from-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:rg-from-cwd (query &optional recurse-p)
  "Ex interface for `+ivy/rg-from-cwd'."
  (interactive "<a><!>")
  (+ivy/rg-from-cwd (not recurse-p) query))

