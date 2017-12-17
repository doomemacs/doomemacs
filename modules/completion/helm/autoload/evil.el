;;; completion/helm/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :feature evil)

;;;###autoload (autoload '+helm:swoop "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:swoop (&optional search bang)
  "Invoke `swoop' with SEARCH. If BANG, do multiline search."
  (interactive "<a><!>")
  (helm-swoop :$query search :$multiline bang))

(defun +helm--file-search (beg end query &optional directory options)
  (require 'helm-ag)
  (helm-ag--init-state)
  (let ((helm-ag--default-directory (or directory (doom-project-root)))
        (query (or query
                   (if (evil-visual-state-p)
                       (and beg end
                            (> (abs (- end beg)) 1)
                            (rxt-quote-pcre (buffer-substring-no-properties beg end)))
                     +helm--file-last-query)
                   +helm--file-last-query))
        (helm-ag-command-option (concat helm-ag-command-option " " (string-join options " "))))
    (setq helm-ag--last-query query)
    (helm-attrset 'search-this-file nil helm-ag-source)
    (helm-attrset 'name (helm-ag--helm-header helm-ag--default-directory) helm-ag-source)
    (helm :sources '(helm-ag-source)
          :input query
          :buffer "*helm-ag*"
          :keymap helm-ag-map
          :history 'helm-ag--helm-history)))

(defvar +helm--file-last-search nil)
;;;###autoload (autoload '+helm:ag "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:ag (beg end query &optional bang)
  "TODO"
  (interactive "<r><a><!>")
  (+helm--file-search beg end query nil
                      (if bang (list "-a" "--hidden"))))

;;;###autoload (autoload '+helm:ag-cwd "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:ag-cwd (beg end query &optional bang)
  "TODO"
  (interactive "<r><a><!>")
  (+helm--file-search beg end query default-directory
                      (list "-n" (if bang "-a"))))

;;;###autoload (autoload '+helm:rg "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:rg (beg end query &optional bang)
  "TODO"
  (interactive "<r><a><!>")
  (let ((helm-ag-base-command "rg --no-heading"))
    (+helm--file-search beg end query nil
                        (if bang (list "-uu")))))

;;;###autoload (autoload '+helm:rg-cwd "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:rg-cwd (beg end query &optional bang)
  "TODO"
  (interactive "<r><a><!>")
  (let ((helm-ag-base-command "rg --no-heading --maxdepth 1"))
    (+helm--file-search beg end query default-directory
                        (if bang (list "-uu")))))
