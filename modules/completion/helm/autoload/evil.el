;;; completion/helm/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+helm:swoop "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:swoop (&optional search bang)
  "Invoke `swoop' with SEARCH. If BANG, do multiline search."
  (interactive "<a><!>")
  (helm-swoop :$query search :$multiline bang))

(defvar +helm--file-last-search nil)
;;;###autoload (autoload '+helm:ag "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:ag (beg end query &optional bang directory)
  "TODO"
  (interactive "<r><a><!>")
  (require 'helm-ag)
  (helm-ag--init-state)
  (let ((helm-ag--default-directory (or directory (doom-project-root)))
        (helm-ag--last-query (or query
                                 (and beg end
                                      (> (abs (- end beg)) 1)
                                      (setq +helm--file-last-search
                                            (rxt-quote-pcre (buffer-substring-no-properties beg end))))
                                 +helm--file-last-search))
        (helm-ag-command-option (concat helm-ag-command-option (if bang " -a "))))
    (helm-attrset 'search-this-file nil helm-ag-source)
    (helm-attrset 'name (helm-ag--helm-header helm-ag--default-directory) helm-ag-source)
    (helm :sources '(helm-ag-source)
          :input query
          :buffer "*helm-ag*"
          :keymap helm-ag-map
          :history 'helm-ag--helm-history)))

;;;###autoload (autoload '+helm:ag-cwd "completion/helm/autoload/evil" nil t)
(evil-define-command +helm:ag-cwd (beg end query &optional bang directory)
  "TODO"
  (interactive "<r><a><!>")
  (let ((helm-ag-command-option (if bang " -n ")))
    (+helm:ag beg end query t default-directory)))

;;;###autoload
(defun +helm:rg (&rest _) (interactive) (error "Not implemented yet"))
;;;###autoload
(defun +helm:rg-cwd (&rest _) (interactive) (error "Not implemented yet"))
;;;###autoload
(defun +helm:todo (&rest _) (interactive) (error "Not implemented yet"))
