(require 'helm)
(require 'helm-ag)

(defun helm-todo--candidate-transformer (candidate)
  (when (string-match "\\`\\([^:]+\\):\\([^:]+\\):\\(.+\\)" candidate)
    (format "%s %s %s"
            (propertize (match-string 1 candidate) 'face 'helm-moccur-buffer)
            (propertize "TODO" 'face 'helm-grep-lineno)
            (helm-ag--highlight-candidate
             (replace-regexp-in-string "^\\( *\\(/\\**\\|/+\\|[;#]+\\) *\\)+TODO ?" ""
                                       (match-string 3 candidate))))))

(defvar helm-todo-source
  '((name . "TODOs")
    (init . helm-ag-init)
    (candidates-in-buffer)
    (persistent-action . helm-ag-persistent-action)
    (real-to-display . helm-todo--candidate-transformer)
    (action . (("Open file" . helm-ag--action-find-file)
               ("Open file other window" . helm-ag--action--find-file-other-window)))))

;;;###autoload (autoload 'my:helm-todo "helm-todo")
(evil-define-operator my:helm-todo (beg end &optional bang input)
  :motion nil
  :move-point nil
  :type inclusive
  :repeat nil
  (interactive "<r><!><a>")
  (let* ((helm-ag-default-directory (my--project-root bang))
         (input (or input ""))
         (helm-ag--last-input ""))
    (helm-attrset 'search-this-file nil helm-ag-source)
    (setq helm-ag--last-query
          "ag --nogroup --nocolor --ignore '*.md' -- '(/\\*\\*?|//|[;#]) *TODO'")
    (helm-attrset 'name
                  (format "TODOs in %s" helm-ag-default-directory)
                  helm-ag-source)
    (helm :sources 'helm-todo-source
          :buffer "*helm-ag*"
          :input input
          :prompt helm-global-prompt)))


(provide 'helm-todo)
;;; helm-todo.el ends here
