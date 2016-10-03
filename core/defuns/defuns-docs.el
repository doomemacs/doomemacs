;;; defuns-docs.el

;;;###autoload (autoload 'doom:docs-lookup "defuns-docs" nil t)
(evil-define-command doom:docs-lookup (&optional bang input)
  "Look up INPUT (otherwise the current selection) in Dash or Zeal."
  (interactive "<!><a>")
  (let ((query input))
    (when (evil-visual-state-p)
      (setq query (concat (buffer-substring-no-properties (region-beginning) (region-end))
                          " " query)))
    (when (or (not query) (zerop (length query)))
      (setq query (thing-at-point 'symbol)))
    (doom-docs-lookup query bang)))

;;;###autoload (autoload 'doom:google-search "defuns-docs" nil t)
(evil-define-command doom:google-search (&optional bang search)
  "Opens a browser and performs the entered google search. If BANG, use 'I'm
feeling lucky'."
  (interactive "<!><a>")
  (if search
      (google-this-parse-and-search-string
       search nil
       (if bang (google-this-lucky-search-url)))
    (cond ((eq major-mode 'c++-mode)
           (google-this-cpp-reference))
          ((evil-visual-state-p)
           (google-this-region nil))
          (t (google-this-symbol nil)))))

(provide 'defuns-docs)
;;; defuns-docs.el ends here
