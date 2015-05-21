(use-package re-builder
  :defer t
  :config
  (progn
    (bind 'normal reb-mode-map
          [escape]     'reb-quit
          (kbd "C-g")  'reb-quit
          [backtab]    'reb-change-syntax)

    (defun my--reb-cleanup ()
      (replace-regexp "^[ \n]*" "" nil (point-min) (point-max))
      (text-scale-set 1.5)
      (goto-char 2))
    (add-hook 'reb-mode-hook 'my--reb-cleanup)

    (use-package pcre2el
      :config
      (progn
        (bind 'normal rxt-help-mode-map [escape] 'kill-buffer-and-window)
        (setq reb-re-syntax 'pcre)))

    (after "evil"
      (evil-set-initial-state 'reb-mode 'insert)
      ;; Either a) converts selected (or entered-in) pcre regex into elisp
      ;; regex, OR b) opens up re-builder.
      (evil-define-operator my:regex (beg end type &optional regexstr bang)
        :move-point nil
        :type inclusive
        :repeat nil
        (interactive "<R><a><!>")
        (if (reb-mode-buffer-p)
            (if regexstr
                (let ((regexstr (s-trim (buffer-string))))
                  (if bang
                      (rxt-explain-pcre regexstr)
                    (rxt-pcre-to-elisp (s-trim (buffer-string)))))
              (erase-buffer)
              (insert (concat "/" regexstr "/")))
          (cond ((and beg end (/= beg (1- end))) ; Convert selection from pcre regex to elisp
                 (let ((regexstr (buffer-substring-no-properties beg end)))
                   (if bang
                       (rxt-explain-pcre (concat "/" regexstr "/"))
                     (delete-region beg end)
                     (insert (rxt-pcre-to-elisp regexstr)))))
                (regexstr ; Convert input regex into elisp regex
                 (let ((newregex (rxt-pcre-to-elisp regexstr)))
                   (when bang
                     (setq newregex (s-replace "\\" "\\\\" newregex)))
                   (kill-new newregex)
                   (message "Copied regex to kill ring: %s" newregex)))
                (t
                 (re-builder))))))))


(provide 'init-regex)
;;; init-regex.el ends here
