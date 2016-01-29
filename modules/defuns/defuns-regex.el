;;; defuns-regex.el

;;;###autoload
(defun narf|reb-cleanup ()
  (replace-regexp "^[ \n]*" "" nil (point-min) (point-max))
  (text-scale-set 2)
  (goto-char 2))

;;;###autoload (autoload 'narf:regex "defuns-regex" nil t)
(evil-define-operator narf:regex (beg end type &optional regexstr bang)
  "Either a) converts selected (or entered-in) pcre regex into elisp
regex, OR b) opens up re-builder."
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
          (t (re-builder)))))

(provide 'defuns-regex)
;;; defuns-regex.el ends here
