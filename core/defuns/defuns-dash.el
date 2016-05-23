;;; defuns-dash.el --- helm-dash integration

;;;###autoload
(defun doom/dash-at-pt ()
  (interactive)
  (let (helm-dash-common-docsets)
    (call-interactively 'helm-dash-at-point)))

;;;###autoload
(defun doom/dash-open (url)
  "Open url in an *eww* popup."
  (interactive)
  (save-window-excursion (eww url))
  (doom/popup-buffer "*eww*"))

;;;###autoload (autoload 'doom:dash "defuns-dash" nil t)
(evil-define-command doom:dash (&optional bang input)
  ""
  (interactive "<!><a>")
  (let ((helm-dash-common-docsets (when bang (helm-dash-installed-docsets))))
    (helm-dash-initialize-debugging-buffer)
    (helm-dash-create-common-connections)
    (helm-dash-create-buffer-connections)
    (helm :sources (helm-dash-sources--narrowed-docsets)
          :buffer "*helm-dash*"
          :prompt "Doc for: "
          :history 'helm-dash-history-input
          :input (or input "")
          :helm-candidate-number-limit 500)))

(defvar doom--dash-docsets nil)
(defvar doom--dash-user-docsets nil)
;;;###autoload (autoload 'doom:dash-install "defuns-dash" nil t)
(evil-define-command doom:dash-install (&optional docset)
  "Async docset install."
  (interactive "<a>")
  (let ((docsets (or doom--dash-docsets
                     (setq doom--dash-docsets (helm-dash-available-docsets))))
        (user-docsets (or doom--dash-user-docsets
                          (setq doom--dash-user-docsets (helm-dash-search-all-user-docsets)))))
    (unless docset
      (setq docset (helm-dash-read-docset
                    "Install docset"
                    (append docsets (mapcar 'car user-docsets)))))
    (message "Installing ...")
    (async-start `(lambda ()
                    ,(async-inject-variables "\\`\\(load-path\\|helm-dash-*\\)$")
                    (require 'f)
                    (require 'helm)
                    (require 'helm-dash)
                    (cond ((member ,docset ',docsets)
                           (helm-dash-install-docset ,docset)
                           ,docset)
                          ((assoc ,docset ',user-docsets)
                           (helm-dash-install-user-docset ,docset)
                           (f-no-ext (f-base (car-safe (cdr-safe (assoc ,docset ',user-docsets))))))))
                 (lambda (doc)
                   (unless doc
                     (error "Invalid docset id" doc))
                   (message "Installing docset %s ..." doc)
                   (helm-dash-activate-docset doc)
                   (setq helm-dash-common-docsets nil)
                   (message "Docset %s installed" doc)))))

(provide 'defuns-dash)
;;; defuns-dash.el ends here
