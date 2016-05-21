;;; defuns-ido.el

;;;###autoload
(defun doom*ido-sort-mtime ()
  "Sort ido filelist by mtime instead of alphabetically."
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (ignore-errors
                  (time-less-p
                   (sixth (file-attributes (concat ido-current-directory b)))
                   (sixth (file-attributes (concat ido-current-directory a))))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (and (char-equal (string-to-char x) ?.) x))
              ido-temp-list))))

;;;###autoload
(defun doom|ido-setup-home-keybind ()
  "Go to $HOME with ~"
  (define-key ido-file-completion-map (kbd "~")
    (λ! (if (looking-back "/")
            (insert "~/")
          (call-interactively 'self-insert-command)))))

;;;###autoload
(defun doom/ido-find-file (&optional dir)
  (interactive)
  (let ((default-directory (or dir default-directory)))
    (ido-find-file)))

;;;###autoload
(defun doom/ido-find-file-other-window (&optional dir)
  (interactive)
  (let ((default-directory (or dir default-directory)))
    (ido-find-file-other-window)))

;;;###autoload
(defun doom/ido-find-project-file ()
  (interactive)
  (let ((default-directory (doom/project-root)))
    (ido-find-file)))

;;;###autoload
(defun doom/ido-recentf ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;;###autoload (autoload 'doom:ido-find-file-in-emacsd "defuns-ido" nil t)
(evil-define-command doom:ido-find-file-in-emacsd (&optional bang) :repeat nil
  (interactive "<!>")
  (if bang
      (ido-find-file-in-dir doom-modules-dir)
    (ido-find-file-in-dir doom-emacs-dir)))

(provide 'defuns-ido)
;;; defuns-ido.el ends here
