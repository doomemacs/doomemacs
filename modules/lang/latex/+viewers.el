;;; lang/latex/+viewers.el -*- lexical-binding: t; -*-

;; Fall back PDF previewing to `latex-preview-pane-mode'.
(add-to-list 'TeX-view-program-selection '(output-pdf "preview-pane") 'append)
(add-to-list 'TeX-view-program-list '("preview-pane" latex-preview-pane-mode))

(letf! (defun prepend-to-list (list-var value &optional append)
         (set list-var (delete value (symbol-value list-var)))
         (add-to-list list-var value append))
  (dolist (viewer (reverse +latex-viewers))
    (pcase viewer
      (`skim
       (when-let
           (app-path
            (and (featurep :system 'macos)
                 (file-exists-p! (or "/Applications/Skim.app"
                                     "~/Applications/Skim.app"))))
         (prepend-to-list 'TeX-view-program-selection '(output-pdf "Skim"))
         (add-to-list 'TeX-view-program-list
                      (list "Skim" (format "%s/Contents/SharedSupport/displayline -r -b %%n %%o %%b"
                                           app-path)))))

      (`sumatrapdf
       (when (and (featurep :system 'windows)
                  (executable-find "SumatraPDF"))
         (prepend-to-list 'TeX-view-program-selection '(output-pdf "SumatraPDF"))))

      (`okular
       (when (executable-find "okular")
         ;; Configure Okular as viewer. Including a bug fix
         ;; (https://bugs.kde.org/show_bug.cgi?id=373855).
         (add-to-list 'TeX-view-program-list '("Okular" ("okular --noraise --unique file:%o" (mode-io-correlate "#src:%n%a"))))
         (prepend-to-list 'TeX-view-program-selection '(output-pdf "Okular"))))

      (`zathura
       (when (executable-find "zathura")
         (prepend-to-list 'TeX-view-program-selection '(output-pdf "Zathura"))))

      (`evince
       (when (executable-find "evince")
         (prepend-to-list 'TeX-view-program-selection '(output-pdf "Evince"))))

      (`pdf-tools
       (when (modulep! :tools pdf)
         (prepend-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
         (when (featurep :system 'macos)
           ;; PDF Tools isn't in `TeX-view-program-list-builtin' on macs.
           (add-to-list 'TeX-view-program-list '("PDF Tools" TeX-pdf-tools-sync-view)))
         ;; Update PDF buffers after successful LaTeX runs.
         (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))))))


(after! latex-preview-pane
  (setq latex-preview-pane-multifile-mode 'auctex)

  ;; TODO: PR this to maintained fork by arifer48. The original project appears abandoned.
  (defadvice! +latex--dont-reopen-preview-pane-a (fn &rest args)
    "Once the preview pane has been closed it should not be reopened."
    :around #'latex-preview-pane-update
    (letf! (defun init-latex-preview-pane (&rest _)
             ;; HACK Avoid the function because it tries to delete the current
             ;;      window, but it's already gone, so it ends up deleting the
             ;;      wrong window.
             (setq-local latex-preview-pane-mode nil))
      (apply fn args)))

  (define-key! doc-view-mode-map
    "ESC" #'delete-window
    "q"   #'delete-window
    "k"   (cmd! (quit-window) (delete-window))))
