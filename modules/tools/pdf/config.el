;;; tools/pdf/config.el -*- lexical-binding: t; -*-

(use-package! pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (unless noninteractive
    (pdf-tools-install))

  (map! :map pdf-view-mode-map :gn "q" #'kill-current-buffer)

  (defun +pdf-cleanup-windows-h ()
    "Kill left-over annotation buffers when the document is killed."
    (when (buffer-live-p pdf-annot-list-document-buffer)
      (pdf-info-close pdf-annot-list-document-buffer))
    (when (buffer-live-p pdf-annot-list-buffer)
      (kill-buffer pdf-annot-list-buffer))
    (let ((contents-buffer (get-buffer "*Contents*")))
      (when (and contents-buffer (buffer-live-p contents-buffer))
        (kill-buffer contents-buffer))))
  (add-hook! 'pdf-view-mode-hook
    (add-hook 'kill-buffer-hook #'+pdf-cleanup-windows-h nil t))

  (setq-default pdf-view-display-size 'fit-page
                pdf-view-use-scaling t
                pdf-view-use-imagemagick nil)

  (advice-add 'pdf-annot-show-annotation :override #'*pdf-pdf-annot-show-annotation)
  (advice-add 'pdf-isearch-hl-matches :override #'*pdf-pdf-isearch-hl-matches)
  (advice-add 'pdf-util-frame-scale-factor :override #'*pdf-pdf-util-frame-scale-factor)
  (advice-add 'pdf-view-display-region :override #'*pdf-pdf-view-display-region)
  (advice-add 'pdf-view-use-scaling-p :override #'*pdf-pdf-view-use-scaling-p)
  ;; Turn off cua so copy works
  (add-hook! 'pdf-view-mode-hook (cua-mode 0))
  ;; Handle PDF-tools related popups better
  (set-popup-rule! "^\\*Outline*" :side 'right :size 40 :select nil)
  ;; The next rules are not needed, they are defined in modules/ui/popups/+hacks.el
  ;; (set-popup-rule! "\\*Contents\\*" :side 'right :size 40)
  ;; (set-popup-rule! "* annots\\*$" :side 'left :size 40 :select nil)
  ;; Fix #1107: flickering pdfs when evil-mode is enabled
  (setq-hook! 'pdf-view-mode-hook evil-normal-state-cursor (list nil)))
