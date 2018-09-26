;;; tools/pdf/config.el -*- lexical-binding: t; -*-

(def-package! pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (unless noninteractive
    (pdf-tools-install))

  (define-key! pdf-view-mode-map
    "q" #'kill-this-buffer
    (kbd doom-leader-key) nil)

  (when (featurep! :feature evil +everywhere)
    (evil-define-key* 'normal pdf-view-mode-map
      "q" #'kill-this-buffer))

  (defun +pdf|cleanup-windows ()
    "Kill left-over annotation buffers when the document is killed."
    (when (buffer-live-p pdf-annot-list-document-buffer)
      (pdf-info-close pdf-annot-list-document-buffer))
    (when (buffer-live-p pdf-annot-list-buffer)
      (kill-buffer pdf-annot-list-buffer))
    (let ((contents-buffer (get-buffer "*Contents*")))
      (when (and contents-buffer (buffer-live-p contents-buffer))
        (kill-buffer contents-buffer))))
  (add-hook! 'pdf-view-mode-hook
    (add-hook 'kill-buffer-hook #'+pdf|cleanup-windows nil t))

  (setq-default pdf-view-display-size 'fit-page)
  ;; Turn off cua so copy works
  (add-hook! 'pdf-view-mode-hook (cua-mode 0))
  ;; Custom modeline that removes useless info and adds page numbers
  (when (or (featurep! :ui doom-modeline) (featurep! :ui modeline))
    (load! "+modeline"))
  ;; Handle PDF-tools related popups better
  (set-popup-rule! "^\\*Outline*" :side 'right :size 40 :select nil)
  ;; TODO: Add additional important windows that should be handled differently
  ;; TODO: These two next rules don't work (they should), investigate
  ;; (set-popup-rule! "\\*Contents\\*" :side 'right :size 40)
  ;; (set-popup-rule! "* annots\\*$" :side 'left :size 40 :select nil)
  )
