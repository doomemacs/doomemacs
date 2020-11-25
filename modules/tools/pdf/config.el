;;; tools/pdf/config.el -*- lexical-binding: t; -*-

(use-package! pdf-view
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init
  (after! pdf-annot
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
      (add-hook 'kill-buffer-hook #'+pdf-cleanup-windows-h nil t)))

  :config
  (map! :map pdf-view-mode-map :gn "q" #'kill-current-buffer)

  (setq-default pdf-view-display-size 'fit-page)
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)

  ;; Handle PDF-tools related popups better
  (set-popup-rules!
    '(("^\\*Outline*" :side right :size 40 :select nil)
      ("\\(?:^\\*Contents\\|'s annots\\*$\\)" :ignore t)))

  ;; The mode-line does serve any useful purpose is annotation windows
  (add-hook 'pdf-annot-list-mode-hook #'hide-mode-line-mode)

  ;; HACK Fix #1107: flickering pdfs when evil-mode is enabled
  (setq-hook! 'pdf-view-mode-hook evil-normal-state-cursor (list nil))

  ;; Persist current page for PDFs opened in Emacs
  (defvar +pdf--page-restored-p nil)
  (add-hook! 'pdf-view-change-page-hook
    (defun +pdf-remember-page-number-h ()
      (when-let (page (and buffer-file-name (pdf-view-current-page)))
        (doom-store-put buffer-file-name page nil "pdf-view"))))
  (add-hook! 'pdf-view-mode-hook
    (defun +pdf-restore-page-number-h ()
      (when-let (page (and buffer-file-name (doom-store-get buffer-file-name "pdf-view")))
        (and (not +pdf--page-restored-p)
             (<= page (or (pdf-cache-number-of-pages) 1))
             (pdf-view-goto-page page)
             (setq-local +pdf--page-restored-p t)))))

  ;; Install epdfinfo binary if needed, blocking until it is finished
  (when doom-interactive-p
    (require 'pdf-tools)
    (unless (file-executable-p pdf-info-epdfinfo-program)
      (let ((wconf (current-window-configuration)))
        (pdf-tools-install)
        (message "Building epdfinfo, this will take a moment...")
        ;; HACK We reset all `pdf-view-mode' buffers to fundamental mode so that
        ;;      `pdf-tools-install' has a chance to reinitialize them as
        ;;      `pdf-view-mode' buffers. This is necessary because
        ;;      `pdf-tools-install' won't do this to buffers that are already in
        ;;      pdf-view-mode.
        (dolist (buffer (doom-buffers-in-mode 'pdf-view-mode))
          (with-current-buffer buffer (fundamental-mode)))
        (while compilation-in-progress
          ;; Block until `pdf-tools-install' is done
          (redisplay)
          (sleep-for 1))
        ;; HACK If pdf-tools was loaded by you opening a pdf file, once
        ;;      `pdf-tools-install' completes, `pdf-view-mode' will throw an error
        ;;      because the compilation buffer is focused, not the pdf buffer.
        ;;      Therefore, it is imperative that the window config is restored.
        (when (file-executable-p pdf-info-epdfinfo-program)
          (set-window-configuration wconf))))

    ;; Sets up `pdf-tools-enable-minor-modes', `pdf-occur-global-minor-mode' and
    ;; `pdf-virtual-global-minor-mode'.
    (pdf-tools-install-noverify))

  ;; Add retina support for MacOS users
  (eval-when! IS-MAC
    (defvar +pdf--scaled-p nil)

    (defadvice! +pdf--scale-up-on-retina-display-a (orig-fn &rest args)
      "Scale up the PDF on retina displays."
      :around #'pdf-util-frame-scale-factor
      (cond ((not pdf-view-use-scaling) 1)
            ((and (memq (pdf-view-image-type) '(imagemagick image-io))
                  (fboundp 'frame-monitor-attributes))
             (funcall orig-fn))
            ;; Add special support for retina displays on MacOS
            ((and (eq (framep-on-display) 'ns)
                  (not +pdf--scaled-p)
                  EMACS27+)
             (setq-local +pdf--scaled-p t)
             2)
            (1)))

    (defadvice! +pdf--use-scaling-on-ns-a ()
      :before-until #'pdf-view-use-scaling-p
      (and (eq (framep-on-display) 'ns)
           EMACS27+
           pdf-view-use-scaling))

    (defadvice! +pdf--supply-width-to-create-image-calls-a (orig-fn &rest args)
      :around '(pdf-annot-show-annotation
                pdf-isearch-hl-matches
                pdf-view-display-region)
      (letf! (defun create-image (file-or-data &optional type data-p &rest props)
               (apply create-image file-or-data type data-p
                      :width (car (pdf-view-image-size))
                      props))
        (apply orig-fn args)))))
