;;; tools/pdf/config.el -*- lexical-binding: t; -*-

(use-package! pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
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
  ;; HACK `pdf-tools-install-noverify' tries to "reset" open pdf-view-mode
  ;;      buffers, but does so incorrectly, causing errors when pdf-tools is
  ;;      loaded after opening a pdf file. We've done its job ourselves in
  ;;      `+pdf--install-epdfinfo-a' instead.
  (defadvice! +pdf--inhibit-pdf-view-mode-resets-a (orig-fn &rest args)
    :around #'pdf-tools-install-noverify
    (letf! ((#'pdf-tools-pdf-buffer-p #'ignore))
      (apply orig-fn args)))

  (defadvice! +pdf--install-epdfinfo-a (orig-fn &rest args)
    "Install epdfinfo after the first PDF file, if needed."
    :around #'pdf-view-mode
    ;; Prevent "epdfinfo not an executable" error short-circuiting this advice
    (prog1 (with-demoted-errors "%s" (apply orig-fn args))
      ;; ...so we can go ahead and install it afterwards.
      (cond ((file-executable-p pdf-info-epdfinfo-program))
            ((y-or-n-p "To read PDFs in Emacs the epdfinfo program must be built. Build it now?")
             (message nil) ; flush lingering prompt in echo-area
             ;; Make sure this doesn't run more than once
             (advice-remove #'pdf-view-mode #'+pdf--install-epdfinfo-a)
             (unless (or (pdf-info-running-p)
                         (ignore-errors (pdf-info-check-epdfinfo) t))
               ;; HACK On the first pdf you open (before pdf-tools loaded)
               ;;      `pdf-tools-install' throws errors because it has hardcoded
               ;;      opinions about what buffer should be focused when it is run.
               ;;      These errors cause `compile' to position the compilation
               ;;      window incorrectly or can interfere with the opening of the
               ;;      original pdf--sometimes aborting/burying it altogether. A
               ;;      timer works around this.
               (run-at-time
                0.1 nil
                (lambda ()
                  (with-current-buffer (pdf-tools-install t)
                    (add-hook! 'compilation-finish-functions :local
                      (dolist (buf (buffer-list))
                        (with-current-buffer buf
                          (and (buffer-file-name)
                               (or (pdf-tools-pdf-buffer-p)
                                   (derived-mode-p 'pdf-view-mode))
                               (revert-buffer t t))))))))))
            ((message "Aborted")))))

  (pdf-tools-install-noverify)

  ;; For consistency with other special modes
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

  ;; HACK Refresh FG/BG for pdfs when `pdf-view-midnight-colors' is changed by a
  ;;      theme or with `setq!'.
  ;; TODO PR this upstream?
  (defun +pdf-reload-midnight-minor-mode-h ()
    (when pdf-view-midnight-minor-mode
      (pdf-info-setoptions
       :render/foreground (car pdf-view-midnight-colors)
       :render/background (cdr pdf-view-midnight-colors)
       :render/usecolors t)
      (pdf-cache-clear-images)
      (pdf-view-redisplay t)))
  (put 'pdf-view-midnight-colors 'custom-set
       (lambda (sym value)
         (set-default sym value)
         (dolist (buffer (doom-buffers-in-mode 'pdf-view-mode))
           (with-current-buffer buffer
             (if (get-buffer-window buffer)
                 (+pdf-reload-midnight-minor-mode-h)
               ;; Defer refresh for buffers that aren't visible, to avoid
               ;; blocking Emacs for too long while changing themes.
               (add-hook 'doom-switch-buffer-hook #'+pdf-reload-midnight-minor-mode-h
                         nil 'local))))))

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


(use-package! saveplace-pdf-view
  :after pdf-view)
