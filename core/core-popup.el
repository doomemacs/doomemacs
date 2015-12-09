;;; core-popup.el --- hacks for better popwin integration

(use-package popwin
  :init
  :config
  (setq popwin:popup-window-height 0.3)
  (mapc (lambda (rule) (push rule popwin:special-display-config))
        '(("*Help*"                :position bottom :height 0.25 :stick t)
          (debugger-mode           :position bottom :height 15)
          (org-src-mode            :position bottom :height 0.5 :stick t)
          (org-agenda-mode         :position bottom :height 0.4 :stick t)
          ("*evil-registers*"      :position bottom :height 0.3 :stick t)
          ("*scratch*"             :position bottom :height 20 :stick t)
          ("*Apropos*"             :position bottom :height 40 :stick t)
          ("*Backtrace*"           :position bottom :height 15 :stick t)
          ("^\\*CPU-Profiler-Report .+\\*$"  :regexp t :position bottom :height 0.35)
          ("*Flycheck errors*"     :position bottom :height 15 :stick t)
          ("*quickrun*"            :position bottom :height 15 :stick t)
          ("*minor-modes*"         :position bottom :height 0.5 :stick t)

          ;; vcs
          ("\\*git-gutter.+\\*" :regexp t :position bottom :height 30 :stick t)

          ;; Helm
          ("^\\*[Hh]elm.*?\\*\\'"  :regexp t :position bottom :height 0.2)
          ("*helm-mode-find-file-at-point*"  :position bottom :height 10)
          ("*helm-mode-ffap*"      :position bottom :height 10)

          ;; Org
          ("^\\*Org-Babel.*\\*$"   :regexp t :position bottom :height 15 :tail t)
          ("*Agenda Commands*"     :position bottom :height 0.5)
          (" *Org todo*"            :position bottom :height 5)
          ("*Org Links*"           :position bottom :height 2)
          ("*ruby*" :position bottom :height 0.3 :stick t)
          ("*ielm*" :position bottom :height 0.3 :stick t)
          ))

  (popwin-mode 1)

  (after! evil
    ;; Fix disruptive errors w/ hidden buffers caused by popwin
    (defadvice evil-ex-hl-do-update-highlight (around evil-ex-hidden-buffer-ignore-errors activate)
      (ignore-errors ad-do-it)))

  (after! neotree
    (when neo-persist-show
      (add-hook! 'popwin:before-popup-hook (setq neo-persist-show nil))
      (add-hook! 'popwin:after-popup-hook  (setq neo-persist-show t))))

  (after! quickrun
    (defun narf*quickrun-close-popwin (&optional _ _ _ _)
      (when (get-buffer quickrun/buffer-name)
        (quickrun/kill-quickrun-buffer)
        (popwin:close-popup-window-if-necessary)))

    (defun quickrun/pop-to-buffer (buf cb)
      (popwin:pop-to-buffer buf)
      (with-current-buffer buf
        (evil-resize-window 5)
        (funcall cb)
        (setq mode-line-format nil)))

    (defun narf/quickrun-after-run ()
      (with-selected-window popwin:popup-window
        (let* ((lines (count-lines (point-min) (point-max)))
               (act-lines (max 5 (min 40 (count-lines (point-min) (point-max))))))
          (evil-resize-window act-lines)
          (goto-char (point-min)))))
    (add-hook! quickrun-after-run 'narf/quickrun-after-run)

    (advice-add 'quickrun :before 'narf*quickrun-close-popwin)
    (advice-add 'quickrun-region :before 'narf*quickrun-close-popwin))

  (after! helm
    (defun narf/helm-split-window (&optional window)
      "Minimalistic split-fn; leaves popwin to handle helm buffers."
      popwin:popup-window)

    (setq-default
     helm-split-window-preferred-function 'narf/helm-split-window
     helm-display-function 'popwin:pop-to-buffer)

    (defadvice helm-ag--edit-abort (around helm-ag-edit-abort-popwin-compat activate)
      (cl-letf (((symbol-function 'select-window) 'ignore)) ad-do-it))
    (defadvice helm-ag--edit-commit (around helm-ag-edit-commit-popwin-compat activate)
      (cl-letf (((symbol-function 'select-window) 'ignore)) ad-do-it))

    ;; I remove any attempt to kill the helm-ag window, because popwin handles it.
    (defun helm-ag--edit (_candidate)
      (let ((default-directory helm-ag--default-directory))
        (with-current-buffer (get-buffer-create "*helm-ag-edit*")
          (erase-buffer)
          (setq-local helm-ag--default-directory helm-ag--default-directory)
          (let (buf-content)
            (with-current-buffer (get-buffer "*helm-ag*")
              (goto-char (point-min))
              (forward-line 1)
              (let* ((body-start (point))
                     (marked-lines (cl-loop for ov in (overlays-in body-start (point-max))
                                            when (eq 'helm-visible-mark (overlay-get ov 'face))
                                            return (helm-marked-candidates))))
                (if (not marked-lines)
                    (setq buf-content (buffer-substring-no-properties
                                       body-start (point-max)))
                  (setq buf-content (concat (mapconcat 'identity marked-lines "\n") "\n")))))
            (insert buf-content)
            (add-text-properties (point-min) (point-max)
                                 '(read-only t rear-nonsticky t front-sticky t))
            (let ((inhibit-read-only t))
              (setq header-line-format
                    (format "[%s] C-c C-c: Commit, C-c C-k: Abort"
                            (abbreviate-file-name helm-ag--default-directory)))
              (goto-char (point-min))
              (while (re-search-forward "^\\(\\(?:[^:]+:\\)\\{1,2\\}\\)\\(.*\\)$" nil t)
                (let ((file-line-begin (match-beginning 1))
                      (file-line-end (match-end 1))
                      (body-begin (match-beginning 2))
                      (body-end (match-end 2)))
                  (add-text-properties file-line-begin file-line-end
                                       '(face font-lock-function-name-face
                                              intangible t))
                  (remove-text-properties body-begin body-end '(read-only t))
                  (set-text-properties body-end (1+ body-end)
                                       '(read-only t rear-nonsticky t))))))))
      (popwin:display-buffer (get-buffer "*helm-ag-edit*"))
      ;; (other-window 1)
      ;; (switch-to-buffer (get-buffer "*helm-ag-edit*"))
      (goto-char (point-min))
      (setq next-error-function 'compilation-next-error-function)
      (setq-local compilation-locs (make-hash-table :test 'equal :weakness 'value))
      (use-local-map helm-ag-edit-map)))

  (add-hook! org-load
    ;; (defun org-src-switch-to-buffer (buffer context)
    ;;   (popwin:popup-buffer (get-buffer buffer) :height 0.5))

    (defun org-switch-to-buffer-other-window (&rest args)
      (mapc (lambda (b)
              (let ((buf (if (stringp b) (get-buffer-create b) b)))
                (popwin:pop-to-buffer buf t t)))
            args)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun narf/popup-p (&optional buffer)
    (and (popwin:popup-window-live-p)
         (if buffer (eq buffer popwin:popup-buffer) t)))

  (defun narf/popup-close ()
    (when (popwin:popup-window-live-p)
      (popwin:close-popup-window t)))

  (defun narf/popup-open (buffer &optional height)
    (popwin:popup-buffer buffer))

  (defun narf/popup-toggle ()
    (interactive)
    (if (popwin:popup-window-live-p)
        (popwin:close-popup-window)
      (popwin:popup-last-buffer)))

  (defun narf:popup-last-buffer ()
    (interactive)
    (popwin:popup-last-buffer))

  (defun narf:popup-messages ()
    (interactive)
    (popwin:messages)))

(provide 'core-popup)
;;; core-popup.el ends here
