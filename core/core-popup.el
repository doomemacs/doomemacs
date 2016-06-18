;;; core-popup.el --- taming stray windows

;; I use a slew of hackery to get Emacs to treat 'pop-ups' consistently. It goes
;; through great lengths to tame helm, flycheck, help buffers--*even* the beast
;; that is org-mode.
;;
;; Be warned, an update could break any of this.

(use-package shackle
  :config
  (shackle-mode 1)
  (setq shackle-rules
        `(;; Util
          ("^\\*.+-Profiler-Report .+\\*$" :align below :size 0.3 :regexp t)
          ("*esup*"            :align below :size 0.4 :noselect t)
          ("*minor-modes*"     :align below :size 0.5 :noselect t)
          ("*eval*"            :align below :size 16  :noselect t)
          ;; Doom
          (" *doom*"           :align below :size 35  :select t)
          ("^\\*doom:.+\\*$"   :align below :size 35  :select t :regexp t)
          ("^\\*doom.+\\*$"    :align below :size 12  :noselect t :regexp t)
          ;; Emacs
          ("*Pp Eval Output*"  :align below :size 0.3)
          ("*Apropos*"         :align below :size 0.3)
          ("*Backtrace*"       :align below :size 25  :noselect t)
          ("*Completions*"     :align below :size 20  :noselect t)
          ("*Help*"            :align below :size 16  :select t)
          ("*Messages*"        :align below :size 15  :select t)
          ("*Warnings*"        :align below :size 10  :noselect t)
          (compilation-mode    :align below :size 15  :noselect t)
          (eww-mode            :align below :size 30  :select t)
          ("*command-log*"     :align right :size 28  :noselect t)
          ;; vcs
          ("*vc-diff*"         :align below :size 15  :noselect t)
          ("*vc-change-log*"   :align below :size 15  :select t)
          (vc-annotate-mode    :same t)))

  ;; :noesc    = Can't be closed with a single ESC
  ;; :nokill   = Won't be killed when closed
  ;; :modeline = Show the modeline
  (defvar doom-popup-rules
    '(("^\\*doom\\(:scratch\\)?\\*$" :noesc :nokill :modeline)
      ("^\\*doom.*\\*$"       :noesc :nokill)
      (ivy-occur-grep-mode    :noesc)
      (compilation-mode       :noesc)
      (comint-mode            :noesc :nokill)
      (eshell-mode            :noesc :nokill)
      (messages-buffer-mode          :nokill)
      (esup-mode              :noesc)
      (tabulated-list-mode    :noesc)))

  ;; There is no shackle-popup hook, so I hacked one in
  (advice-add 'shackle-display-buffer :around 'doom*popup-init)
  ;; Don't mess with popups
  (advice-add 'balance-windows :around 'doom*save-popup)
  (advice-add 'doom/evil-window-move :around 'doom*save-popup))


;;
;; Hacks
;;

(after! help-mode
  ;; Following links in help buffers sometimes uses itself or other-window
  ;; (annoying!). It should only replace the buffer we opened the popup from. To
  ;; fix this these three button types need to be redefined to stow away the
  ;; popups then follow the link from the last buffer, using `doom/popup-save'.
  (define-button-type 'help-function-def
    :supertype 'help-xref
    'help-function (lambda (fun file)
                     (require 'find-func)
                     (when (eq file 'C-source)
                       (setq file (help-C-file-name (indirect-function fun) 'fun)))
                     (let ((location (find-function-search-for-symbol fun nil file)))
                       (doom/popup-save
                        (switch-to-buffer (car location) nil t)
                        (if (cdr location)
                            (goto-char (cdr location))
                          (message "Unable to find location in file")))
                       (select-window (get-buffer-window (car location))))))

  (define-button-type 'help-variable-def
    :supertype 'help-xref
    'help-function (lambda (var &optional file)
                     (when (eq file 'C-source)
                       (setq file (help-C-file-name var 'var)))
                     (let ((location (find-variable-noselect var file)))
                       (doom/popup-save
                        (switch-to-buffer (car location) nil t)
                        (if (cdr location)
                            (goto-char (cdr location))
                          (message "Unable to find location in file")))
                       (select-window (get-buffer-window (car location))))))

  (define-button-type 'help-face-def
    :supertype 'help-xref
    'help-function (lambda (fun file)
                     (require 'find-func)
                     (let ((location
                            (find-function-search-for-symbol fun 'defface file)))
                       (doom/popup-save
                        (switch-to-buffer (car location) nil t)
                        (if (cdr location)
                            (goto-char (cdr location))
                          (message "Unable to find location in file")))
                       (select-window (get-buffer-window (car location)))))))

(provide 'core-popup)
;;; core-popup.el ends here
