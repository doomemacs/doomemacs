;;; completion/corfu/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +corfu/move-to-minibuffer ()
  "Move the current list of candidates to your choice of minibuffer completion UI."
  (interactive)
  (pcase completion-in-region--data
    (`(,beg ,end ,table ,pred ,extras)
     (let ((completion-extra-properties extras)
           completion-cycle-threshold completion-cycling)
       (cond ((and (modulep! :completion vertico)
                   (fboundp #'consult-completion-in-region))
              (consult-completion-in-region beg end table pred))
             ((and (modulep! :completion ivy)
                   (fboundp #'ivy-completion-in-region))
              (ivy-completion-in-region (marker-position beg) (marker-position end) table pred))
             ;; Important: `completion-in-region-function' is set to corfu at
             ;; this moment, so `completion-in-region' (single -) doesn't work
             ;; below.
             ((modulep! :completion helm)
              ;; Helm is special and wants to _wrap_ `completion--in-region'
              ;; instead of replacing it in `completion-in-region-function'.
              ;; But because the advice is too unreliable we "fake" the wrapping.
              (helm--completion-in-region #'completion--in-region beg end table pred))
             ((modulep! :completion ido)
              (completion--in-region beg end table pred))
             (t (error "No minibuffer completion UI available for moving to!")))))))

;;;###autoload
(defun +corfu/smart-sep-toggle-escape ()
  "Insert `corfu-separator' or toggle escape if it's already there."
  (interactive)
  (cond ((and (char-equal (char-before) corfu-separator)
              (char-equal (char-before (1- (point))) ?\\))
         (save-excursion (delete-char -2)))
        ((char-equal (char-before) corfu-separator)
         (save-excursion (backward-char 1)
                         (insert-char ?\\)))
        (t (call-interactively #'corfu-insert-separator))))

;;;###autoload
(defun +corfu/dabbrev-this-buffer ()
  "Like `cape-dabbrev', but only scans current buffer."
  (interactive)
  (require 'cape)
  (let ((cape-dabbrev-check-other-buffers nil))
    (cape-dabbrev t)))

;;; end of autoload.el
