;;; completion/corfu/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +corfu-dabbrev-friend-buffer-p (other-buffer)
  (< (buffer-size other-buffer) +corfu-buffer-scanning-size-limit))


;;
;;; Commands

;;;###autoload
(defun +corfu/move-to-minibuffer ()
  "Move list of candidates to your choice of minibuffer completion UI."
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

;;;###autoload
(defun +corfu/toggle-auto-complete (&optional interactive)
  "Toggle as-you-type completion in Corfu."
  (interactive (list 'interactive))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when corfu-mode
        (if corfu-auto
            (remove-hook 'post-command-hook #'corfu--auto-post-command 'local)
          (add-hook 'post-command-hook #'corfu--auto-post-command nil 'local)))))
  (when interactive
    (message "Corfu auto-complete %s" (if corfu-auto "disabled" "enabled")))
  (setq corfu-auto (not corfu-auto)))

;;;###autoload
(defun +corfu/dabbrev-or-next (&optional arg)
  "Trigger corfu popup and select the first candidate.

Intended to mimic `evil-complete-next', unless the popup is already open."
  (interactive "p")
  (if corfu--candidates
      (corfu-next arg)
    (require 'cape)
    (let ((cape-dabbrev-check-other-buffers
           (bound-and-true-p evil-complete-all-buffers)))
      (cape-dabbrev t)
      (when (> corfu--total 0)
        (corfu--goto (or arg 0))))))

;;;###autoload
(defun +corfu/dabbrev-or-last (&optional arg)
  "Trigger corfu popup and select the first candidate.

Intended to mimic `evil-complete-previous', unless the popup is already open."
  (interactive "p")
  (if corfu--candidates
      (corfu-previous arg)
    (require 'cape)
    (let ((cape-dabbrev-check-other-buffers
           (bound-and-true-p evil-complete-all-buffers)))
      (cape-dabbrev t)
      (when (> corfu--total 0)
        (corfu--goto (- corfu--total (or arg 1)))))))

;;; end of autoload.el
