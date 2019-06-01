;;; ui/tabbar/config.el -*- lexical-binding: t; -*-

;; This is here for reference. It is incomplete, buggy, and may be removed one
;; day. It shows window-local buffer lists.

(defvar +tabbar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap delete-window] #'+tabbar/close-tab-or-window)
    (define-key map [remap +workspace/close-window-or-workspace] #'+tabbar/close-tab-or-window)
    map)
  "TODO")

(after! persp-mode
  (define-key persp-mode-map [remap delete-window] #'+tabbar/close-tab-or-window)
  (define-key persp-mode-map [remap +workspace/close-window-or-workspace] #'+tabbar/close-tab-or-window))

(define-minor-mode +tabbar-mode
  "TODO"
  :global t
  :keymap +tabbar-mode-map
  (if +tabbar-mode
      (setq-default header-line-format '((:eval (+tabbar-line))))
    (setq-default header-line-format nil)))
(+tabbar-mode +1)


;;
(add-hook! 'doom-load-theme-hook
  (defconst +tabbar-bar (make-xpm (face-background 'highlight) 3 26))
  (defconst +tabbar-hidden-bar (make-xpm nil 1 26))
  (setq-default header-line-format `(,+tabbar-hidden-bar (:eval (+tabbar-line))))

  (add-to-list 'default-frame-alist '(buffer-predicate . +tabbar-buffer-predicate))
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'buffer-predicate #'+tabbar-buffer-predicate)))

(defun +tabbar-buffer-predicate (buffer)
  (or (memq buffer (window-parameter nil 'tabbar-buffers))
      (eq buffer (doom-fallback-buffer))))


(defun +tabbar-line ()
  (unless (or (window-dedicated-p)
              (null mode-line-format))
    (concat (cl-loop for buf in (window-parameter nil 'tabbar-buffers)
                     if (eq (get-buffer buf) (current-buffer))
                     concat (propertize (format "%s  %s  " +tabbar-bar buf)
                                        'face 'default)
                     else
                     concat (propertize (format "  %s  " buf) 'face 'mode-line-inactive))
            +tabbar-hidden-bar)))

(defun +tabbar*bury-buffer (orig-fn &rest args)
  (let ((b (current-buffer)))
    (apply orig-fn args)
    (unless (eq b (current-buffer))
      (with-current-buffer b
        (+tabbar|remove-buffer)))))
(advice-add #'bury-buffer :around #'+tabbar*bury-buffer)

(defun +tabbar*kill-current-buffer (&rest _)
  (+tabbar|remove-buffer))
(advice-add #'kill-current-buffer :before #'+tabbar*kill-current-buffer)

(defun +tabbar|remove-buffer ()
  (set-window-parameter
   nil
   'tabbar-buffers (delete (current-buffer) (window-parameter nil 'tabbar-buffers))))

(defun +tabbar/close-tab-or-window ()
  (interactive)
  (call-interactively
   (cond ((cdr (window-parameter nil 'tabbar-buffers))
          #'kill-current-buffer)
         ((fboundp '+workspace/close-window-or-workspace)
          #'+workspace/close-window-or-workspace)
         (#'delete-window))))

(defun +tabbar|add-buffer ()
  (when (doom-real-buffer-p (current-buffer))
    (let* ((this-buf (current-buffer))
           (buffers (window-parameter nil 'tabbar-buffers)))
      (unless (memq this-buf buffers)
        (setq buffers (append buffers (list this-buf))))
      (add-hook 'kill-buffer-hook #'+tabbar|remove-buffer nil t)
      (set-window-parameter nil 'tabbar-buffers buffers))))
(add-hook 'doom-switch-buffer-hook #'+tabbar|add-buffer)

(defun +tabbar|new-window ()
  (unless (window-parameter nil 'tabbar-buffers)
    (+tabbar|add-buffer)))
(add-hook 'doom-switch-window-hook #'+tabbar|new-window)
