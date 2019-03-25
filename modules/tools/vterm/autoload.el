;;; tools/vterm/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +vterm/open (arg)
  "Open a terminal buffer in the current window. If ARG (universal argument) is
non-nil, cd into the current project's root."
  (interactive "P")
  (unless (fboundp 'module-load)
    (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
  (let ((default-directory
          (if arg
              (or (doom-project-root) default-directory)
            default-directory)))
    (switch-to-buffer (save-window-excursion (vterm)))))

;;;###autoload
(defun +vterm/open-popup (arg)
  "Open a terminal popup window. If ARG (universal argument) is
non-nil, cd into the current project's root."
  (interactive "P")
  (unless (fboundp 'module-load)
    (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
  (let ((default-directory
          (if arg
              (or (doom-project-root) default-directory)
            default-directory)))
    (pop-to-buffer (save-window-excursion (vterm)))))

;;;###autoload
(defun +vterm/open-popup-in-project ()
  "Open a terminal popup window in the root of the current project."
  (interactive)
  (+vterm/open-popup t))
