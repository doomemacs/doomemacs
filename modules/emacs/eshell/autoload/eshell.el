;;; emacs/eshell/autoload/eshell.el -*- lexical-binding: t; -*-

(defvar eshell-buffer-name "*doom eshell*")

(defvar +eshell-buffers (make-ring 25)
  "List of open eshell buffers.")


(defvar +eshell--last-buffer nil)


;;
;; Helpers

(defun +eshell--add-buffer (buf)
  (ring-remove+insert+extend +eshell-buffers buf 'grow))

(defun +eshell--remove-buffer (buf)
  (when-let* ((idx (ring-member +eshell-buffers buf)))
    (ring-remove +eshell-buffers idx)
    t))

(defun +eshell--bury-buffer (&optional dedicated-p)
  (unless (switch-to-prev-buffer nil 'bury)
    (switch-to-buffer (doom-fallback-buffer)))
  (when (eq major-mode 'eshell-mode)
    (switch-to-buffer (doom-fallback-buffer)))
  (when +eshell-enable-new-shell-on-split
    (when-let* ((win (get-buffer-window (+eshell/open t))))
      (set-window-dedicated-p win dedicated-p))))

(defun +eshell--setup-window (window &optional flag)
  (when (window-live-p window)
    (set-window-parameter window 'no-other-window flag)
    (set-window-parameter window 'visible flag)))

(defun +eshell--unused-buffer (&optional new-p)
  (or (unless new-p
        (cl-loop for buf in (+eshell-buffers)
                 if (and (buffer-live-p buf)
                         (not (get-buffer-window buf t)))
                 return buf))
      (generate-new-buffer eshell-buffer-name)))

;;;###autoload
(defun +eshell-last-buffer (&optional noerror)
  "Return the last opened eshell buffer."
  (let ((buffer (cl-find-if #'buffer-live-p (+eshell-buffers))))
    (cond (buffer)
          (noerror nil)
          ((user-error "No live eshell buffers remaining")))))

;;;###autoload
(defun +eshell-buffers ()
  "TODO"
  (ring-elements +eshell-buffers))

;;;###autoload
(defun +eshell-run-command (command &optional buffer)
  "TODO"
  (let ((buffer
         (or buffer
             (if (eq major-mode 'eshell-mode)
                 (current-buffer)
               (cl-find-if #'buffer-live-p (+eshell-buffers))))))
    (unless buffer
      (user-error "No living eshell buffers available"))
    (unless (buffer-live-p buffer)
      (user-error "Cannot operate on a dead buffer"))
    (with-current-buffer buffer
      (goto-char eshell-last-output-end)
      (goto-char (line-end-position))
      (insert command)
      (eshell-send-input nil t))))


;;
;; Commands

;;;###autoload
(defun +eshell/open (arg &optional command)
  "Open eshell in the current buffer."
  (interactive "P")
  (when (eq major-mode 'eshell-mode)
    (user-error "Already in an eshell buffer"))
  (let* ((default-directory (or (if arg default-directory (doom-project-root))
                                default-directory))
         (buf (+eshell--unused-buffer)))
    (with-current-buffer (switch-to-buffer buf)
      (if (eq major-mode 'eshell-mode)
          (run-hooks 'eshell-mode-hook)
        (eshell-mode))
      (if command (+eshell-run-command command buf)))
    buf))

;;;###autoload
(defun +eshell/open-popup (arg &optional command)
  "Open eshell in a popup window."
  (interactive "P")
  (let* ((default-directory (or (if arg default-directory (doom-project-root))
                                default-directory))
         (buf (+eshell--unused-buffer)))
    (with-current-buffer (pop-to-buffer buf)
      (if (eq major-mode 'eshell-mode)
          (run-hooks 'eshell-mode-hook)
        (eshell-mode))
      (if command (+eshell-run-command command buf)))
    buf))

;;;###autoload
(defun +eshell/open-fullscreen (arg &optional command)
  "Open eshell in a separate workspace. Requires the (:feature workspaces)
module to be loaded."
  (interactive "P")
  (let ((default-directory (or (if arg default-directory (doom-project-root))
                               default-directory))
        (buf (+eshell--unused-buffer 'new)))
    (set-frame-parameter nil 'saved-wconf (current-window-configuration))
    (delete-other-windows)
    (with-current-buffer (switch-to-buffer buf)
      (eshell-mode)
      (if command (+eshell-run-command command buf)))
    buf))


;;
;; Keybinds

;;;###autoload
(defun +eshell/search-history ()
  "Search the eshell command history with helm, ivy or `eshell-list-history'."
  (interactive)
  (cond ((featurep! :completion ivy)
         (require 'em-hist)
         (let* ((ivy-completion-beg (eshell-bol))
                (ivy-completion-end (point-at-eol))
                (input (buffer-substring-no-properties
                        ivy-completion-beg
                        ivy-completion-end)))
           ;; Better than `counsel-esh-history' because that doesn't
           ;; pre-populate the initial input or selection.
           (ivy-read "Command: "
                     (delete-dups
                      (when (> (ring-size eshell-history-ring) 0)
                        (ring-elements eshell-history-ring)))
                     :initial-input input
                     :action #'ivy-completion-in-region-action)))
        ((featurep! :completion helm)
         (helm-eshell-history))
        ((eshell-list-history))))

;;;###autoload
(defun +eshell/pcomplete ()
  "Use pcomplete with completion-in-region backend instead of popup window at
bottom. This ties pcomplete into ivy or helm, if they are enabled."
  (interactive)
  (require 'pcomplete)
  (ignore-errors (pcomplete-std-complete)))

;;;###autoload
(defun +eshell/quit-or-delete-char (arg)
  "Delete a character (ahead of the cursor) or quit eshell if there's nothing to
delete."
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp nil))
      (eshell-life-is-too-much)
    (delete-char arg)))

;;;###autoload
(defun +eshell/split-below ()
  "Create a new eshell window below the current one."
  (interactive)
  (let ((ignore-window-parameters t)
        (dedicated-p (window-dedicated-p))
        (+eshell-enable-new-shell-on-split
         (or +eshell-enable-new-shell-on-split (frame-parameter nil 'saved-wconf))))
    (select-window (split-window-vertically))
    (+eshell--bury-buffer dedicated-p)))

;;;###autoload
(defun +eshell/split-right ()
  "Create a new eshell window to the right of the current one."
  (interactive)
  (let* ((ignore-window-parameters t)
         (dedicated-p (window-dedicated-p))
         (+eshell-enable-new-shell-on-split
          (or +eshell-enable-new-shell-on-split (frame-parameter nil 'saved-wconf))))
    (select-window (split-window-horizontally))
    (+eshell--bury-buffer dedicated-p)))

;;;###autoload
(defun +eshell/switch-to-next ()
  "Switch to the next eshell buffer."
  (interactive)
  (when (ring-empty-p +eshell-buffers)
    (user-error "No eshell buffers are available"))
  (switch-to-buffer (ring-next +eshell-buffers (current-buffer))))

;;;###autoload
(defun +eshell/switch-to-previous ()
  "Switch to the previous eshell buffer."
  (interactive)
  (when (ring-empty-p +eshell-buffers)
    (user-error "No eshell buffers are available"))
  (switch-to-buffer (ring-previous +eshell-buffers (current-buffer))))

;;;###autoload
(defun +eshell/switch-to-last ()
  "Switch to the last eshell buffer that was open (and is still alive)."
  (interactive)
  (unless (buffer-live-p +eshell--last-buffer)
    (setq +eshell--last-buffer nil)
    (user-error "No last eshell buffer to jump to"))
  (switch-to-buffer +eshell--last-buffer))

;;;###autoload
(defun +eshell/switch-to (buffer)
  "Interactively switch to another eshell buffer."
  (interactive
   (let ((buffers (doom-buffers-in-mode
                   'eshell-mode (delq (current-buffer) (+eshell-buffers)))))
     (if (not buffers)
         (user-error "No eshell buffers are available")
       (list
        (completing-read "Eshell buffers"
                         (mapcar #'buffer-name buffers)
                         #'get-buffer
                         'require-match
                         nil nil
                         (when (eq major-mode 'eshell-mode)
                           (buffer-name (current-buffer))))))))
  (if-let* ((window (get-buffer-window buffer)))
      (select-window window)
    (switch-to-buffer buffer)))

;;;###autoload
(defun +eshell/kill-and-close ()
  "Kill the current eshell buffer and close its window."
  (interactive)
  (unless (eq major-mode 'eshell-mode)
    (user-error "Not in an eshell buffer"))
  (let ((+eshell-kill-window-on-exit t))
    (kill-this-buffer)))


;;
;; Hooks

;;;###autoload
(defun +eshell|init ()
  "Initialize and track this eshell buffer in `+eshell-buffers'."
  (let ((current-buffer (current-buffer)))
    (dolist (buf (+eshell-buffers))
      (unless (buffer-live-p buf)
        (+eshell--remove-buffer buf)))
    (+eshell--setup-window (get-buffer-window current-buffer))
    (+eshell--add-buffer current-buffer)
    (setq +eshell--last-buffer current-buffer)))

;;;###autoload
(defun +eshell|cleanup ()
  "Close window (or workspace) on quit."
  (let ((buf (current-buffer)))
    (when (+eshell--remove-buffer buf)
      (when-let* ((win (get-buffer-window buf)))
        (+eshell--setup-window win nil)
        (cond ((and (one-window-p t)
                    (window-configuration-p (frame-parameter nil 'saved-wconf)))
               (set-window-configuration (frame-parameter nil 'saved-wconf))
               (set-frame-parameter win 'saved-wconf nil))
              ((one-window-p)
               (let ((prev (save-window-excursion (previous-buffer))))
                 (unless (and prev (doom-real-buffer-p prev))
                   (switch-to-buffer (doom-fallback-buffer)))))
              ((or (window-dedicated-p win)
                   +eshell-kill-window-on-exit)
               (let ((ignore-window-parameters t)
                     (popup-p (window-dedicated-p win)))
                 (delete-window win)
                 (when popup-p
                   (cl-loop for win in (window-list)
                            for buf = (window-buffer win)
                            for mode = (buffer-local-value 'major-mode buf)
                            if (eq mode 'eshell-mode)
                            return (select-window win))))))))))

;;;###autoload
(defun +eshell|switch-workspace (type)
  (when (eq type 'frame)
    (setq +eshell-buffers
          (or (persp-parameter 'eshell-buffers)
              (make-ring 25)))))

;;;###autoload
(defun +eshell|save-workspace (_workspace target)
  (when (framep target)
    (set-persp-parameter 'eshell-buffers +eshell-buffers)))
