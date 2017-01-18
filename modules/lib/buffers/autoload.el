;;; autoload.el

(defvar-local +buffer--narrowed-origin nil)

(defvar +buffers-unreal-alist
  '("^ ?\\*.+" image-mode dired-mode reb-mode messages-buffer-mode
    tabulated-list-mode comint-mode magit-mode)
  "A list of regexps or modes whose buffers are considered unreal, and will be
ignored when using `doom:next-real-buffer' and `doom:previous-real-buffer' (or
killed by `doom:kill-old-buffers', or after `+buffer-kill').")

(defvar +buffers-processes-alist
  '(("pry" . ruby-mode)
    ("irb" . ruby-mode)
    ("ipython" . python-mode)
    ("tide" . typescript-mode))
  "An alist of (process-name . major-mode) that `+buffers-kill-process-buffers'
checks before killing processes. If there are no buffers with matching
major-modes, the process gets killed.")

;;;###autoload (autoload '+buffer:narrow "lib/buffer/autoload" nil t)
(evil-define-operator +buffer:narrow (&optional beg end bang)
  "Restrict editing in this buffer to the current region, indirectly. With BANG,
clone the buffer and hard-narrow the selection. If mark isn't active, then widen
the buffer (if narrowed).

Inspired from http://demonastery.org/2013/04/emacs-evil-narrow-region/"
  (interactive "<r><!>")
  (if (region-active-p)
      (progn
        (deactivate-mark)
        (when bang
          (let ((old-buf (current-buffer)))
            (switch-to-buffer (clone-indirect-buffer nil nil))
            (setq +buffer--narrowed-origin old-buf)))
        (narrow-to-region beg end))
    (if +buffer--narrowed-origin
        (progn
          (kill-this-buffer)
          (switch-to-buffer +buffer--narrowed-origin)
          (setq +buffer--narrowed-origin nil))
      (widen))))

;;;###autoload
(defun +buffer-set-readonly-region (begin end)
  "Mark a region as read-only (http://stackoverflow.com/questions/7410125)"
  (let ((modified (buffer-modified-p)))
    (add-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))

;;;###autoload
(defun +buffer-unset-readonly-region (begin end)
  "Undoes `+buffer-set-readonly-region' (http://stackoverflow.com/questions/7410125)"
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (remove-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))


;; Buffer Life and Death ;;;;;;;;;;;;;;;

;;;###autoload
(defun +buffers-list (&optional all-p)
  "Get all buffers in the current project, in the current workgroup.

If ALL-P is non-nil, get all buffers across all projects in current
workgroup."
  (let ((buffers (if (wg-current-workgroup t)
                     (+buffers-in-workgroup)
                   (buffer-list)))
        (project-root (and (not all-p) (doom-project-root t))))
    (append (if project-root
                (funcall (if (eq all-p 'not) '-remove '-filter)
                         (lambda (b) (projectile-project-buffer-p b project-root))
                         buffers)
              buffers)
            (list doom-buffer))))

;;;###autoload
(defun +buffers-list-real (&optional buffer-list)
  "Get a list of all buffers (in the current workgroup OR in BUFFER-LIST) that
`+buffer-real-p' returns non-nil for."
  (-filter #'+buffer-real-p (or buffer-list (+buffers-list))))

;;;###autoload
(defun +buffers-in-workspace ()
  "Get a list of buffers in current workgroup. Returns nil if workgroups2 isn't
loaded."
  (when (featurep 'workgroups2)
    (let ((assoc-bufs (wg-workgroup-associated-buffers nil)))
      (--filter (memq it assoc-bufs) (buffer-list)))))

;;;###autoload
(defun +buffers-in-mode (modes &optional buffer-list)
  "Get a list of all buffers (in the current workgroup OR in BUFFER-LIST) whose
`major-mode' is one of MODES."
  (--filter (memq (buffer-local-value 'major-mode it) modes)
            (or buffer-list (+buffers-list))))

;;;###autoload
(defun +buffers-visible-windows (&optional window-list)
  "Get a list of the visible windows in the current frame (that aren't popups),
OR return only the visible windows in WINDOW-LIST."
  (-remove #'doom-popup-p (or window-list (window-list))))

;;;###autoload
(defun +buffers-visible (&optional buffer-list)
  "Get a list of unburied buffers in the current project and workgroup, OR
return only the unburied buffers in BUFFER-LIST (a list of BUFFER-OR-NAMEs)."
  (-filter #'get-buffer-window (or buffer-list (+buffers-list))))

;;;###autoload
(defun +buffers-buried (&optional buffer-list)
  "Get a list of buried buffers in the current project and workgroup, OR return
only the buried buffers in BUFFER-LIST (a list of BUFFER-OR-NAMEs)."
  (-remove 'get-buffer-window (or buffer-list (+buffers-list))))

;;;###autoload
(defun +buffers-matching (pattern &optional buffer-list)
  "Get a list of all buffers (in the current workgroup OR in BUFFER-LIST) that
match the regex PATTERN."
  (--filter (string-match-p pattern (buffer-name it))
            (or buffer-list (+buffers-list))))

;;;###autoload
(defun +buffer-kill ()
  "Kill buffer then switch to a real buffer. Buries the buffer if it is being
displayed in another window.

See `+buffer-real-p' for what 'real' means."
  (interactive)
  (let* ((scratch-p (doom-scratch-buffer-p))
         (old-project (doom-project-root))
         (buffer (current-buffer))
         (only-buffer-window-p (= (length (get-buffer-window-list buffer nil t)) 1)))
    (unless scratch-p
      (when (and buffer-file-name only-buffer-window-p (buffer-modified-p))
        (if (yes-or-no-p "Buffer is unsaved, save it?")
            (save-buffer)
          (set-buffer-modified-p nil)))
      (+buffer-previous)
      (unless (eq (current-buffer) buffer)
        (when only-buffer-window-p
          (kill-buffer buffer)
          (unless (+buffer-real-p)
            (+buffer-previous))))))
  ;;; TODO Add to `kill-buffer-hook'
  ;; (when (doom-scratch-buffer-p)
  ;;   (doom-scratch-force-reload))
  t)

;;;###autoload
(defun +buffers-kill-process-buffers ()
  "Kill all buried buffers that represent running processes."
  (interactive)
  (let ((buffer-list (buffer-list))
        (killed-processes 0))
    (dolist (p (process-list))
      (let* ((process-name (process-name p))
             (assoc (assoc process-name +buffers-processes-alist)))
        (when (and assoc
                   (not (string= process-name "server"))
                   (process-live-p p)
                   (not (--any? (let ((mode (buffer-local-value 'major-mode it)))
                                  (eq mode (cdr assoc)))
                                buffer-list)))
          (delete-process p)
          (incf killed-processes))))
    (message "Cleaned up %s processes" killed-processes)))

;;;###autoload
(defun +buffers-kill-matching (pattern &optional buffer-list)
  "Kill all buffers (in current workgroup OR in BUFFER-LIST) that match the
regex PATTERN."
  (interactive)
  (let ((i 0))
    (mapc (lambda (b)
            (when (string-match-p pattern (buffer-name b))
              (kill-buffer b)
              (setq i (1+ i))))
          (if buffer-list buffer-list (+buffers-list)))
    (message "Killed %s matches" i)))

(defun +buffers--cycle-real-buffers (&optional n)
  "Switch to the previous buffer, skipping over special buffers. If there's
nothing left, create a scratch buffer."
  (let* ((start-buffer (current-buffer))
         (move-func (if (> n 0) 'switch-to-next-buffer 'switch-to-prev-buffer))
         (max 25)
         (i 0)
         (continue t)
         (buffers (+buffers-list-real (+buffers-list t)))
         (fail-buffer (if (> (length (get-buffer-window-list doom-buffer nil t)) 1)
                          start-buffer
                        doom-buffer))
         destbuf)
    (setq destbuf
          (catch 'goto
            (if (or (not buffers)
                    (= (length buffers) 1))
                (progn (message "No other buffers in workgroup")
                       (throw 'goto fail-buffer))
              (funcall move-func)
              (while (not (memq (current-buffer) buffers))
                (if (or (eq (current-buffer) start-buffer)
                        (>= i max))
                    (throw 'goto fail-buffer)
                  (funcall move-func))
                (cl-incf i))
              (current-buffer))))
    (when (eq destbuf doom-buffer)
      (doom-scratch-reload)
      (message "Nowhere to go"))
    (switch-to-buffer destbuf)))

;;;###autoload
(defun +buffer-real-p (&optional buffer)
  "Returns whether BUFFER a 'real' buffer or not. Real means: a) it isn't a
popup (or temporary) window and b) it isn't a special buffer (e.g. scratch or
*messages* buffer). See `+buffers-unreal-alist'."
  (setq buffer (or (and (bufferp buffer) buffer)
                   (and (stringp buffer) (get-buffer buffer))
                   (current-buffer)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (not (or (apply #'derived-mode-p (-filter 'symbolp +buffers-unreal-alist))
               (--any? (string-match-p it (buffer-name buffer))
                       (-filter 'stringp +buffers-unreal-alist)))))))

;;;###autoload
(defun +buffer-next ()
  "Switch to the next real buffer, skipping special buffers. See
`+buffer-real-p'."
  (interactive)
  (+buffers--cycle-real-buffers +1))

;;;###autoload
(defun +buffer-previous ()
  "Switch to the previous real buffer, skipping special buffers. See
`+buffer-real-p'."
  (interactive)
  (+buffers--cycle-real-buffers -1))

(defun +buffers--kill (buffers &optional filter-func)
  (let ((buffers (if filter-func (-filter filter-func buffers) buffers))
        (affected 0))
    (mapc (lambda (b) (when (kill-buffer b) (incf affected))) buffers)
    (unless (+buffer-real-p)
      (+buffer-previous))
    (message "Killed %s buffers" affected)))

;;;###autoload (autoload '+buffers:kill-all "lib/buffer/autoload" nil t)
(evil-define-command +buffers:kill-all (&optional bang)
  "Kill all buffers in this workgroup. If BANG, kill *all* buffers in workgroup
that belong to this project. Returns to the doom scratch buffer."
  (interactive "<!>")
  (+buffers--kill (--filter (not (eq it doom-buffer))
                                (+buffers-list (not bang))))
  (unless bang
    (delete-other-windows))
  (switch-to-buffer doom-buffer))

;;;###autoload (autoload '+buffers:kill-others "lib/buffer/autoload" nil t)
(evil-define-command +buffers:kill-others (&optional bang)
  "Kill all other buffers in this workgroup. If BANG, kill only the other
buffers that belong to the current project."
  (interactive "<!>")
  (+buffers--kill (+buffers-list (not bang))
                      (lambda (b) (not (eq b (current-buffer)))))
  (when bang
    (delete-other-windows)))

;;;###autoload (autoload '+buffers:kill-matching "lib/buffer/autoload" nil t)
(evil-define-command +buffers:kill-matching (&optional bang pattern)
  "Kill buffers in current workgroup that match regex PATTERN. If BANG, then
exclude buffers that aren't part of the current project."
  :repeat nil
  (interactive "<!><a>")
  (+buffers--kill (+buffers-list-matching pattern (+buffers-list (not bang)))))

;;;###autoload (autoload '+buffers:cleanup "lib/buffer/autoload" nil t)
(evil-define-command +buffers:cleanup (&optional bang)
  "Kill all buried and unreal buffers in the current workgroup."
  (interactive "<!>")
  (let ((kill-list (+buffers-list-buried (+buffers-list t))))
    (+buffers--kill kill-list)
    (+buffers-kill-process-buffers)
    (message "Cleaned up %s buffers" (length kill-list))))

(provide 'autoload)
;;; autoload.el ends here
