;;; core/autoload/debug.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom/what-face (&optional pos)
  "Lists all faces at point. Overlay faces are denoted with an asterix."
  (interactive "d")
  (let ((pos (or pos (point)))
        faces)
    (when-let (face (get-text-property pos 'face))
      (dolist (f (if (listp face) face (list face)))
        (push (propertize (symbol-name f) 'face f) faces)))
    (dolist (ov (overlays-at pos (1+ pos)))
      (let ((face (overlay-get ov 'face)))
        (dolist (f (if (listp face) face (list face)))
          (push (propertize (concat (symbol-name f) "*") 'face f) faces))))
    (if (called-interactively-p 'any)
        (message "%s %s"
                 (propertize "Faces:" 'face 'font-lock-comment-face)
                 (if faces (string-join faces ", ") "n/a"))
      (mapcar #'substring-no-properties faces))))

;;;###autoload
(defun doom-active-minor-modes ()
  "Get a list of active minor-mode symbols."
  (cl-loop for mode in minor-mode-list
           unless (and (boundp mode) (symbol-value mode))
           collect mode))

;;;###autoload
(defun doom/what-minor-mode (mode)
  "Get information on an active minor mode. Use `describe-minor-mode' for a
selection of all minor-modes, active or not."
  (interactive
   (list (completing-read "Minor mode: "
                          (doom-active-minor-modes))))
  (describe-minor-mode-from-symbol
   (cl-typecase mode
     (string (intern mode))
     (symbol mode)
     (t (error "Expected a symbol/string, got a %s" (type-of mode))))))

;;;###autoload
(defun doom/am-i-secure ()
  "Test to see if your root certificates are securely configured in emacs."
  (declare (interactive-only t))
  (interactive)
  (if-let (bad-hosts
           (cl-loop for bad
                    in '("https://wrong.host.badssl.com/"
                         "https://self-signed.badssl.com/")
                    if (condition-case _e
                           (url-retrieve bad (lambda (_retrieved) t))
                         (error nil))
                    collect bad))
      (error (format "tls seems to be misconfigured (it got %s)."
                     bad-hosts))
    (url-retrieve "https://badssl.com"
                  (lambda (status)
                    (if (or (not status) (plist-member status :error))
                        (warn "Something went wrong.\n\n%s" (pp-to-string status))
                      (message "Your trust roots are set up properly.\n\n%s" (pp-to-string status))
                      t)))))
