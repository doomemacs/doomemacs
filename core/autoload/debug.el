;;; core/autoload/debug.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom/what-face (&optional pos)
  "Shows all faces and overlay faces at point.

Interactively prints the list to the echo area. Noninteractively, returns a list
whose car is the list of faces and cadr is the list of overlay faces."
  (interactive)
  (unless pos
    (setq pos (point)))
  (let ((faces (let ((face (get-text-property pos 'face)))
                 (if (keywordp (car-safe face))
                     (list face)
                   (cl-loop for f in (if (listp face) face (list face))
                            collect f))))
        (overlays (cl-loop for ov in (overlays-at pos (1+ pos))
                           nconc (cl-loop with face = (overlay-get ov 'face)
                                          for f in (if (listp face) face (list face))
                                          collect f))))

    (cond ((called-interactively-p 'any)
           (message "%s %s\n%s %s"
                    (propertize "Faces:" 'face 'font-lock-comment-face)
                    (if faces
                        (cl-loop for face in faces
                                 if (listp face)
                                   concat (format "'%s " face)
                                 else
                                   concat (concat (propertize (symbol-name face) 'face face) " "))
                      "n/a ")
                    (propertize "Overlays:" 'face 'font-lock-comment-face)
                    (if overlays
                        (cl-loop for ov in overlays
                                 concat (concat (propertize (symbol-name ov) 'face ov) " "))
                      "n/a")))
          (t
           (and (or faces overlays)
                (list faces overlays))))))

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

(defvar doom--profiler nil)
;;;###autoload
(defun doom/toggle-profiler ()
  (interactive)
  (if (not doom--profiler)
      (profiler-start 'cpu+mem)
    (profiler-report)
    (profiler-stop))
  (setq doom--profiler (not doom--profiler)))
