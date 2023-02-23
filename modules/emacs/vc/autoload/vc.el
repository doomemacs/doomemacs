;;; emacs/vc/autoload/vc.el -*- lexical-binding: t; -*-

;;
;;; Helpers

(defun +vc--remote-homepage ()
  (require 'browse-at-remote)
  (or (let ((url (browse-at-remote--remote-ref)))
        (plist-get (browse-at-remote--get-url-from-remote (car url)) :url))
      (user-error "Can't find homepage for current project")))

;; TODO: PR these upstream?
;;;###autoload
(defun browse-at-remote--format-region-url-as-codeberg (repo-url location filename &optional linestart lineend)
  "URL formatted for codeberg."
  (cond
   ((and linestart lineend)
    (format "%s/src/%s/%s#L%d-L%d" repo-url location filename linestart lineend))
   (linestart (format "%s/src/%s/%s#L%d" repo-url location filename linestart))
   (t (format "%s/src/%s/%s" repo-url location filename))))

;;;###autoload
(defun browse-at-remote--format-commit-url-as-codeberg (repo-url commithash)
  "Commit URL formatted for codeberg"
  (format "%s/src/commit/%s" repo-url commithash))


;;
;;; Commands

(defvar browse-at-remote-prefer-symbolic)
;;;###autoload
(defun +vc/browse-at-remote (&optional arg)
  "Open URL to current file (and line if selection is active) in browser.
If prefix ARG, negate the default value of `browse-at-remote-prefer-symbolic'."
  (interactive "P")
  (require 'browse-at-remote)
  (let ((vc-ignore-dir-regexp locate-dominating-stop-dir-regexp)
        (browse-at-remote-prefer-symbolic
         (if arg
             (not browse-at-remote-prefer-symbolic)
           browse-at-remote-prefer-symbolic)))
    (browse-at-remote)))

;;;###autoload
(defun +vc/browse-at-remote-kill (&optional arg interactive?)
  "Copy URL to current file (and line if selection is active) to clipboard.
If prefix ARG, negate the default value of `browse-at-remote-prefer-symbolic'."
  (interactive (list current-prefix-arg 'interactive))
  (require 'browse-at-remote)
  (let ((vc-ignore-dir-regexp locate-dominating-stop-dir-regexp)
        (browse-at-remote-prefer-symbolic
         (if arg
             (not browse-at-remote-prefer-symbolic)
           browse-at-remote-prefer-symbolic)))
    (browse-at-remote-kill)
    (if interactive? (message "Copied to clipboard"))))

;;;###autoload
(defun +vc/browse-at-remote-homepage ()
  "Open homepage for current project in browser."
  (interactive)
  (browse-url (+vc--remote-homepage)))

;;;###autoload
(defun +vc/browse-at-remote-kill-homepage ()
  "Copy homepage URL of current project to clipboard."
  (interactive)
  (let ((url (+vc--remote-homepage)))
    (kill-new url)
    (message "Copied to clipboard: %S" url)))
