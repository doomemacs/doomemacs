;;; lang/org/autoload/org-capture.el -*- lexical-binding: t; -*-
;;;###if (featurep! +capture)

(defvar org-capture-initial)

;;
;; External frame

;;;###autoload
(defvar +org-capture-frame-parameters
  `((name . "org-capture")
    (width . 70)
    (height . 25)
    (transient . t)
    ,(if IS-LINUX '(display . ":0")))
  "TODO")

;;;###autoload
(defun +org-capture|cleanup-frame ()
  "Closes the org-capture frame once done adding an entry."
  (when (+org-capture-frame-p)
    (delete-frame nil t)))

;;;###autoload
(defun +org-capture-frame-p (&rest _)
  "Return t if the current frame is an org-capture frame opened by
`+org-capture/open-frame'."
  (and (equal "org-capture" (frame-parameter nil 'name))
       (frame-parameter nil 'transient)))

;;;###autoload
(defun +org-capture/open-frame (&optional string key)
  "Opens the org-capture window in a floating frame that cleans itself up once
you're done. This can be called from an external shell script."
  (interactive)
  (when (and string (string-empty-p string))
    (setq string nil))
  (when (and key (string-empty-p key))
    (setq key nil))
  (let* ((frame-title-format "")
         (frame (if (+org-capture-frame-p)
                    (selected-frame)
                  (let (before-make-frame-hook after-make-frame-functions)
                    (make-frame +org-capture-frame-parameters)))))
    (with-selected-frame frame
      (require 'org-capture)
      (condition-case ex
          (cl-letf (((symbol-function #'pop-to-buffer)
                     (symbol-function #'switch-to-buffer)))
            (switch-to-buffer (doom-fallback-buffer))
            (let ((org-capture-initial string)
                  org-capture-entry)
              (when (and key (not (string-empty-p key)))
                (setq org-capture-entry (org-capture-select-template key)))
              (if (or org-capture-entry
                      (not (fboundp 'counsel-org-capture)))
                  (org-capture)
                (unwind-protect
                    (counsel-org-capture)
                  (if-let* ((buf (cl-loop for buf in (buffer-list)
                                          if (buffer-local-value 'org-capture-mode buf)
                                          return buf)))
                      (with-current-buffer buf
                        (add-hook 'kill-buffer-hook #'+org-capture|cleanup-frame nil t))
                    (delete-frame frame))))))
        ('error
         (message "org-capture: %s" (error-message-string ex))
         (delete-frame frame))))))

;;;###autoload
(defun +org-capture-available-keys ()
  "TODO"
  (string-join (mapcar #'car org-capture-templates) ""))


;;
;; Capture targets

(defun +org--capture-root (path)
  (let ((filename (file-name-nondirectory path)))
    (expand-file-name
     filename
     (or (locate-dominating-file (file-truename default-directory)
                                 filename)
         (doom-project-root)
         (user-error "Couldn't detect a project")))))

;;;###autoload
(defun +org-capture-project-todo-file ()
  "Find the nearest `+org-capture-todo-file' in a parent directory, otherwise,
opens a blank one at the project root. Throws an error if not in a project."
  (+org--capture-root +org-capture-todo-file))

;;;###autoload
(defun +org-capture-project-notes-file ()
  "Find the nearest `+org-capture-notes-file' in a parent directory, otherwise,
opens a blank one at the project root. Throws an error if not in a project."
  (+org--capture-root +org-capture-notes-file))

;;;###autoload
(defun +org-capture-project-changelog-file ()
  "Find the nearest `+org-capture-changelog-file' in a parent directory,
otherwise, opens a blank one at the project root. Throws an error if not in a
project."
  (+org--capture-root +org-capture-changelog-file))
