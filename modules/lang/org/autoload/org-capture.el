;;; lang/org/autoload/org-capture.el -*- lexical-binding: t; -*-

(defvar org-capture-initial)


;;
;;; External frame

(defvar +org-capture-fn #'org-capture
  "Command to use to initiate org-capture.")

;;;###autoload
(defvar +org-capture-frame-parameters
  `((name . "doom-capture")
    (width . 70)
    (height . 25)
    (transient . t)
    ,@(when IS-LINUX
        `((window-system . ,(if (boundp 'pgtk-initialized) 'pgtk 'x))
          (display . ,(or (getenv "DISPLAY") ":0"))))
    ,(if IS-MAC '(menu-bar-lines . 1)))
  "TODO")

;;;###autoload
(defun +org-capture-cleanup-frame-h ()
  "Closes the org-capture frame once done adding an entry."
  (when (and (+org-capture-frame-p)
             (not org-capture-is-refiling))
    (delete-frame nil t)))

;;;###autoload
(defun +org-capture-frame-p (&rest _)
  "Return t if the current frame is an org-capture frame opened by
`+org-capture/open-frame'."
  (and (equal (alist-get 'name +org-capture-frame-parameters)
              (frame-parameter nil 'name))
       (frame-parameter nil 'transient)))

;;;###autoload
(defun +org-capture/open-frame (&optional initial-input key)
  "Opens the org-capture window in a floating frame that cleans itself up once
you're done. This can be called from an external shell script."
  (interactive)
  (when (and initial-input (string-empty-p initial-input))
    (setq initial-input nil))
  (when (and key (string-empty-p key))
    (setq key nil))
  (let* ((frame-title-format "")
         (frame (if (+org-capture-frame-p)
                    (selected-frame)
                  (make-frame +org-capture-frame-parameters))))
    (select-frame-set-input-focus frame)  ; fix MacOS not focusing new frames
    (with-selected-frame frame
      (require 'org-capture)
      (condition-case ex
          (letf! ((#'pop-to-buffer #'switch-to-buffer))
            (switch-to-buffer (doom-fallback-buffer))
            (let ((org-capture-initial initial-input)
                  org-capture-entry)
              (when (and key (not (string-empty-p key)))
                (setq org-capture-entry (org-capture-select-template key)))
              (funcall +org-capture-fn)))
        ('error
         (message "org-capture: %s" (error-message-string ex))
         (delete-frame frame))))))

;;;###autoload
(defun +org-capture-available-keys ()
  "TODO"
  (string-join (mapcar #'car org-capture-templates) ""))


;;
;;; Capture targets

;;;###autoload
(defun +org-capture-todo-file ()
  "Expand `+org-capture-todo-file' from `org-directory'.
If it is an absolute path return `+org-capture-todo-file' verbatim."
  (expand-file-name +org-capture-todo-file org-directory))

;;;###autoload
(defun +org-capture-notes-file ()
  "Expand `+org-capture-notes-file' from `org-directory'.
If it is an absolute path return `+org-capture-todo-file' verbatim."
  (expand-file-name +org-capture-notes-file org-directory))

(defun +org--capture-local-root (path)
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
  (+org--capture-local-root +org-capture-todo-file))

;;;###autoload
(defun +org-capture-project-notes-file ()
  "Find the nearest `+org-capture-notes-file' in a parent directory, otherwise,
opens a blank one at the project root. Throws an error if not in a project."
  (+org--capture-local-root +org-capture-notes-file))

;;;###autoload
(defun +org-capture-project-changelog-file ()
  "Find the nearest `+org-capture-changelog-file' in a parent directory,
otherwise, opens a blank one at the project root. Throws an error if not in a
project."
  (+org--capture-local-root +org-capture-changelog-file))

(defun +org--capture-ensure-heading (headings &optional initial-level)
  (if (not headings)
      (widen)
    (let ((initial-level (or initial-level 1)))
      (if (and (re-search-forward (format org-complex-heading-regexp-format
                                          (regexp-quote (car headings)))
                                  nil t)
               (= (org-current-level) initial-level))
          (progn
            (beginning-of-line)
            (org-narrow-to-subtree))
        (goto-char (point-max))
        (unless (and (bolp) (eolp)) (insert "\n"))
        (insert (make-string initial-level ?*)
                " " (car headings) "\n")
        (beginning-of-line 0))
      (+org--capture-ensure-heading (cdr headings) (1+ initial-level)))))

(defun +org--capture-central-file (file project)
  (let ((file (expand-file-name file org-directory)))
    (set-buffer (org-capture-target-buffer file))
    (org-capture-put-target-region-and-position)
    (widen)
    (goto-char (point-min))
    ;; Find or create the project headling
    (+org--capture-ensure-heading
     (append (org-capture-get :parents)
             (list project (org-capture-get :heading))))))

;;;###autoload
(defun +org-capture-central-project-todo-file ()
  "TODO"
  (+org--capture-central-file
   +org-capture-projects-file (projectile-project-name)))

;;;###autoload
(defun +org-capture-central-project-notes-file ()
  "TODO"
  (+org--capture-central-file
   +org-capture-projects-file (projectile-project-name)))

;;;###autoload
(defun +org-capture-central-project-changelog-file ()
  "TODO"
  (+org--capture-central-file
   +org-capture-projects-file (projectile-project-name)))
