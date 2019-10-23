;;; term/eshell/autoload/company.el -*- lexical-binding: t; -*-
;;;###if (featurep! :completion company)

;; REVIEW Refactor me

(defvar company-pcomplete-available 'unknown)

(defun company-pcomplete--prefix ()
  (let* ((pcomplete-stub)
         pcomplete-seen
         pcomplete-norm-func
         pcomplete-args
         pcomplete-last pcomplete-index
         (pcomplete-autolist pcomplete-autolist)
         (pcomplete-suffix-list pcomplete-suffix-list))
    (pcomplete-completions)
    (buffer-substring (pcomplete-begin) (point))))

(defun company-pcomplete--candidates ()
  (let* ((pcomplete-stub)
         (pcomplete-show-list t)
         pcomplete-seen pcomplete-norm-func
         pcomplete-args pcomplete-last pcomplete-index
         (pcomplete-autolist pcomplete-autolist)
         (pcomplete-suffix-list pcomplete-suffix-list)
         (candidates (pcomplete-completions))
         (prefix (buffer-substring (pcomplete-begin) (point)))
         ;; Collect all possible completions for the current stub
         (cnds (all-completions pcomplete-stub candidates))
         (bnds (completion-boundaries pcomplete-stub candidates nil ""))
         (skip (- (length pcomplete-stub) (car bnds))))
    ;; Replace the stub at the beginning of each candidate by the prefix
    (mapcar (lambda (cand)
              (concat prefix (substring cand skip)))
            cnds)))

;;;###autoload
(defun company-pcomplete-available ()
  (when (eq company-pcomplete-available 'unknown)
    (condition-case _err
        (progn
          (company-pcomplete--candidates)
          (setq company-pcomplete-available t))
      (error
       (message "Company: pcomplete not found")
       (setq company-pcomplete-available nil))))
  company-pcomplete-available)

;;;###autoload
(defun company-pcomplete (command &optional _arg &rest ignored)
  "`company-mode' completion backend using `pcomplete'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-pcomplete))
    (prefix (when (company-pcomplete-available)
              (company-pcomplete--prefix)))
    (candidates (company-pcomplete--candidates))
    (sorted t)))
