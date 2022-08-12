;;; lang/org/autoload/contrib-ipython.el -*- lexical-binding: t; -*-
;;;###if (modulep! +ipython)

;;;###autoload
(defun +org-ob-ipython-initiate-session-a (&optional session params)
  "Create a session named SESSION according to PARAMS."
  (if (string= session "none")
      (error
       "ob-ipython currently only supports evaluation using a session.
Make sure your src block has a :session param.")
    (when (not (string-suffix-p ".json" session t))
      (ob-ipython--create-kernel
       (ob-ipython--normalize-session
        session)
       (cdr (assoc :kernel params))))
    (ob-ipython--create-repl
     (ob-ipython--normalize-session
      session)
     params)))

(defun +org--ob-ipython-generate-local-path-from-remote (session host params)
  "Given a remote SESSION with PARAMS and corresponding HOST, copy remote config to local, start a jupyter console to generate a new one."
  (let* ((runtime-dir
          (cdr
           (doom-call-process "ssh " host "jupyter" "--runtime-dir")))
         (runtime-file (concat runtime-dir "/" "kernel-" session ".json"))
         (tramp-path (concat "/ssh:" host ":" runtime-file))
         (tramp-copy (concat (or +ob-ipython-local-runtime-dir
                                 (cdr (doom-call-process "jupyter" "--runtime-dir")))
                             "/remote-" host "-kernel-" session ".json"))
         (local-path
          (concat
           "Python:ob-ipython-"
           (file-name-sans-extension (file-name-nondirectory tramp-copy))
           "-ssh.json")))
    ;; scp remote file to local
    (copy-file tramp-path tramp-copy t)
    ;; connect to remote use new config
    (let* ((python-shell-interpreter-interactive-arg " console --simple-prompt")
           (python-shell-prompt-detect-enabled nil)
           (python-shell-completion-native-enable nil)
           (buf (python-shell-make-comint
                 (concat ob-ipython-command
                         " console --simple-prompt --existing "
                         tramp-copy " --ssh " host)
                 (concat "" local-path)
                 t))
           (proc (get-buffer-process buf))
           (dir (cdr (assoc :pydir params))))
      (sleep-for 3)
      (when dir
        (with-current-buffer buf
          (setq-local default-directory dir)))
      (format "*%s*" proc))))

;;;###autoload
(defun +org-ob-ipython-create-repl-a (name &optional params)
  "Create repl based on NAME and PARAMS.
If PARAMS specifies remote kernel, copy the kernel config from remote server and
create a repl connecting to remote session."
  (let ((cmd (string-join (ob-ipython--kernel-repl-cmd name) " ")))
    (cond ((string= "default" name)
           (run-python cmd nil nil)
           (format "*%s*" python-shell-buffer-name))
          ((string-match "^remote-.*ssh.json" name)
           (when (not (ignore-errors
                        (process-live-p
                         (get-process
                          (format
                           "Python:ob-ipython-%s"
                           name)))))
             (let* ((remote (s-split "-" name))
                    (remote-host (nth 1 remote))
                    (remote-session (nth 3 remote)))
               (+org--ob-ipython-generate-local-path-from-remote
                remote-session
                remote-host
                params))))
          ((let* ((process-name (format "Python:ob-ipython-%s" name))
                  (python-shell-prompt-detect-enabled nil)
                  (python-shell-completion-native-enable nil)
                  (buf (python-shell-make-comint cmd process-name t))
                  (dir (cdr (assoc :pydir params))))
             (if dir
                 (with-current-buffer buf
                   (setq-local default-directory dir)))
             (sleep-for 1)
             (format "*%s*" process-name))))))

;;;###autoload
(defun +org-babel-execute:ipython-a (body params)
  "Execute a BODY of IPython code with PARAMS in org-babel.
This function is called by `org-babel-execute-src-block'."
  (message default-directory)
  (org-babel-ipython-initiate-session (cdr (assoc :session params))
                                      params))


;;
;; * org-src-edit

;;;###autoload
(defun +org-babel-edit-prep:ipython-a (info)
  (let* ((params (nth 2 info))
         (session (cdr (assoc :session params))))
    (org-babel-ipython-initiate-session session params))
  ;; Support for python.el's "send-code" commands within edit buffers.
  (setq-local python-shell-buffer-name
              (format "Python:ob-ipython-%s"
                      (ob-ipython--normalize-session
                       (cdr (assoc :session (nth 2 info))))))
  (setq-local default-directory
              (format "%s"
                      (ob-ipython--normalize-session
                       (cdr (assoc :pydir (nth 2 info))))))
  (ob-ipython-mode 1)
  ;; hack on company mode to use company-capf rather than company-anaconda
  (when (modulep! :completion company)
    (setq-local company-backends
                '(company-capf
                  company-dabbrev
                  company-files
                  company-yasnippet))
    (setq-local company-idle-delay nil))
  (when (featurep 'lpy)
    (setq lispy-python-proc
          (format "Python:ob-ipython-%s"
                  (ob-ipython--normalize-session
                   (cdr (assoc :session (nth 2 info)))))
          lispy--python-middleware-loaded-p nil)
    (lispy--python-middleware-load)))


;;
;; * retina

(defun +org--ob-ipython-mac-2x-image-file-name (filename &optional scale)
  "Return the name of high-resolution image file for FILENAME.
The optional arg SCALE is scale factor, and defaults to 2."
  (let ((pos (or (string-match "\\.[^./]*\\'" filename) (length filename))))
    (format "%s@%dx%s"
            (substring filename 0 pos)
            (or scale 2)
            (substring filename pos))))

;;;###autoload
(defun +org-ob-ipython-write-base64-string-a (oldfunc &rest args)
  (let ((file (car args))
        (b64-string (cdr args)))
    (let ((file2x (+org--ob-ipython-mac-2x-image-file-name file)))
      (apply oldfunc file2x b64-string)
      (shell-command (concat "convert " file2x " -resize 50% " file)))))
