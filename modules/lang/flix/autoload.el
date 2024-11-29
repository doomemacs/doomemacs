;;; lang/flix/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +flix/install-jar ()
  "Install flix.jar in a selected directory if it is not already present."
  (interactive)
  (let* ((default-directory (or (projectile-project-root) default-directory))
         (selected-dir (read-directory-name "Install into directory: " default-directory))
         (jar-path (concat (file-name-as-directory selected-dir) "flix.jar"))
         (download-url "https://github.com/flix/flix/releases/latest/download/flix.jar"))
    (if (file-exists-p jar-path)
        (message "flix.jar is already installed.")
      (url-copy-file download-url jar-path t)
      (message "flix.jar has been downloaded and installed.")
      (setq flix-jar-directory selected-dir))))

;;;###autoload
(defun +flix/flix-command ()
  "Run a command with the flix.jar in the specified directory."
  (interactive)
  (let* ((default-directory (or (projectile-project-root) flix-jar-directory default-directory))
         (selected-dir (read-directory-name "init project in: " default-directory))
         (command (read-string "Command: "))
         (jar-path (concat (file-name-as-directory selected-dir) "flix.jar")))
    (if (file-exists-p jar-path)
        (let ((default-directory selected-dir))
          (message (concat "Running flix " command "..."))
          (setq flix-jar-directory selected-dir)
          (async-shell-command (concat "java -jar flix.jar " command)))
      (message "flix.jar not found in the selected directory."))))

;;;###autoload
(defun +flix/init-project ()
  "Initialize a flix project in the selected directory if flix.jar is present."
  (interactive)
  (let* ((default-directory (or (projectile-project-root) flix-jar-directory default-directory))
         (selected-dir (read-directory-name "init project in: " default-directory))
         (jar-path (concat (file-name-as-directory selected-dir) "flix.jar")))
    (if (file-exists-p jar-path)
        (let ((default-directory selected-dir))
          (message "initializing flix project...")
          (setq flix-jar-directory selected-dir)
          (shell-command "java -jar flix.jar init"))
      (message "flix.jar not found in the selected directory."))))

;;;###autoload
(defun +flix/run-project ()
  "Initialize a Flix project in the selected directory if flix.jar is present."
  (interactive)
  (let* ((default-directory (or (projectile-project-root) flix-jar-directory default-directory))
         (selected-dir (read-directory-name "Flix exe: " default-directory))
         (jar-path (concat (file-name-as-directory selected-dir) "flix.jar")))
    (if (file-exists-p jar-path)
        (let ((default-directory selected-dir))
          (message "Running Flix project...")
          (setq flix-jar-directory selected-dir)
          (async-shell-command "java -jar flix.jar run"))
      (message "flix.jar not found in the selected directory."))))

;;;###autoload
(defun +flix/build-project ()
  "Initialize a Flix project in the selected directory if flix.jar is present."
  (interactive)
  (let* ((default-directory (or (projectile-project-root) flix-jar-directory default-directory))
         (selected-dir (read-directory-name "Flix exe: " default-directory))
         (jar-path (concat (file-name-as-directory selected-dir) "flix.jar")))
    (if (file-exists-p jar-path)
        (let ((default-directory selected-dir))
          (message "Building Flix project...")
          (setq flix-jar-directory selected-dir)
          (async-shell-command "java -jar flix.jar build"))
      (message "flix.jar not found in the selected directory."))))

;;;###autoload
(defun +flix/repl ()
  "Run an inferior instance of `flix` inside Emacs."
  (interactive)
  (setq flix-last-buffer (current-buffer))
  (+flix--repl-start-repl))

;;;###autoload
(defun +flix/goto-flix-buffer ()
  "Switch to active Flix REPL"
  (interactive)
  (if (buffer-live-p (get-buffer flix-last-buffer))
      (switch-to-buffer-other-window flix-last-buffer)
    (message "Flix buffer deleted")))

;;;###autoload
(defun +flix/goto-repl ()
  "Switch to active Flix REPL"
  (interactive)
  (setq flix-last-buffer (current-buffer))
  (if (get-buffer "*Flix REPL*")
      (switch-to-buffer-other-window "*Flix REPL*")
    (+flix/repl)))

;;;###autoload
(defun +flix/repl-restart ()
  "Restart the Flix REPL process."
  (interactive)
  (+flix--repl-stop-repl)
  (+flix--repl-start-repl))

;;;###autoload
(defun +flix/set-jar-directory ()
  "Set the directory where flix.jar is located."
  (interactive)
  (let* ((default-directory (or (projectile-project-root) flix-jar-directory default-directory))
         (selected-dir (read-directory-name "Flix exe: " default-directory)))
    (setq flix-jar-directory selected-dir)))

;;;###autoload
(defun +flix/send-line-to-repl ()
  "Send the current line to the Flix REPL."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (comint-send-string (get-buffer-process "*Flix REPL*") line)
    (comint-send-string (get-buffer-process "*Flix REPL*") "\n")))
