;;; lang/plantuml/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +plantuml-org-babel-execute:plantuml-a (body params)
  "Execute a block of plantuml code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (require 'plantuml-mode)
  (let* ((body (replace-regexp-in-string
                "^[[:blank:]\n]*\\(@start\\)"
                "\\\\\\1"
                body))
         (out-file (or (cdr (assoc :file params))
                       (concat doom-cache-dir
                               "ob-plantuml/"
                               (md5 str nil nil nil t)
                               ".png")))
         (in-file (org-babel-temp-file "plantuml-")))
    (if (eq plantuml-default-exec-mode 'server)
        (let* ((url-request-location ))
          (with-current-buffer
              (url-retrieve-synchronously (plantuml-server-encode-url body))
            (goto-char (point-min))
            ;; skip the HTTP headers
            (while (not (looking-at "\n")) (forward-line))
            (kill-region (point-min) (+ 1 (point)))
            (write-file (org-babel-process-file-name out-file))))
      (let* ((cmd (concat (cond ((eq plantuml-default-exec-mode 'executable)
                                 (unless (executable-find plantuml-executable-path)
                                   (error "Could not find plantuml at %s"
                                          (executable-find plantuml-executable-path)))
                                 (concat (shell-quote-argument (executable-find plantuml-executable-path))
                                         " --headless "))
                                ((not (file-exists-p org-plantuml-jar-path))
                                 (error "Could not find plantuml.jar at %s" org-plantuml-jar-path))
                                ((concat "java " (or (cdr (assoc :java params)) "") " -jar "
                                         (shell-quote-argument (expand-file-name plantuml-executable-path)))))
                          (concat (if (string= (file-name-extension out-file) "svg")
                                      " -tsvg" "")
                                  (if (string= (file-name-extension out-file) "eps")
                                      " -teps" "")
                                  " -p " (cdr (assoc :cmdline params)) " < "
                                  (org-babel-process-file-name in-file)
                                  " > "
                                  (org-babel-process-file-name out-file)))))
        (with-temp-file in-file
          (insert (concat "@startuml\n" body "\n@enduml")))
        (message "%s" cmd)
        (org-babel-eval cmd "")
        nil)))) ;; signal that output has already been written to file
