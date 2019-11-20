;;; config/literate/init.el -*- lexical-binding: t; -*-

(defvar +literate-config-file
  (concat doom-private-dir "config.org")
  "The file path of your literate config file.")

(defvar +literate-config-cache-file
  (concat doom-cache-dir "literate-last-compile")
  "The file path that `+literate-config-file' will be tangled to, then
byte-compiled from.")


;;
(defun +literate-tangle (&optional force-p)
  "Tangles `+literate-config-file' if it has changed."
  (let ((default-directory doom-private-dir))
    (when (or (file-newer-than-file-p +literate-config-file
                                      +literate-config-cache-file)
              force-p)
      (message "Compiling your literate config...")
      (let* ((org  (expand-file-name +literate-config-file))
             (dest (concat (file-name-sans-extension +literate-config-file) ".el"))
             (output (get-buffer-create "*org-tangle*")))
        (unwind-protect
            ;; We tangle in a separate, blank process because loading it here
            ;; would load all of :lang org (very expensive!).
            (or (and (zerop (call-process
                             "emacs" nil output nil
                             "-q" "--batch"
                             "-l" "ob-tangle"
                             "--eval" (format "(org-babel-tangle-file %S %S)"
                                              org dest)))
                     (with-current-buffer output
                       (message "%s" (buffer-string))
                       t)
                     ;; Write the cache file to serve as our mtime cache
                     (with-temp-file +literate-config-cache-file
                       (message "Done!")))
                (warn "There was a problem tangling your literate config!"))
          (kill-buffer output))))))


;; Let 'er rip!
(+literate-tangle (or doom-reloading-p noninteractive))
;; No need to load the resulting file. Doom will do this for us after all
;; modules have finished loading.


;; Recompile our literate config if we modify it
(after! org
  (add-hook 'after-save-hook #'+literate-recompile-maybe-h))
