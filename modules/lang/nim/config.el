;;; lang/nim/config.el -*- lexical-binding: t; -*-

(after! nim-mode
  (defun +nim|init-nimsuggest-mode ()
    "Conditionally load `nimsuggest-mode', instead of clumsily erroring out if
nimsuggest isn't installed."
    (unless (stringp nimsuggest-path)
      (setq nimsuggest-path (executable-find "nimsuggest")))
    (when (and nimsuggest-path (file-executable-p nimsuggest-path))
      (nimsuggest-mode)))
  (add-hook 'nim-mode-hook #'+nim|init-nimsuggest-mode)

  (when IS-WINDOWS
    ;; TODO File PR/report upstream (https://github.com/nim-lang/nim-mode)
    (defun doom*nimsuggest--get-dirty-dir ()
      "The original `nimsuggest--get-dirty-dir' incorrectly extracts the frame
number from the string representation of `selected-frame', which can contain
characters that are illegal on Windows, causing invalid argument errors when
`nimsuggest--make-tempdir' tries to use it."
      (let* ((frame-str (format "%s" (selected-frame)))
             (frame-num-str (if (string-match " \\(0x[0-9a-z]+\\)>$" frame-str)
                                (match-string 1 frame-str))))
        (file-name-as-directory (concat nimsuggest-dirty-directory frame-num-str))))
    (advice-add #'nimsuggest--get-dirty-dir :override #'doom*nimsuggest--get-dirty-dir)
    
    ;; TODO File PR/report upstream (https://github.com/nim-lang/nim-mode)
    (defun doom*nimsuggest--get-temp-file-name (path)
      "Removes invalid characters from the temp file path, including the unicode
character that colon is replaced with, which is known to cause issues on
windows."
      (replace-regexp-in-string "[꞉* |<>\"?*]" "" path))
    (advice-add #'nimsuggest--get-temp-file-name :filter-return #'doom*nimsuggest--get-temp-file-name))

  (map! :localleader
        :map nim-mode-map
        "b" #'nim-compile))


(def-package! flycheck-nim
  :when (featurep! :feature syntax-checker)
  :after nim-mode)

