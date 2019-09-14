;;; lang/nim/config.el -*- lexical-binding: t; -*-

(after! nim-mode
  (add-hook! 'nim-mode-hook
    (defun +nim-init-nimsuggest-mode-h ()
      "Conditionally load `nimsuggest-mode', instead of clumsily erroring out if
nimsuggest isn't installed."
      (unless (stringp nimsuggest-path)
        (setq nimsuggest-path (executable-find "nimsuggest")))
      (when (and nimsuggest-path (file-executable-p nimsuggest-path))
        (nimsuggest-mode))))

  (when IS-WINDOWS
    ;; TODO File PR/report upstream (https://github.com/nim-lang/nim-mode)
    (defadvice! +nim--suggest-get-dirty-dir-a ()
      "The original `nimsuggest--get-dirty-dir' incorrectly extracts the frame
number from the string representation of `selected-frame', which can contain
characters that are illegal on Windows, causing invalid argument errors when
`nimsuggest--make-tempdir' tries to use it."
      :override #'nimsuggest--get-dirty-dir
      (let* ((frame-str (format "%s" (selected-frame)))
             (frame-num-str (if (string-match " \\(0x[0-9a-z]+\\)>$" frame-str)
                                (match-string 1 frame-str))))
        (file-name-as-directory (concat nimsuggest-dirty-directory frame-num-str))))

    ;; TODO File PR/report upstream (https://github.com/nim-lang/nim-mode)
    (defadvice! +nim--suggest-get-temp-file-name-a (path)
      "Removes invalid characters from the temp file path, including the unicode
character that colon is replaced with, which is known to cause issues on
windows."
      :filter-return #'nimsuggest--get-temp-file-name
      (replace-regexp-in-string "[꞉* |<>\"?*]" "" path)))

  (map! :localleader
        :map nim-mode-map
        "b" #'nim-compile))


(use-package! flycheck-nim
  :when (featurep! :tools flycheck)
  :after nim-mode)

