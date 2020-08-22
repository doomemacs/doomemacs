;;; lang/nim/config.el -*- lexical-binding: t; -*-

(use-package! nim-mode
  :defer t
  :init
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
    (defadvice! +nim--suggest-get-temp-file-name-a (path)
      "Removes invalid characters from the temp file path, including the unicode
character that colon is replaced with, which is known to cause issues on
windows."
      :filter-return #'nimsuggest--get-temp-file-name
      (replace-regexp-in-string "[꞉* |<>\"?*]" "" path)))

  :config
  (set-lookup-handlers! '(nim-mode nimsuggest-mode)
    :definition #'+nimsuggest-find-definition
    :documentation #'nimsuggest-show-doc)

  (map! :localleader
        :map nim-mode-map
        "b" #'nim-compile
        "h" #'nimsuggest-show-doc
        "d" #'nimsuggest-find-definition))


(use-package! flycheck-nim
  :when (featurep! :checkers syntax)
  :after nim-mode)

