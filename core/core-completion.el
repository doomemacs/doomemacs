;;; core-completion.el --- auto-completion, for the lazy typist

(package! company
  :commands (company-mode company-complete company-complete-common company-manual-begin)
  :init
  (setq company-idle-delay nil
        company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode)
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-backends '(company-capf company-yasnippet)
        company-quickhelp-delay nil
        company-statistics-file (concat doom-temp-dir "company-stats-cache.el"))

  ;; vim-like omni-complete
  (map! :i "C-SPC" 'doom/company-complete
        (:prefix "C-x"
          :i "C-l"   'doom/company-whole-lines
          :i "C-k"   'doom/company-dict-or-keywords
          :i "C-f"   'company-files
          :i "C-]"   'company-tags
          :i "s"     'company-ispell
          :i "C-s"   'company-yasnippet
          :i "C-o"   'company-capf
          :i "C-n"   'company-dabbrev-code
          :i "C-p"   (λ! (let ((company-selection-wrap-around t))
                           (call-interactively 'company-dabbrev-code)
                           (company-select-previous-or-abort)))))

  :config
  (require 'company-capf)
  (require 'company-yasnippet)
  (push 'company-sort-by-occurrence company-transformers)
  ;; Don't interfere with insert mode binding for `evil-delete-backward-word'
  (define-key company-active-map "\C-w" nil)

  (map! (:map company-active-map
          "C-o"        'company-search-kill-others
          "C-n"        'company-select-next
          "C-p"        'company-select-previous
          "C-h"        'company-quickhelp-manual-begin
          "C-S-h"      'company-show-doc-buffer
          "C-S-s"      'company-search-candidates
          "C-s"        'company-filter-candidates
          "C-SPC"      'company-complete-common
          [tab]        'company-complete-common-or-cycle
          [backtab]    'company-select-previous
          [escape]     (λ! (company-abort) (evil-normal-state 1))
          [C-return]   'counsel-company)

        (:map company-search-map
          "C-n"        'company-search-repeat-forward
          "C-p"        'company-search-repeat-backward
          [escape]     'company-search-abort))

  (global-company-mode +1))

(package! company-dict
  :commands company-dict
  :config (setq company-dict-dir (concat doom-private-dir "dict")))

;; NOTE: Doesn't look pretty on OSX without emacs-mac
(package! company-quickhelp
  :after company
  :config (company-quickhelp-mode +1))

(package! company-statistics
  :after company
  :config (company-statistics-mode +1))

;;
(autoload 'company-dabbrev "company-dabbrev" nil t)
(autoload 'company-dabbrev-code "company-dabbrev-code" nil t)
(autoload 'company-etags "company-etags" nil t)
(autoload 'company-elisp "company-elisp" nil t)
(autoload 'company-files "company-files" nil t)
(autoload 'company-gtags "company-gtags" nil t)
(autoload 'company-ispell "company-ispell" nil t)


;;
;; Defuns
;;

(defun doom/company-complete ()
  "Bring up the completion popup. If only one result, complete it."
  (interactive)
  (require 'company)
  (when (and (company-manual-begin)
             (= company-candidates-length 1))
    (company-complete-common)))

(defun doom/company-whole-lines (command &optional arg &rest ignored)
  "`company-mode' completion backend that completes whole-lines, akin to vim's
C-x C-l."
  (interactive (list 'interactive))
  (require 'company)
  (unless (bound-and-true-p company-mode) (company-mode))
  (let ((lines (split-string
                (replace-regexp-in-string
                 "^[\t\s]+" ""
                 (concat (buffer-substring-no-properties (point-min) (line-beginning-position))
                         (buffer-substring-no-properties (line-end-position) (point-max))))
                "\\(\r\n\\|[\n\r]\\)" t)))
    (cl-case command
      (interactive (company-begin-backend 'doom/company-whole-lines))
      (prefix (company-grab-line "^[\t\s]*\\(.+\\)" 1))
      (candidates (all-completions arg lines)))))

(defun doom/company-dict-or-keywords ()
  "`company-mode' completion combining `company-dict' and `company-keywords'."
  (interactive)
  (require 'company-dict)
  (require 'company-keywords)
  (let ((company-backends '((company-keywords company-dict))))
    (call-interactively 'company-complete)))

(provide 'core-completion)
;;; core-completion.el ends here
