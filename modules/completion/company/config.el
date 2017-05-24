;;; completion/company/config.el

(def-setting! :company-backend (modes backends)
  "Register company BACKENDS to MODES."
  (let* ((modes (if (listp modes) modes (list modes)))
         (backends (if (listp backends) backends (list backends)))
         (def-name (intern (format "doom--init-company-%s"
                                   (mapconcat #'identity (mapcar #'symbol-name modes) "-"))))
         (quoted (eq (car-safe backends) 'quote)))
    ;; TODO more type checks
    `(prog1
         (defun ,def-name ()
           (when (memq major-mode ',modes)
             (require 'company)
             (unless (member ',backends company-backends)
               (setq-local company-backends (append '((,@backends)) company-backends)))))
       (add-hook! ,modes #',def-name))))


;;
;; Packages
;;

(def-package! company
  :demand t
  :commands (company-mode global-company-mode company-complete
             company-complete-common company-manual-begin company-grab-line)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 3
        company-tooltip-limit 10
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode)
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-backends '(company-capf))

  (push #'company-sort-by-occurrence company-transformers)

  (after! yasnippet
    (nconc company-backends '(company-yasnippet)))

  (map! (:map company-active-map
          ;; Don't interfere with `evil-delete-backward-word' in insert mode
          "C-w"        nil

          "C-o"        #'company-search-kill-others
          "C-n"        #'company-select-next
          "C-p"        #'company-select-previous
          "C-h"        #'company-quickhelp-manual-begin
          "C-S-h"      #'company-show-doc-buffer
          "C-S-s"      #'company-search-candidates
          "C-s"        #'company-filter-candidates
          "C-SPC"      #'company-complete-common
          "C-h"        #'company-quickhelp-manual-begin
          [tab]        #'company-complete-common-or-cycle
          [backtab]    #'company-select-previous
          [escape]     (λ! (company-abort) (evil-normal-state 1)))

        ;; Automatically applies to `company-filter-map'
        (:map company-search-map
          "C-n"        #'company-search-repeat-forward
          "C-p"        #'company-search-repeat-backward
          "C-s"        (λ! (company-search-abort) (company-filter-candidates))
          [escape]     #'company-search-abort))

  ;; TAB auto-completion in term buffers
  (after! comint
    (map! :map comint-mode-map [tab] #'company-complete))

  (global-company-mode +1))


(def-package! company-statistics
  :after company
  :config
  (setq company-statistics-file (concat doom-cache-dir "company-stats-cache.el"))
  (quiet! (company-statistics-mode +1)))


;; Looks ugly on OSX without emacs-mac build
(def-package! company-quickhelp
  :after company
  :config
  (setq company-quickhelp-delay nil)
  (company-quickhelp-mode +1))


(def-package! company-dict
  :commands company-dict
  :config
  ;; Project-specific dictionaries
  (defun +company|enable-project-dicts (mode &rest _)
    (if (symbol-value mode)
        (push mode company-dict-minor-mode-list)
      (setq company-dict-minor-mode-list (delq mode company-dict-minor-mode-list))))
  (add-hook 'doom-project-hook #'+company|enable-project-dicts))


;;
;; Autoloads
;;

(autoload 'company-capf "company-capf")
(autoload 'company-yasnippet "company-yasnippet")
(autoload 'company-dabbrev "company-dabbrev")
(autoload 'company-dabbrev-code "company-dabbrev-code")
(autoload 'company-etags "company-etags")
(autoload 'company-elisp "company-elisp")
(autoload 'company-files "company-files")
(autoload 'company-gtags "company-gtags")
(autoload 'company-ispell "company-ispell")

