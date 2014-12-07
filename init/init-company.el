(provide 'init-company)

(use-package company
  :init
  (global-company-mode 1)
  :config
  (progn
    (setq company-idle-delay nil)
    (setq company-minimum-prefix-length 1)
    (setq company-show-numbers nil)
    (setq company-tooltip-limit 20)

    (setq company-dabbrev-downcase nil)
    (setq company-dabbrev-ignore-case nil)
    (setq company-tooltip-align-annotations t)
    (setq company-require-match 'never)

    (setq company-global-modes
          '(not eshell-mode comint-mode org-mode erc-mode message-mode help-mode))

    (require 'color)
    (let ((bg (face-attribute 'default :background)))
      (custom-set-faces
       `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 9)))))
       `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 15)))))
       `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
       `(company-search ((t (:background ,(color-lighten-name bg 15)))))
       `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
       `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

    ;; Sort candidates by
    (add-to-list 'company-transformers 'company-sort-by-occurrence)
    ;; (add-to-list 'company-transformers 'company-sort-by-backend-importance)

    (progn ; frontends
      (setq-default company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                                        company-echo-metadata-frontend
                                        company-preview-if-just-one-frontend)))

    (progn ; backends
      (setq-default company-backends '(company-capf (company-yasnippet company-keywords)))
      (make-variable-buffer-local 'company-backends)

      (defun company--backend-on (hook &rest backends)
        (add-hook hook
                  `(lambda()
                     (setq company-backends
                           (append '((,@backends company-yasnippet)) company-backends)))))

      (company--backend-on 'nxml-mode-hook 'company-nxml)
      (company--backend-on 'emacs-lisp-mode-hook 'company-elisp)

      ;; Rewrite evil-complete to use company-dabbrev
      (setq company-dabbrev-code-other-buffers t)
      (setq company-dabbrev-code-buffers nil)

      (setq evil-complete-next-func
            (lambda(arg)
              (if company-candidates
                  (call-interactively 'company-select-next)
                (call-interactively 'company-dabbrev))))

      (setq evil-complete-previous-func
            (lambda (arg)
              (let ((company-selection-wrap-around t))
                (call-interactively 'company-dabbrev)
                (call-interactively 'company-select-previous))))

    (progn ; keybinds
      (bind 'insert company-mode-map
            "C-SPC"     'company-complete-common
            "C-x C-k"   'company-keywords
            "C-x C-f"   'company-files
            "C-x C-]"   'company-etags
            "C-x s"     'company-ispell
            "C-x C-s"   'company-yasnippet
            "C-x C-o"   'company-complete
            "C-x C-n"   'company-dabbrev-code
            "C-x C-p"   (λ (let ((company-selection-wrap-around t))
                             (call-interactively 'company-dabbrev-code)
                             (company-select-previous-or-abort))))

      (bind company-active-map
            "C-w"        nil
            "C-o"        'company-search-kill-others
            "C-n"        'company-select-next-or-abort
            "C-p"        'company-select-previous-or-abort
            "C-h"        'company-show-doc-buffer
            "C-S-h"      'company-show-location
            "C-S-s"      'company-search-candidates
            "C-s"        'company-filter-candidates
            "C-SPC"      'company-complete-common
            [tab]        'company-complete
            "<backtab>"  'company-select-previous
            [escape]     'company-abort)

      (bind company-search-map
            "C-n"        'company-search-repeat-forward
            "C-p"        'company-search-repeat-backward
            [escape]     'company-abort)

      ;; (bind company-filter-map
      ;;       [escape]     'company-filterd)

      (after "helm-company"
             (bind company-mode-map   "<C-return>" 'helm-company)
             (bind company-active-map "<C-return>" 'helm-company))))))
