(provide 'init-auto-complete)

(defconst my-dicts-dir  (concat my-dir "dict/"))

(setq tags-case-fold-search nil)

(use-package pos-tip)

(use-package auto-complete
  :init
  (progn
    (require 'auto-complete-config)

    (setq ac-auto-start nil
          ac-auto-show-menu t        ; Suggestions box must be invoked manually (see core-keymaps.el)
          ac-use-menu-map t          ; Enable ac-menu-map map when menu is open
          ac-use-quick-help nil      ; Don't show tooltips unless invoked (see core-keymaps.el)
          ac-use-fuzzy nil
          ac-use-comphist t
          ac-candidate-limit 25)
    (setq ac-comphist-file (concat my-tmp-dir "ac-comphist.dat"))

    (setq-default ac-sources '(ac-source-yasnippet)))
  :config
  (progn
    ;; Redefine this function so auto-complete is available [almost] everywhere
    (defun auto-complete-mode-maybe ()
      (unless (minibufferp (current-buffer))
        (auto-complete-mode 1)))

    (defun auto-complete--backend (hook &rest backends)
      (add-hook hook
                `(lambda()
                   (set (make-local-variable 'ac-sources) (append '(,@backends) ac-sources)))))

    (global-auto-complete-mode t)

    (let ((bg (face-attribute 'default :background)))
      (require 'color)
      (custom-set-faces
       ;; `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 15)))))
       ;; `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
       `(ac-yasnippet-candidate-face ((t (:inherit ac-candidate-face :background ,(color-lighten-name bg 5)))))
       `(ac-yasnippet-selection-face ((t (:inherit ac-selection-face :background ,(color-lighten-name bg 10)))))
       `(ac-completion-face ((t (:background ,(color-lighten-name bg 15)))))
       `(ac-candidate-face ((t (:inherit font-lock-function-name-face :background ,(color-lighten-name bg 4)))))
       `(ac-selection-face ((t (:background ,(color-lighten-name bg 9)))))))

    (add-to-list 'ac-dictionary-directories my-dicts-dir)

    (auto-complete--backend 'emacs-lisp-mode-hook 'ac-source-features 'ac-source-functions 'ac-source-variables 'ac-source-symbols)
    (auto-complete--backend 'scss-mode-hook 'ac-source-css-property)

    (push '("*Popup Help*" :position bottom :height 0.35 :noselect t) popwin:special-display-config)

    ;; Tell ido not to care about case
    ;; (setq completion-ignore-case t)

    (bind 'insert ac-mode-map
          (kbd "C-x C-k")   'ac-complete-dictionary
          (kbd "C-x C-f")   (λ (let ((ac-sources '(ac-source-filename ac-source-files-in-current-dir)))
                                 (auto-complete)))
          (kbd "C-x C-]")   'ac-complete-etags
          (kbd "C-x s")     'ac-complete-ispell
          (kbd "C-x C-s")   'ac-complete-yasnippet
          (kbd "C-x C-n")   'ac-complete-words-in-all-buffer
          (kbd "C-x C-p")   'ac-complete-words-in-same-mode-buffers
          (kbd "C-x C-o")   'auto-complete
          (kbd "C-SPC")     'auto-complete)

    (bind ac-completing-map
          (kbd "<tab>")      'ac-complete
          (kbd "C-n")        'ac-next
          (kbd "C-p")        'ac-previous
          (kbd "C-h")        'ac-quick-help
          (kbd "C-S-h")      'ac-help
          (kbd "<ESC>")      'ac-stop
          (kbd "<RET>")      'ac-complete)

    (use-package ac-etags
      :commands (ac-complete-etags)
      :config (ac-etags-setup))

    (use-package ac-ispell
      :commands (ac-complete-ispell ac-complete-ispell-fuzzy)
      :config
      (progn (ac-ispell-setup)
             (setq ac-ispell-requires 1
                   ac-ispell-fuzzy-limit 25)))))
