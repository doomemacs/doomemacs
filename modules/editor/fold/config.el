;;; editor/fold/config.el -*- lexical-binding: t; -*-

(when (featurep! :feature evil)
  ;; Add vimish-fold, outline-mode & hideshow support to folding commands
  (define-key! 'global
    [remap evil-toggle-fold]   #'+fold/toggle
    [remap evil-close-fold]    #'+fold/close
    [remap evil-open-fold]     #'+fold/open
    [remap evil-open-fold-rec] #'+fold/open
    [remap evil-close-folds]   #'+fold/close-all
    [remap evil-open-folds]    #'+fold/open-all)
  (evil-define-key* 'motion 'global
    "zj" #'+fold/next
    "zk" #'+fold/previous))


;;
;; Packages

(def-package! hideshow ; built-in
  :defer t
  :init
  ;; Ensure `hs-minor-mode' is active when triggering these commands
  (advice-add #'hs-toggle-hiding :before #'+fold-hideshow*ensure-mode)
  (advice-add #'hs-hide-block    :before #'+fold-hideshow*ensure-mode)
  (advice-add #'hs-hide-level    :before #'+fold-hideshow*ensure-mode)
  (advice-add #'hs-show-all      :before #'+fold-hideshow*ensure-mode)
  (advice-add #'hs-hide-all      :before #'+fold-hideshow*ensure-mode)
  :config
  (setq hs-hide-comments-when-hiding-all nil
        ;; Nicer code-folding overlays (with fringe indicators)
        hs-set-up-overlay #'+fold-hideshow-set-up-overlay)

  ;; extra folding support for more languages
  (unless (assq 't hs-special-modes-alist)
    (setq hs-special-modes-alist
          (append
           '((vimrc-mode "{{{" "}}}" "\"")
             (yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>"
                        ""
                        "#"
                        +fold-hideshow-forward-block-by-indent nil)
             (haml-mode "[#.%]" "\n" "/" +fold-hideshow-haml-forward-sexp nil)
             (ruby-mode "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]"
                        "end\\|[]}]"
                        "#\\|=begin"
                        ruby-forward-sexp)
             (enh-ruby-mode "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]"
                            "end\\|[]}]"
                            "#\\|=begin"
                            enh-ruby-forward-sexp nil)
             (matlab-mode "if\\|switch\\|case\\|otherwise\\|while\\|for\\|try\\|catch"
                          "end"
                          nil (lambda (_arg) (matlab-forward-sexp))))
           hs-special-modes-alist
           '((t))))))


(def-package! evil-vimish-fold
  :when (featurep! :feature evil)
  :commands (evil-vimish-fold/next-fold evil-vimish-fold/previous-fold
             evil-vimish-fold/delete evil-vimish-fold/delete-all
             evil-vimish-fold/create evil-vimish-fold/create-line)
  :init
  (setq vimish-fold-dir (concat doom-cache-dir "vimish-fold/")
        vimish-fold-indication-mode 'right-fringe)
  (evil-define-key* 'motion 'global
    "zf" #'evil-vimish-fold/create
    "zF" #'evil-vimish-fold/create-line
    "zd" #'vimish-fold-delete
    "zE" #'vimish-fold-delete-all)
  :config
  (vimish-fold-global-mode +1))
