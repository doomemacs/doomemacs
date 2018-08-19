;;; emacs/hideshow/config.el -*- lexical-binding: t; -*-

(after! hideshow  ; built-in
  (setq hs-hide-comments-when-hiding-all nil
        ;; Nicer code-folding overlays (with fringe indicators)
        hs-set-up-overlay #'+hideshow-set-up-overlay)

  ;; extra folding support for more languages
  (unless (assq 't hs-special-modes-alist)
    (setq hs-special-modes-alist
          (append
           '((vimrc-mode "{{{" "}}}" "\"")
             (yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>"
                        ""
                        "#"
                        +hideshow-forward-block-by-indent nil)
             (haml-mode "[#.%]" "\n" "/" +hideshow-haml-forward-sexp nil)
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
                          nil (lambda (arg) (matlab-forward-sexp))))
           hs-special-modes-alist
           '((t))))))

