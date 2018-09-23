;;; lang/ocaml/config.el -*- lexical-binding: t; -*-

(def-package! tuareg
  :mode ("\\.ml[4ilpy]?\\'" . tuareg-mode)
  :config
  (def-package! utop
    :when (and (featurep! :feature eval)
               (executable-find "opam")
               (executable-find "utop"))
    :init
    (setq utop-command "opam config exec -- utop -emacs")
    :hook
    (tuareg-mode . utop-mode)
    :config
    (set-popup-rules! "*\\utop" :quit nil :ttl nil)
    (set-repl-handler! 'tuareg-mode #'utop)
    (set-lookup-handlers! 'tuareg-mode
      :definition #'utop-type-at-point
      :documentation #'utop-ident-at-point)
    (map! :after utop
          :map utop-mode-map
          :localleader
          :desc "eval input or newline" :nve [return] #'utop-eval-input-or-newline
          :desc "bol" :nve "a" #'utop-bol
          :desc "history goto prev" :nve "p" #'utop-history-goto-prev
          :desc "history goto next" :nve "n" #'utop-history-goto-next
          :desc "interrupt" :nve "i" #'utop-interrupt
          :desc "kill" :nve "k" #'utop-kill
          :desc "copy old input" :nve "c" #'utop-copy-old-input)))


    



(def-package! merlin
  :after tuareg
  :hook (tuareg-mode . merlin-mode)
  :config
  (set-company-backend! 'tuareg-mode 'merlin-company-backend)
  (after! company
    (remove-hook 'company-backends 'merlin-company-backend)))
