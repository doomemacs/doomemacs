;;; tools/prodigy/config.el -*- lexical-binding: t; -*-

(after! prodigy
  (set! :evil-state 'prodigy-mode 'emacs)

  ;; Make services, etc persistent between Emacs sessions
  (doom-cache-persist
   :prodigy '(prodigy-services prodigy-tags prodigy-filters))

  (advice-add #'prodigy-services :around #'+prodigy*services)

  (define-key prodigy-mode-map "d" #'+prodigy/delete))

