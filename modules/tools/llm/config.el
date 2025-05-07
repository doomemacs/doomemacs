;;; tools/llm/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :defer t
  :config
  (setq gptel-display-buffer-action nil)  ; if user changes this, popup manager will bow out
  (set-popup-rule!
    (lambda (bname _action)
      (and (null gptel-display-buffer-action)
           (buffer-local-value 'gptel-mode (get-buffer bname))))
    :select t
    :size 0.3
    :quit nil))


(use-package! gptel-quick
  :defer t
  :config
  (when (modulep! :tools lookup)
    ;; TODO: Write `+llm-lookup-documentation-handler'
    ;; (add-hook '+lookup-documentation-functions #'+llm-lookup-documentation-handler)
    ))


(use-package! gptel-magit
  :when (modulep! :tools magit)
  :hook (magit-mode . gptel-magit-install))


;; TODO: Aidermacs?
