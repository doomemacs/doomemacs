;;; tools/llm/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :defer t
  :config
  (setq gptel-display-buffer-action nil   ; if changed, popup manager will bow out
        gptel-default-mode 'org-mode)

  (set-popup-rule!
    (lambda (bname _action)
      (and (null gptel-display-buffer-action)
           (buffer-local-value 'gptel-mode (get-buffer bname))))
    :select t
    :size 0.3
    :quit nil
    :ttl nil))


(use-package! gptel-quick
  :defer t
  :config
  (when (modulep! :tools lookup)
    ;; TODO: Write `+llm-lookup-documentation-handler'
    ;; (add-hook '+lookup-documentation-functions #'+llm-lookup-documentation-handler)
    ))


(use-package! gptel-magit
  :when (modulep! :tools magit)
  :hook (magit-mode . gptel-magit-install)
  :config
  ;; HACK: `gptel-include-reasoning' can break gptel-magit, and needs to be
  ;;   excluded if you use openrouter.
  ;; REVIEW: Remove when ragnard/gptel-magit#8 is resolved.
  (defadvice! +llm--fix-gptel-magit--omit-reasoning-a (fn &rest args)
    :around #'gptel-magit--generate
    (let ((gptel-include-reasoning nil)
          (gptel--request-params
           (if (eq gptel-magit-backend gptel--openrouter)
               '(:reasoning (:exclude t :effort "minimal"))
             nil)))
      (apply fn args))))


(use-package! ob-gptel
  :when (modulep! :lang org)
  :hook (org-mode . +llm-ob-gptel-install-completions-h)
  :config
  (defun +llm-ob-gptel-install-completions-h ()
    (add-hook 'completion-at-point-functions 'ob-gptel-capf nil t)))
