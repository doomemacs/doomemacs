;;; tools/llm/config.el -*- lexical-binding: t; -*-

(autoload 'gptel-org-set-topic "gptel-org" nil t)
(autoload 'gptel-org-set-properties "gptel-org" nil t)


(use-package! gptel
  :defer t
  :config
  (set-debug-var! 'gptel-log-level 'debug)

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
    (let (gptel-include-reasoning)
      (apply fn args)))

  ;; HACK: Responses from the system/API calls might not be a string, causing
  ;;   type errors. It also doesn't do any error handling at all, so we do it.
  ;; REVIEW: Remove these when ragnard/gptel-magit#9 is resolved OR
  ;;   ragnard/gptel-magit#4 is merged.
  (defadvice! +llm--fix-gptel-magit--non-string-responses-a (args)
    :filter-args #'gptel-magit--request
    (when-let* ((callback (plist-get (cdr args) :callback)))
      (cl-callf plist-put (cdr args)
        :callback (lambda (response info)
                    (if (stringp response)
                        (funcall callback response info)
                      (message "gptel-magit error: %s: %s"
                               (plist-get info :status)
                               (plist-get (plist-get info :error) :message))))))
    args))


(use-package! ob-gptel
  :when (modulep! :lang org)
  :hook (org-mode . +llm-ob-gptel-install-completions-h)
  :config
  (defun +llm-ob-gptel-install-completions-h ()
    (add-hook 'completion-at-point-functions 'ob-gptel-capf nil t)))
