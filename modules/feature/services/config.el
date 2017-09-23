;;; feature/services/config.el -*- lexical-binding: t; -*-

(def-setting! :service (&rest plist)
  "TODO"
  `(after! prodigy
     (prodigy-define-service ,@plist)))


;;
;; Plugins
;;

(def-package! prodigy
  :commands (prodigy prodigy-view-mode prodigy-add-filter)
  :config
  (set! :evil-state 'prodigy-mode 'emacs)

  ;; Make services, etc persistent between Emacs sessions
  (setq prodigy-services (persistent-soft-fetch 'prodigy-services "prodigy")
        prodigy-tags     (persistent-soft-fetch 'prodigy-tags "prodigy")
        prodigy-filters  (persistent-soft-fetch 'prodigy-filters "prodigy"))

  (defun +services|save ()
    "Save all services, tags and filters to files."
    (+services/cleanup)
    (cl-loop for sym in '(prodigy-services prodigy-tags prodigy-filters)
             do (persistent-soft-store sym (symbol-value sym) "prodigy")))
  (add-hook 'kill-emacs-hook #'+services|save)

  (defun +services*prodigy-services (orig-fn &rest args)
    "Adds a new :project property to prodigy services, which hides the service
unless invoked from the relevant project."
    (let ((project-root (downcase (doom-project-root)))
          (services (apply orig-fn args)))
      (if current-prefix-arg
          services
        (cl-remove-if-not (lambda (service)
                            (let ((project (plist-get service :project)))
                              (or (not project)
                                  (file-in-directory-p project-root project))))
                          services))))
  (advice-add #'prodigy-services :around #'+services*prodigy-services)

  ;; Keybindings
  (map! :map prodigy-mode-map "d" #'+services/prodigy-delete)
  (when (featurep! :feature evil)
    (map! :map prodigy-mode-map
          "j" #'prodigy-next
          "k" #'prodigy-prev)))

