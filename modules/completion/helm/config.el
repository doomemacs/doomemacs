;;; completion/helm/config.el -*- lexical-binding: t; -*-

;; Warning: since I don't use helm, this may be out of date.

(defvar +helm-global-prompt "››› "
  "The helm text prompt prefix string is globally replaced with this string.")


;;
;; Packages
;;

(def-package! helm
  :demand t
  :init
  (setq helm-quick-update t
        ;; Speedier without fuzzy matching
        helm-mode-fuzzy-match nil
        helm-buffers-fuzzy-matching nil
        helm-apropos-fuzzy-match nil
        helm-M-x-fuzzy-match nil
        helm-recentf-fuzzy-match nil
        helm-projectile-fuzzy-match nil
        ;; Display extraineous helm UI elements
        helm-display-header-line nil
        helm-ff-auto-update-initial-value nil
        helm-find-files-doc-header nil
        ;; Don't override evil-ex's completion
        helm-mode-handle-completion-in-region nil
        helm-candidate-number-limit 50
        ;; Don't wrap item cycling
        helm-move-to-line-cycle-in-source t)

  :config
  (load "helm-autoloads" nil t)
  (setq projectile-completion-system 'helm)
  (set! :popup "\\` ?\\*[hH]elm.*?\\*\\'" :size 14 :regexp t)

  (add-hook 'emacs-startup-hook #'helm-mode)

  ;;; Helm hacks
  ;; A simpler prompt: see `+helm-global-prompt'
  (defun +helm*replace-prompt (plist)
    (if (keywordp (car plist))
        (plist-put plist :prompt +helm-global-prompt)
      (setf (nth 2 plist) +helm-global-prompt)
      plist))
  (advice-add #'helm :filter-args #'+helm*replace-prompt)

  ;; Hide mode-line in helm windows
  (defun +helm*hide-header (&rest _))
  (advice-add #'helm-display-mode-line :override #'+helm*hide-header)

  (map! [remap apropos]                   #'helm-apropos
        [remap find-file]                 #'helm-find-files
        [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer
        [remap recentf]                   #'helm-recentf
        [remap projectile-recentf]        #'helm-projectile-recentf
        [remap projectile-find-file]      #'helm-projectile-find-file
        [remap imenu]                     #'helm-semantic-or-imenu
        [remap bookmark-jump]             #'helm-bookmarks
        [remap noop-show-kill-ring]       #'helm-show-kill-ring
        [remap projectile-switch-project] #'helm-projectile-switch-project
        [remap projectile-find-file]      #'helm-projectile-find-file
        [remap imenu-anywhere]            #'helm-imenu-anywhere
        [remap execute-extended-command]  #'helm-M-x

        (:map helm-map
          "C-S-n"      #'helm-next-source
          "C-S-p"      #'helm-previous-source
          "C-u"        #'helm-delete-minibuffer-contents
          "C-w"        #'backward-kill-word
          "M-v"        #'clipboard-yank
          "C-r"        #'evil-paste-from-register ; Evil registers in helm! Glorious!
          "C-b"        #'backward-word
          "<left>"     #'backward-char
          "<right>"    #'forward-char
          "<escape>"   #'helm-keyboard-quit
          "ESC"        #'helm-keyboard-quit
          [escape]     #'helm-keyboard-quit
          "<tab>"      #'helm-execute-persistent-action)

        (:map* helm-generic-files-map
          :e "ESC"     #'helm-keyboard-quit))

  (defvar helm-projectile-find-file-map (make-sparse-keymap))
  (require 'helm-projectile)
  (set-keymap-parent helm-projectile-find-file-map helm-map))


(def-package! helm-locate
  :init (defvar helm-generic-files-map (make-sparse-keymap))
  :config (set-keymap-parent helm-generic-files-map helm-map))


(def-package! helm-bookmark
  :config (setq-default helm-bookmark-show-location t))


(def-package! helm-files
  :config
  (setq helm-boring-file-regexp-list
        (append (list "\\.projects$" "\\.DS_Store$")
                helm-boring-file-regexp-list))

  (map! :map helm-find-files-map
        "C-w" #'helm-find-files-up-one-level
        "TAB" #'helm-execute-persistent-action))


(def-package! helm-ag
  :config
  (map! (:map helm-ag-map
          "<backtab>"  #'helm-ag-edit)
        (:map helm-ag-edit-map
          [remap doom/kill-this-buffer] #'helm-ag--edit-abort
          [remap quit-window]           #'helm-ag--edit-abort)))


(def-package! helm-css-scss ; https://github.com/ShingoFukuyama/helm-css-scss
  :commands (helm-css-scss
             helm-css-scss-multi
             helm-css-scss-insert-close-comment)
  :config
  (setq helm-css-scss-split-direction #'split-window-vertically
        helm-css-scss-split-with-multiple-windows t))


(def-package! helm-swoop ; https://github.com/ShingoFukuyama/helm-swoop
  :commands (helm-swoop helm-multi-swoop helm-multi-swoop-all)
  :config
  (setq helm-swoop-use-line-number-face t
        helm-swoop-candidate-number-limit 200
        helm-swoop-speed-or-color t
        helm-swoop-pre-input-function (lambda () "")))


(def-package! helm-describe-modes :commands helm-describe-modes)


;;
;; Popup hacks
;;

(after! helm
  ;; Helm tries to clean up after itself, but shackle has already done this.
  ;; This fixes that. To reproduce, add a helm rule in `shackle-rules', open two
  ;; splits side-by-side, move to the buffer on the right and invoke helm. It
  ;; will close all but the left-most buffer.
  (setq-default helm-reuse-last-window-split-state t
                helm-split-window-in-side-p t))

(after! helm-swoop
  (setq helm-swoop-split-window-function (lambda (b) (doom-popup-buffer b))))


(after! helm-ag
  ;; This prevents helm-ag from switching between windows and buffers.
  (defun +helm*ag-edit-done (orig-fn &rest args)
    (cl-letf (((symbol-function 'select-window) #'ignore))
      (apply orig-fn args))
    (doom/popup-close))
  (advice-add #'helm-ag--edit-commit :around #'+helm*ag-edit-done)
  (advice-add #'helm-ag--edit-abort :around #'+helm*ag-edit-done)

  (defun +helm*ag-edit (orig-fn &rest args)
    (cl-letf (((symbol-function 'other-window) #'ignore)
              ((symbol-function 'switch-to-buffer) #'doom-popup-buffer))
      (apply orig-fn args)
      (with-current-buffer (get-buffer "*helm-ag-edit*")
        (use-local-map helm-ag-edit-map))))
  (advice-add #'helm-ag--edit :around #'+helm*ag-edit))

