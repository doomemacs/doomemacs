;;; emacs/eww/config.el -*- lexical-binding: t; -*-

(use-package! eww
  :defer t
  :config
  (map! :map eww-mode-map
        [remap text-scale-increase] #'+eww/increase-font-size
        [remap text-scale-decrease] #'+eww/decrease-font-size
        [remap imenu] #'+eww/jump-to-heading-on-page
        [remap quit-window] #'+eww/quit
        :ni [C-return] #'+eww/open-in-other-window
        :n "yy" #'+eww/copy-current-url
        :n "zk" #'text-scale-increase
        :n "zj" #'text-scale-decrease

        (:localleader
         :desc "external browser" "e" #'eww-browse-with-external-browser
         :desc "buffers" "b" #'eww-switch-to-buffer
         :desc "jump to link" "l" #'+eww/jump-to-url-on-page

         (:prefix ("t" . "toggle")
          :desc "readable" "r" #'eww-readable
          :desc "colors" "c" #'eww-toggle-colors
          :desc "fonts" "f" #'eww-toggle-fonts
          :desc "images" "i" #'eww-toggle-images)

         (:prefix ("y" . "copy")
          :desc "copy url" "y" #'+eww/copy-current-url
          :desc "copy for Org" "o" #'org-eww-copy-for-org-mode)))

  ;; HACK: There are packages that use eww to pop up html documentation; we want
  ;;   those to open in a popup, but if the user calls `eww' directly, it should
  ;;   open in the current window.
  (defadvice! +eww-open-in-fullscreen-if-interactive-a (fn &rest args)
    :around #'eww
    (if (called-interactively-p 'any)
        (apply fn args)
      (let (display-buffer-alist)
        (apply fn args))))

  ;; HACK: Rename the eww buffer to match the open page's title or URL.
  (if (boundp 'eww-auto-rename-buffer)
      (setq eww-auto-rename-buffer #'+eww-page-title-or-url)  ; for >=29.1
    ;; REVIEW: Remove when we drop 28 support
    (add-hook! 'eww-after-render-hook
      (defun +eww--rename-buffer-to-page-title-or-url-h (&rest _)
        (rename-buffer (+eww-page-title-or-url))))
    (advice-add #'eww-back-url :after #'+eww--rename-buffer-to-page-title-or-url-h)
    (advice-add #'eww-forward-url :after #'+eww--rename-buffer-to-page-title-or-url-h)))
