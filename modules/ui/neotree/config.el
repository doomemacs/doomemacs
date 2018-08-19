;;; ui/neotree/config.el -*- lexical-binding: t; -*-

(def-package! neotree
  :commands (neotree-show
             neotree-hide
             neotree-toggle
             neotree-dir
             neotree-find
             neo-global--with-buffer
             neo-global--window-exists-p)
  :config
  (setq neo-create-file-auto-open nil
        neo-auto-indent-point nil
        neo-autorefresh nil
        neo-mode-line-type 'none
        neo-window-width 28
        neo-show-updir-line nil
        neo-theme 'nerd ; fallback
        neo-banner-message nil
        neo-confirm-create-file #'off-p
        neo-confirm-create-directory #'off-p
        neo-show-hidden-files nil
        neo-keymap-style 'concise
        neo-show-hidden-files t
        neo-hidden-regexp-list
        '(;; vcs folders
          "^\\.\\(?:git\\|hg\\|svn\\)$"
          ;; compiled files
          "\\.\\(?:pyc\\|o\\|elc\\|lock\\|css.map\\|class\\)$"
          ;; generated files, caches or local pkgs
          "^\\(?:node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
          ;; org-mode folders
          "^\\.\\(?:sync\\|export\\|attach\\)$"
          ;; temp files
          "~$"
          "^#.*#$"))

  (set-popup-rule! "^ ?\\*NeoTree"
    :side neo-window-position :size neo-window-width
    :quit 'current :select t)

  (after! winner
    (add-to-list 'winner-boring-buffers neo-buffer-name))

  ;; The cursor always sits at bol. `+neotree*fix-cursor' and
  ;; `+neotree*indent-cursor' change that behavior, so that the cursor is always
  ;; on the first non-blank character on the line, in the neo buffer.
  (defun +neotree*fix-cursor (&rest _)
    (with-current-buffer neo-global--buffer
      (+neotree*indent-cursor)))
  (add-hook 'neo-enter-hook #'+neotree*fix-cursor)

  (defun +neotree*indent-cursor (&rest _)
    (beginning-of-line)
    (skip-chars-forward " \t\r"))
  (advice-add #'neotree-next-line :after #'+neotree*indent-cursor)
  (advice-add #'neotree-previous-line :after #'+neotree*indent-cursor))
