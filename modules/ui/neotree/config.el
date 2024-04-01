;;; ui/neotree/config.el -*- lexical-binding: t; -*-

(use-package! neotree
  :commands (neotree-show
             neotree-hide
             neotree-toggle
             neotree-dir
             neotree-find
             neo-global--with-buffer
             neo-global--window-exists-p)
  :init
  (setq neo-create-file-auto-open nil
        neo-auto-indent-point nil
        neo-autorefresh nil
        neo-mode-line-type 'none
        neo-window-width 30
        neo-show-updir-line nil
        neo-theme 'nerd
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

  :config
  (set-popup-rule! "^ ?\\*NeoTree" :ignore t)

  (after! winner
    (add-to-list 'winner-boring-buffers neo-buffer-name))

  ;; The cursor always sits at bol. `+neotree--fix-cursor-h' and
  ;; `+neotree--indent-cursor-a' change that behavior so that the cursor is
  ;; always on the first non-blank character on the line, in the neo buffer.
  (add-hook! 'neo-enter-hook
    (defun +neotree-fix-cursor-h (&rest _)
      (with-current-buffer neo-global--buffer
        (+neotree--indent-cursor-a))))

  (defadvice! +neotree--indent-cursor-a (&rest _)
    :after '(neotree-next-line neotree-previous-line)
    (beginning-of-line)
    (skip-chars-forward " \t\r"))

  (map! :map neotree-mode-map
        :n [tab] (neotree-make-executor
                  :dir-fn  #'neo-open-dir
                  :file-fn #'neotree-quick-look)
        :n "DEL" #'evil-window-prev
        :n "n"   #'neotree-next-line
        :n "p"   #'neotree-previous-line
        :m "h"   #'+neotree/collapse-or-up
        :m "l"   #'+neotree/expand-or-open
        :n "J"   #'neotree-select-next-sibling-node
        :n "K"   #'neotree-select-previous-sibling-node
        :n "H"   #'neotree-select-up-node
        :n "L"   #'neotree-select-down-node
        :n "G"   #'evil-goto-line
        :n "gg"  #'evil-goto-first-line
        :n "v"   (neotree-make-executor :file-fn 'neo-open-file-vertical-split)
        :n "s"   (neotree-make-executor :file-fn 'neo-open-file-horizontal-split)))
