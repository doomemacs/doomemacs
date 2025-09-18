;;; tools/editorconfig/config.el -*- lexical-binding: t; -*-

;; TODO: Adapt to built-in `editorconfig' in Emacs 30+
;; Handles whitespace (tabs/spaces) settings externally. This way projects can
;; specify their own formatting rules.
(use-package! editorconfig
  :hook (doom-first-buffer . editorconfig-mode)
  :config
  ;; The elisp implementation is the default (rather than the external,
  ;; editorconfig binary), because upstream claims it's "faster and more
  ;; secure". Whether that's true or not, I argue the principle of least
  ;; surprise: if the user has the editorconfig binary installed, they're likely
  ;; expecting it to be used.
  (setq editorconfig-get-properties-function #'editorconfig-get-properties)

  (when (require 'ws-butler nil t)
    (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))

  ;; Fix #5057 archives don't need editorconfig settings, and they may otherwise
  ;; interfere with the process of opening them (office formats are zipped XML
  ;; formats).
  (add-to-list 'editorconfig-exclude-regexps
               "\\.\\(zip\\|\\(doc\\|xls\\|ppt\\)x\\)\\'")

  (add-hook! 'editorconfig-after-apply-functions
    (defun +editorconfig-disable-indent-detection-h (props)
      "Inhibit `dtrt-indent' if an explicit indent_style and indent_size is
specified by editorconfig."
      (when (and (not doom-inhibit-indent-detection)
                 (or (gethash 'indent_style props)
                     (gethash 'indent_size props)))
        (setq doom-inhibit-indent-detection 'editorconfig)))
    ;; I use a hook over `editorconfig-exclude-modes' because the option
    ;; inhibits all settings, and I only want to inhibit indent_size. Plus modes
    ;; in that option won't apply to derived modes, so we'd have to add *all*
    ;; possible org-mode derivatives to it.
    (defun +editorconfig-unset-tab-width-in-org-mode-h (props)
      "A tab-width != 8 is an error state in org-mode, so prevent changing it."
      (when (and (gethash 'indent_size props)
                 (derived-mode-p 'org-mode))
        (setq tab-width 8)))))
