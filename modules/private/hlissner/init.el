;;; private/hlissner/init.el -*- lexical-binding: t; -*-

;; An extra measure to prevent the flash of unstyled mode-line while Emacs is
;; booting up (when Doom is byte-compiled).
(setq-default mode-line-format nil)

;; I've swapped these keys on my keyboard
(setq x-super-keysym 'alt
      x-alt-keysym   'meta)

(setq user-mail-address "henrik@lissner.net"
      user-full-name    "Henrik Lissner")

(setq doom-big-font (font-spec :family "Fira Mono" :size 19))

(pcase (system-name)
  ("proteus"
   ;; My 13" laptop has very little screen estate, so we use a bitmap font
   ;; there. For Doom, that means we need to take up less space!
   (setq-default line-spacing 1)

   (setq doom-font (font-spec :family "kakwa kakwafont" :size 12)
         doom-variable-pitch-font (font-spec :family "kakwa kakwafont")
         doom-unicode-font (font-spec :family "UT Ttyp0")
         ;; ui/doom-modeline
         +doom-modeline-height 23
         ;; `doom-themes'
         doom-neotree-enable-variable-pitch nil
         doom-neotree-project-size 1.2
         doom-neotree-line-spacing 0
         doom-neotree-folder-size 1.0
         doom-neotree-chevron-size 0.6)
   (add-hook! doom-big-font-mode
     (setq +doom-modeline-height (if doom-big-font-mode 37 23)))

   ;; No highlighted bar in the mode-line
   (setq +doom-modeline-bar-width 1)
   (custom-set-faces '(doom-modeline-bar ((t (:background nil))))))

  (_
   ;; Everywhere else, I have big displays and plenty of space, so use it!
   (setq doom-font (font-spec :family "Fira Mono" :size 12)
         doom-variable-pitch-font (font-spec :family "Fira Sans")
         doom-unicode-font (font-spec :family "DejaVu Sans Mono")
         org-ellipsis "  ")

   ;; Fira Mono doesn't have italics, so we highlight it instead.
   (add-hook! doom-post-init
     (set-face-attribute 'italic nil :weight 'ultra-light :foreground "#ffffff"))

   (add-hook! doom-big-font-mode
     (setq +doom-modeline-height (if doom-big-font-mode 37 29)))))
