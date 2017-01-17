
(defvar +doom-theme 'doom-one
  "The color theme currently in use.")

(defvar +doom-dashboard t
  "")

(defvar +doom-modeline t "Whether or not to enable the modeline")

; (when +doom-dashboard (load! +dashboard))
; (when +doom-modeline  (load! +modeline))

;;
(after! doom-themes
  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-file-icons 'simple
        doom-neotree-line-spacing 3)

  (load-theme +doom-theme t)

  ;; brighter source buffers
  (add-hook 'find-file-hook 'doom-buffer-mode)

  ;; neotree + nlinum integration
  (require 'doom-neotree)
  (require 'doom-nlinum))
