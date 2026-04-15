;;; app/everywhere/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +everywhere-app-info-hyprland ()
  "Return information on the current active window, on a Linux Hyprland session."
  (require 'json)
  (let-alist
      (json-read-from-string
       (emacs-everywhere--call "hyprctl" "-j" "activewindow"))
    (make-emacs-everywhere-app
     :id .address
     :class .class
     :title .title
     :geometry (list (aref .at 0)
                     (aref .at 1)
                     (aref .size 0)
                     (aref .size 1)))))

;;;###autoload
(defun +everywhere-app-info-niri ()
  "Return information on the current active window, on a Linux Niri session."
  (unless (getenv "NIRI_SOCKET")
    (unless (car-safe sockets)
      (user-error "Could not find an active niri socket"))
    (when-let* ((sockets (doom-glob (format "/run/user/%d/" (user-uid)) "niri.*sock")))
      (setenv "NIRI_SOCKET" (car-safe sockets))
      (setq +everywhere--niri-socket t)))
  (require 'json)
  (let* ((json-raw (emacs-everywhere--call "niri" "msg" "-j" "focused-window"))
         (is-err (string-prefix-p "Error" json-raw)))
    (if is-err
        (progn
          (message "[emacs-everywhere] %s" json-raw)
          (message "[emacs-everywhere] NIRI_SOCKET=%s" (getenv "NIRI_SOCKET"))
          (user-error "[emacs-everywhere] Error in `niri msg -j focused-window' (see *messages*)"))
      (let-alist (json-read-from-string json-raw)
        (make-emacs-everywhere-app
         :id (if (numberp .id) (number-to-string .id) .id)
         :class .app_id
         :title .title)))))
