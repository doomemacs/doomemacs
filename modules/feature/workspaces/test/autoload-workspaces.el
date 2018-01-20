;; -*- no-byte-compile: t; -*-
;;; feature/workspaces/test/autoload-workspaces.el

(require! :feature workspaces)

(defmacro with-workspace!! (buffer-args &rest body)
  (declare (indent defun))
  (let ((buffers
         (cl-loop for bsym in buffer-args
                  collect `(,bsym (get-buffer-create ,(symbol-name bsym))))))
    `(let ((persp-auto-resume-time -1)
           (persp-auto-save-opt 0))
       (require 'persp-mode)
       (let (noninteractive)
         (persp-mode +1))
       (+workspace-switch +workspaces-main t)
       (let* (,@buffers)
         (cl-loop with persp = (get-current-persp)
                  for buf in (list ,@(mapcar #'car buffers))
                  do (persp-add-buffer buf persp)
                  do (with-current-buffer buf
                       (setq buffer-file-name (make-temp-file "workspaces-test-"))))
         ,@body
         (let (kill-buffer-query-functions kill-buffer-hook)
           (mapc #'kill-buffer (list ,@(mapcar #'car buffers)))))
       (let (noninteractive)
         (persp-mode -1)))))

;; `+workspaces|init'
(def-test! init
  (with-workspace!! ()
    (should (equal (+workspace-current-name) +workspaces-main))))

;; `+workspaces*auto-add-buffer'
(def-test! auto-add-buffer-to-persp
  (let ((a (generate-new-buffer "a")))
    (doom-set-buffer-real a t)
    (with-workspace!! ()
      (should-not (+workspace-contains-buffer-p a))
      (switch-to-buffer a)
      (should (+workspace-contains-buffer-p a)))))

;; `+workspace-current'
;; `+workspace-current-name'
(def-test! current
  (with-workspace!! ()
    (should (equal (+workspace-current-name) +workspaces-main))
    (should (+workspace-exists-p +workspaces-main))
    (let ((workspace (+workspace-get +workspaces-main))
          (current-workspace (+workspace-current)))
      (should workspace)
      (should (+workspace-p workspace))
      (should (+workspace-p current-workspace))
      (should (equal workspace current-workspace)))))

;; `+workspace-list'
;; `+workspace-list-names'
(def-test! workspace-list
  (with-workspace!! ()
    (should (equal (+workspace-list-names)
                   (list (+workspace-current-name))))
    (should (equal (+workspace-list)
                   (list (+workspace-current))))))

;; `+workspace-new'
;; `+workspace-rename'
;; `+workspace-delete'
(def-test! workspace-crud
  "Creating, reading, updating and deleting workspaces."
  (with-workspace!! ()
    (let ((new-workspace-name "*new-test*")
          (renamed-workspace-name "*old-test*"))
      (should (+workspace-new new-workspace-name))
      (should (seq-contains (+workspace-list-names) new-workspace-name))
      (should (equal new-workspace-name
                     (+workspace-rename new-workspace-name renamed-workspace-name)))
      (should-not (seq-contains (+workspace-list-names) new-workspace-name))
      (should (seq-contains (+workspace-list-names) renamed-workspace-name))
      (should (= (length (+workspace-list-names)) 2))
      (+workspace-delete renamed-workspace-name)
      (should (= (length (+workspace-list-names)) 1)))))

;; `+workspace-switch'
(def-test! workspace-switch
  (with-workspace!! ()
    (let ((new-workspace-name "*new-test*"))
      (should-error (+workspace-switch new-workspace-name))
      (should (+workspace-switch new-workspace-name t))
      (should (equal (+workspace-current-name) new-workspace-name)))))

;; `+workspace-buffer-list'
;; `+workspace-contains-buffer-p'
(def-test! buffer-list
  (with-workspace!! (a b)
    (let ((c (get-buffer-create "c"))
          (d (get-buffer-create "d")))
      (should (+workspace-contains-buffer-p a))
      (should (+workspace-contains-buffer-p b))
      (should-not (+workspace-contains-buffer-p c))
      ;; New (and real) buffers should be added to workspace buffer list.
      (doom-set-buffer-real c t)
      (switch-to-buffer "c")
      (should (+workspace-contains-buffer-p c))
      ;; unreal buffers shouldn't
      (switch-to-buffer "d")
      (should-not (+workspace-contains-buffer-p d)))))

;; `+workspace/close-window-or-workspace'
(def-test! close-window-or-workspace
  (with-workspace!! (a b)
    (let ((ws (+workspace-current-name))
          (inhibit-message t))
      (+workspace-switch "test" t)
      (split-window)
      (should (equal (+workspace-current-name) "test"))
      (should (= (length (doom-visible-windows)) 2))
      ;; kill window if more than one
      (+workspace/close-window-or-workspace)
      (should (= (length (doom-visible-windows)) 1))
      ;; kill workspace on last window
      (+workspace/close-window-or-workspace)
      (should (equal (+workspace-current-name) "main")))))
