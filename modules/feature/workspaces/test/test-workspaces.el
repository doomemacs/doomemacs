;; -*- no-byte-compile: t; -*-
;;; feature/workspaces/test/test-workspaces.el

(describe "feature/workspaces"
  :var (persp-auto-resume-time
        persp-auto-save-opt
        persp-switch-to-added-buffer
        persp-autokill-persp-when-removed-last-buffer
        persp-autokill-buffer-on-remove
        in1 in2 out1 out2
        persp1 persp1-name persp2 persp2-name
        wconf)

  (before-all
    (delete-other-windows)
    (require! :feature workspaces)
    (require 'persp-mode))

  (before-each
    (switch-to-buffer "*scratch*")
    (setq wconf (current-window-configuration)
          persp-auto-resume-time -1
          persp-auto-save-opt 0
          persp-switch-to-added-buffer nil
          persp-autokill-persp-when-removed-last-buffer nil
          persp-autokill-buffer-on-remove nil
          in1  (get-buffer-create "in1")
          in2  (get-buffer-create "in2")
          out1 (get-buffer-create "out1")
          out2 (get-buffer-create "out2"))
    (doom-set-buffer-real in1 t)
    (doom-set-buffer-real out1 t)
    (let (noninteractive)
      (persp-mode +1)
      (let (persp-before-switch-functions persp-activated-functions)
        (setq persp1-name +workspaces-main
              persp1 (persp-add-new persp1-name)
              persp2-name "test"
              persp2 (persp-add-new persp2-name))
        (persp-switch persp1-name)
        (persp-add-buffer (list in1 in2) persp1))))

  (after-each
    (let (kill-buffer-query-functions kill-buffer-hook)
      (let (noninteractive ignore-window-parameters)
        (dolist (persp (persp-names))
          (ignore-errors (persp-kill persp)))
        (persp-mode -1))
      (set-window-configuration wconf)
      (mapc #'kill-buffer (list in1 in2 out1 out2))))

  ;;
  (describe "switch"
    (it "throws an error when switching to a non-existent workspace"
      (expect (+workspace-switch "non-existent") :to-throw))
    (it "switches to a valid workspace"
      (+workspace-switch persp2-name)
      (expect (+workspace-current-name) :to-equal persp2-name)))

  (describe "current"
    (it "returns the current workspace persp"
      (expect (+workspace-p (+workspace-current)))
      (expect (+workspace-current) :to-equal (get-current-persp)))
    (it "returns the current workspace's name"
      (expect (+workspace-current-name) :to-equal persp1-name)
      (persp-switch (persp-name persp2))
      (expect (+workspace-current-name) :to-equal persp2-name)))

  (describe "exists-p"
    (it "returns t for valid workspaces"
      (expect (+workspace-exists-p persp1-name)))
    (it "returns t for non-current (but valid) workspaces"
      (expect (+workspace-exists-p persp2-name)))
    (it "returns nil for non-existent workspaces"
      (expect (+workspace-exists-p "non-existent") :to-be nil)))

  (describe "buffer membership"
    (it "returns t for buffers in current workspace"
      (expect (+workspace-contains-buffer-p in1)))
    (it "returns nil for buffers outside of current workspace"
      (expect (+workspace-contains-buffer-p out1) :to-be nil))
    (xit "returns a list of orphaned buffers"
      (expect (+workspace-orphaned-buffer-list) :to-contain out2)))

  (describe "list"
    (it "returns a list of names"
      (expect (+workspace-list-names)
              :to-have-same-items-as (list persp1-name persp2-name)))
    (it "returns a list of perspective structs"
      (expect (+workspace-list)
              :to-have-same-items-as (list persp1 persp2))))

  (describe "CRUD"
    (it "creates new workspaces"
      (+workspace-new "X")
      (expect (+workspace-list-names) :to-contain "X"))
    (it "renames an existing workspace"
      (+workspace-rename persp2-name "X")
      (expect (persp-name persp2) :to-equal "X")
      (expect (+workspace-list-names)
              :to-have-same-items-as (list persp1-name "X")))
    (it "deletes a live workspace"
      (+workspace-delete persp2-name)
      (expect (+workspace-list-names) :not :to-contain persp2-name)))

  (describe "command"
    (describe "close-window-or-workspace"
      (before-each
        (+workspace-switch persp2-name)
        (split-window)
        (expect (length (doom-visible-windows)) :to-be 2))
      (it "kills window if more than one window"
        (quiet! (+workspace/close-window-or-workspace))
        (expect (length (doom-visible-windows)) :to-be 1))
      (it "kills workspace on last window"
        (quiet! (+workspace/close-window-or-workspace)
                (+workspace/close-window-or-workspace))
        (expect (+workspace-current-name) :to-equal persp1-name)))

    (describe "rename"
      (it "renames the current workspace"
        (quiet! (+workspace/rename "X"))
        (expect (+workspace-current-name) :to-equal "X")))))
