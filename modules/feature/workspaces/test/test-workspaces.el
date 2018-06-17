;; -*- no-byte-compile: t; -*-
;;; feature/workspaces/test/test-workspaces.el

(describe "feature/workspaces"
  :var (persp-auto-resume-time
        persp-auto-save-opt
        persp-switch-to-added-buffer
        in1 in2 out1 out2
        persp1 persp1-name persp2 persp2-name
        doom-before-switch-buffer-hook
        doom-after-switch-buffer-hook)

  (before-all
    (require! :feature workspaces)
    (require 'persp-mode))
  (after-all
    (unload-feature 'persp-mode t))

  (before-each
    (setq persp-auto-resume-time -1
          persp-auto-save-opt 0
          persp-switch-to-added-buffer nil
          in1 (get-buffer-create "a")
          in2 (get-buffer-create "b")
          out1 (get-buffer-create "c")
          out2 (get-buffer-create "d"))
    (doom-set-buffer-real in1 t)
    (doom-set-buffer-real out1 t)
    (let (noninteractive)
      (persp-mode +1))
    (let (persp-before-switch-functions persp-activated-functions)
      (setq persp1-name +workspaces-main
            persp1 (persp-add-new persp1-name)
            persp2-name "test"
            persp2 (persp-add-new persp2-name))
      (persp-frame-switch +workspaces-main))
    (delete-other-windows)
    (switch-to-buffer in1)
    (persp-add-buffer (list in1 in2))
    (spy-on 'persp-add-buffer :and-call-through)
    (doom|init-custom-hooks))

  (after-each
    (doom|init-custom-hooks 'disable)
    (let (kill-buffer-query-functions kill-buffer-hook)
      (mapc #'kill-buffer (list in1 in2 out1 out2)))
    (let (noninteractive)
      (mapc #'persp-kill (cdr (persp-names)))
      (persp-mode -1)))

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
    (it "automatically adds interactively opened buffers"
      (expect (+workspace-contains-buffer-p out1) :to-be nil)
      (switch-to-buffer out1)
      (expect (+workspace-contains-buffer-p out1)))
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
    (describe "new"
      (it "creates a new, blank workspace"
        (quiet! (+workspace/new "X"))
        (expect (one-window-p))
        (expect (current-buffer) :to-be (doom-fallback-buffer)))
      (it "clones a workspace"
        (quiet! (+workspace/new "X" t))
        (expect (current-buffer) :to-be in1)))

    (describe "rename"
      (it "renames the current workspace"
        (quiet! (+workspace/rename "X"))
        (expect (+workspace-current-name) :to-equal "X")))

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
        (expect (+workspace-current-name) :to-equal persp1-name)))))
