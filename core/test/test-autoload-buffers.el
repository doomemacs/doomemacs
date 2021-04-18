;; -*- no-byte-compile: t; -*-
;;; core/test/test-autoload-buffers.el

(describe "core/autoload/buffers"
  :var (a b c d)

  (require 'core-projects)
  (load! "autoload/buffers" doom-core-dir)

  (before-each
    (delete-other-windows)
    (setq a (switch-to-buffer (get-buffer-create "a"))
          b (get-buffer-create "b")
          c (get-buffer-create "c")
          d (get-buffer-create "d")))
  (after-each
    (kill-buffer a)
    (kill-buffer b)
    (kill-buffer c)
    (kill-buffer d))

  (describe "buffer lists"
    (describe "doom-buffer-list"
      (it "should only see four buffers"
        (expect (doom-buffer-list) :to-contain-items (list a b c d)))))

  ;; TODO predicate tests
  (xdescribe "predicate functions"
    (describe "doom-dired-buffer-p")
    (describe "doom-special-buffer-p")
    (describe "doom-temp-buffer-p")
    (describe "doom-visible-buffer-p")
    (describe "doom-buried-buffer-p")
    (describe "doom-non-file-visiting-buffer-p")
    (describe "doom-dired-buffer-p")
    (describe "doom-buffer-frame-predicate"))

  (describe "doom-project-buffer-list"
    :var (projectile-projects-cache-time projectile-projects-cache)
    (before-all (require 'projectile))
    (after-all  (unload-feature 'projectile t))

    (before-each
      (with-current-buffer a (setq default-directory doom-emacs-dir))
      (with-current-buffer b (setq default-directory doom-core-dir))
      (with-current-buffer c (setq default-directory "/tmp/"))
      (with-current-buffer d (setq default-directory "~"))
      (projectile-mode +1))
    (after-each
      (projectile-mode -1))

    (it "returns buffers in the same project"
      (with-current-buffer a
        (expect (doom-project-buffer-list)
                :to-contain-items (list a b))))

    (it "returns all buffers if not in a project"
      (with-current-buffer c
        (expect (doom-project-buffer-list)
                :to-have-same-items-as (buffer-list)))))

  (describe "doom-fallback-buffer"
    (it "returns a live buffer"
      (expect (buffer-live-p (doom-fallback-buffer))))

    (it "returns the scratch buffer"
      (expect (doom-fallback-buffer) :to-equal (get-buffer "*scratch*"))))

  (describe "real buffers"
    (before-each
      (with-current-buffer b (setq buffer-file-name "x"))
      (with-current-buffer c (rename-buffer "*C*")))

    (describe "doom-mark-buffer-as-real-h"
      (with-current-buffer a
        (doom-mark-buffer-as-real-h)
        (expect (buffer-local-value 'doom-real-buffer-p a))))

    (describe "doom-set-buffer-real"
      (it "sets `doom-real-buffer-p' buffer-locally"
        (doom-set-buffer-real a t)
        (expect (buffer-local-value 'doom-real-buffer-p a))))

    (describe "doom-real-buffer-p"
      (it "returns t for buffers manually marked real"
        (doom-set-buffer-real a t)
        (expect (doom-real-buffer-p a)))
      (it "returns t for file-visiting buffers"
        (expect (doom-real-buffer-p b)))
      (it "returns nil for temporary buffers"
        (expect (doom-real-buffer-p c) :to-be nil)
        (expect (doom-real-buffer-p d) :to-be nil)))

    (describe "doom-unreal-buffer-p"
      (it "returns t for unreal buffers"
        (expect (doom-unreal-buffer-p c))
        (expect (doom-unreal-buffer-p d)))
      (it "returns nil for real buffers"
        (doom-set-buffer-real a t)
        (expect (not (doom-unreal-buffer-p a)))
        (expect (not (doom-unreal-buffer-p b)))))

    (describe "doom-real-buffer-list"
      (it "returns only real buffers"
        (expect (doom-real-buffer-list) :to-contain-items (list a b)))))

  (describe "buffer/window management"
    (describe "buffer search methods"
      (before-each
        (with-current-buffer a (lisp-mode))
        (with-current-buffer b (text-mode))
        (with-current-buffer c (text-mode))
        (split-window)
        (switch-to-buffer b))

      (describe "doom-matching-buffers"
        (it "can match buffers by regexp"
          (expect (doom-matching-buffers "^[ac]$") :to-have-same-items-as (list a c))))

      (describe "doom-buffers-in-mode"
        (it "can match buffers by major-mode"
          (expect (doom-buffers-in-mode 'text-mode) :to-have-same-items-as (list b c))))

      (describe "doom-buried-buffers"
        (it "can find all buried buffers"
          (expect (doom-buried-buffers) :to-contain-items (list c d))))

      (describe "doom-visible-buffers"
        (it "can find all visible buffers"
          (expect (doom-visible-buffers)
                  :to-have-same-items-as (list a b))))

      (describe "doom-visible-windows"
        (it "can find all visible windows"
          (expect (doom-visible-windows)
                  :to-have-same-items-as
                  (mapcar #'get-buffer-window (list a b))))))

    (describe "killing buffers/windows"
      (describe "doom-kill-buffer-and-windows"
        (before-each
          (split-window) (switch-to-buffer b)
          (split-window) (switch-to-buffer a))

        (it "kills the selected buffers and all its windows"
          (doom-kill-buffer-and-windows a)
          (expect (buffer-live-p a) :to-be nil)
          (expect (length (doom-visible-windows)) :to-be 1)))

      ;; TODO
      (xdescribe "doom-fixup-windows")
      (xdescribe "doom-kill-buffer-fixup-windows")
      (xdescribe "doom-kill-buffers-fixup-windows"))

    (xdescribe "commands"
      (describe "doom/kill-all-buffers")
      (describe "doom/kill-other-buffers")
      (describe "doom/kill-matching-buffers")
      (describe "doom/kill-buried-buffers")
      (describe "doom/kill-project-buffers"))))
