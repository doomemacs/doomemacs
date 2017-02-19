;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core packages
(@package anaphora)
(@package async)
(@package persistent-soft)
(@package ht)
(@package smex)

;; core-os.el
(when IS-MAC
  (@package exec-path-from-shell)
  (@package osx-clipboard))

;; core-ui.el
(@package highlight-indent-guides)
(@package highlight-numbers)
(@package nlinum)
(@package rainbow-delimiters)

;; core-popups.el
(@package shackle)

;; core-editor.el
(@package editorconfig)
(@package smartparens)
(@package ace-link)
(@package ace-window)
(@package avy)
(@package command-log-mode)
(@package emr)
(@package expand-region)
(@package goto-last-change)
(@package help-fns+)
(@package imenu-anywhere)
(@package imenu-list)
(@package pcre2el)
(@package rotate-text :recipe (:fetcher github :repo "debug-ito/rotate-text.el"))
(@package smart-forward)
(@package swiper)
(@package wgrep)

;; core-projects.el
(@package projectile)

;; core-keybinds.el
(@package which-key)
