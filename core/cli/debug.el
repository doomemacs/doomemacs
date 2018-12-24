;;; core/cli/debug.el -*- lexical-binding: t; -*-

(dispatcher! info (doom/info)
  "Output system info in markdown for bug reports.")

(dispatcher! (version v) (doom/version)
  "Reports the version of Doom and Emacs.")
