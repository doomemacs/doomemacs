;;; tools/macos/config.el -*- lexical-binding: t; -*-

(after! auth-source
  (when IS-MAC
    (pushnew! auth-sources 'macos-keychain-internet 'macos-keychain-generic)))
