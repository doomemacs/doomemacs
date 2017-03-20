;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! cmake-mode)
(package! cuda-mode)
(package! demangle-mode)
(package! disaster)
(package! glsl-mode)
(package! irony)
(package! irony-eldoc)
(package! opencl-mode)
(package! modern-cpp-font-lock)

(when (featurep! :feature syntax-checker)
  (package! flycheck-irony))

(when (featurep! :completion company)
  (package! company-irony)
  (package! company-irony-c-headers))

;;
(def-bootstrap! cc
  ;; NOTE Untested
  (require! :lang cc t)
  (require 'irony)
  (unless (expand-file-name "bin/irony-server" irony-server-install-prefix)
    (pcase (doom-system-os)
      ('arch
       (let (pkgs)
         (unless (executable-find "cmake")
           (push "cmake" pkgs))
         (unless (file-exists-p "/usr/lib/libclang.so")
           (push "clang" pkgs))
         (when pkgs
           (sudo "pacman --noconfirm -S %s" pkgs))))
      ('debian) ;; TODO
      ('macos
       (unless (executable-find "cmake")
         (sh "brew install cmake"))
       (unless (file-exists-p "/usr/loacl/opt/llvm/lib/libclang.dylib")
         ;; Since installing llvm is a _huge_ undertaking for homebrew, we force
         ;; you to install it manually.
         (error "libclang.so not found. Run `brew install llvm` first, then retry"))))
    (let ((default-directory (concat (file-name-as-directory temporary-file-directory)
                                     (file-name-as-directory (format "build-irony-server-%s" (irony-version))))))
      (irony-install-server
       (format (concat "%s "
                       (when IS-MAC "-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON ")
                       "-DCMAKE_INSTALL_PREFIX='%s' "
                       "%s && %s --build . "
                       "--use-stderr --config Release --target install")
               (shell-quote-argument irony-cmake-executable)
               (shell-quote-argument (expand-file-name irony-server-install-prefix))
               (shell-quote-argument irony-server-source-dir)
               (shell-quote-argument irony-cmake-executable)))
      (when IS-MAC
        (sh (format "install_name_tool -change @rpath/libclang.dylib %s %s"
                    "/usr/local/opt/llvm/lib/libclang.dylib"
                    (expand-file-name "bin/irony-server" irony-server-install-prefix)))))
    t))
