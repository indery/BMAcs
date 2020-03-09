

(use-package swift-mode
  :hook (swift-mode . (lambda () (lsp))))

(use-package lsp-sourcekit
  :after lsp-mode
  :init
  (setq
   sourcekit-toolchain-path "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain"
   sourcekit-exec-files (concat sourcekit-toolchain-path "/usr/bin")
   lsp-sourcekit-executable (concat sourcekit-exec-files "/sourcekit-lsp")
   )

  (setenv "SOURCEKIT_TOOLCHAIN_PATH" sourcekit-toolchain-path) 
  (add-to-exec-path sourcekit-exec-files)
  (setenv "TOOLCHAINS" "swift")
  )
