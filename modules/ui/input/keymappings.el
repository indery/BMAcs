(with-eval-after-load 'doom/keymapper
  (with-eval-after-load '+evil
    (map!
     :leader

     :desc "M-x"                   "SPC"    #'counsel-M-x
     :desc "configuration actions" "c" #'hydra-config-actions/body
     :desc "swiper"  "/"   #'swiper

     :desc "open eshell"    "'" '+eshell-pop-window	   
     
     (:prefix ("b" . "buffer")
					; :desc "Toggle narrowing"            "-"   #'doom/toggle-narrow-buffer
       :desc "Switch buffer"               "b"   #'switch-to-buffer
       :desc "Kill buffer"                 "d"   #'kill-current-buffer
       :desc "ibuffer"                     "i"   #'ibuffer
       :desc "Kill buffer"                 "k"   #'kill-current-buffer
       :desc "Kill all buffers"            "K"   #'doom/kill-all-buffers
       :desc "Switch to last buffer"       "l"   #'evil-switch-to-windows-last-buffer
					;   :desc "Set bookmark"                "m"   #'bookmark-set
					;   :desc "Delete bookmark"             "M"   #'bookmark-delete
       :desc "Next buffer"                 "n"   #'next-buffer
					;   :desc "New empty buffer"            "N"   #'evil-buffer-new
					;   :desc "Kill other buffers"          "O"   #'doom/kill-other-buffers
       :desc "Previous buffer"             "p"   #'previous-buffer
       :desc "Revert buffer"               "r"   #'revert-buffer
       :desc "Save buffer"                 "s"   #'basic-save-buffer
       :desc "Save all buffers"            "S"   #'evil-write-all
       :desc "Save buffer as root"         "u"   #'doom/sudo-save-buffer
       :desc "Pop up scratch buffer"       "x"   #'doom/open-scratch-buffer
       :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer
       :desc "Bury buffer"                 "z"   #'bury-buffer
       :desc "Kill buried buffers"         "Z"   #'doom/kill-buried-buffers)

     (:prefix ("g" . "git/revision-control")
       :desc "Git Status"     "s"    #'magit-status
       :desc "Git Blame"     "b"    #'magit-blame

       )

     (:prefix ("p" . "project")
       :desc "Find file in project"         "f" #'counsel-projectile-find-file
       :desc "Switch project"               "p" #'counsel-projectile-switch-project
       :desc "Run project"                  "R" #'projectile-run-project
       :desc "Invalidate project cache"     "i" #'projectile-invalidate-cache
       :desc "Find recent project files"    "r" #'projectile-recentf
       :desc "Save project files"           "S" #'projectile-save-project-buffers
       :desc "ripgrep in project"           "s" #'counsel-projectile-rg
       :desc "Configure project"            "g" #'projectile-configure-project
       :desc "Edit project .dir-locals"     "e" #'projectile-edit-dir-locals
       :desc "Find other file"              "o" #'projectile-find-other-file
       :desc "Open project in treemacs"     "t" #'+treemacs-open-current-project
       )

     (:prefix ("f" . "file")
       :desc "Delete current buffer file" "d" '+delete-file-and-buffer
       :desc "Rename current buffer file" "r" '+rename-file-and-buffer
       :desc "Find file"                   "f"   #'find-file
       :desc "Save file"                   "s"   #'save-buffer
       )

     (:prefix ("t" . "toggle")
       :desc "line wrapping"               "l" #'toggle-truncate-lines
       :desc "line numbers" "n" 'display-line-numbers-mode
       :desc "treemacs" "t" 'treemacs
       )

     (:prefix ("s" . "search")
       :desc "search current directory" "d" #'(lambda () (interactive) (counsel-rg nil default-directory))
       )

     (:prefix ("w" . "window")

       :desc "delete window" "d" 'delete-window
       :desc "ace window" "w"  'ace-window
       :desc "ace swap window" "x" 'ace-swap-window
       :desc "window undo" "u" 'winner-undo
       :desc "window undo" "r" 'winner-redo
       :desc "delete other windows" "m" 'delete-other-windows
       :desc "split-window-vertically" "s" 'split-window-vertically
       :desc "split-window-horizontally" "v" 'split-window-horizontally
       :desc "focus window Left" "h" 'evil-window-left
       :desc "focus window right" "l" 'evil-window-right
       :desc "focus window up" "k" 'evil-window-up
       :desc "focus window down" "j" 'evil-window-down
       :desc "balance windows" "b" 'balance-windows
       )

     (:prefix ("e" . "eval")
       :desc "eval last sexp" "e" 'eval-last-sexp
       :desc "eval last sexp and print" "l" 'eval-print-last-sexp
       :desc "eval buffer" "b"    'eval-buffer
       :desc "eval line" "l"      'eval-line
       :desc "eval region" "r"    'eval-region

       )

     )

    ))
