
(with-eval-after-load 'doom/keybinds

  (map!
   :leader
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

   (:prefix ("p" . "project")

     :desc "Find file in project"         "f" #'counsel-projectile-find-file
     :desc "Switch project"               "p" #'counsel-projectile-switch-project
     :desc "Run project"                  "R" #'projectile-run-project


     :desc "Invalidate project cache"     "i" #'projectile-invalidate-cache

     :desc "Find recent project files"    "r" #'projectile-recentf
     :desc "Save project files"           "s" #'projectile-save-project-buffers
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

   ))
(map!
 :leader
 (:prefix ("t" . "Test Binds")
   :desc "test a" "a" '()

   ))
