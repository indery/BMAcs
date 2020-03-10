(defun add-to-exec-path (&rest items)
  (progn
    (setq exec-path (append items exec-path ) ) 
    (setenv "PATH" (string-join exec-path ":") )))

(add-to-exec-path "/usr/local/opt/bin") 
(add-to-exec-path "/usr/local/bin") 

;; setup global directory config locations
(setq
 home-dir (getenv "$HOME")
 dotfiles-root "~/.config/"
 emacs-config-root "~/.config/emacs/"
 modules-root (concat emacs-config-root "modules/")
 org-root "~/org"
 package-repos-root "~/.config/emacs/straight/repos/" 
 projects-root (concat org-root "/projects")
 core-lisp-files-root "/usr/local/Cellar/emacs-plus/26.3/share/emacs/26.3/lisp/"
 )

(setq floobits-workspaces '( "GlympseHackNight" )) 
(setq floobits-username "parsoj")

(provide '+env)
