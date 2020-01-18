(defun add-to-exec-path (&rest items)
  (progn
    (setq exec-path (append items exec-path ) ) 
    (setenv "PATH" (string-join exec-path ":") )))

(add-to-exec-path "/usr/local/opt/bin") 
(add-to-exec-path "/usr/local/bin") 


;; setup global directory config locations
(setq
 home-dir (getenv "$HOME")
 emacs-config-root "~/.config/emacs/"
 modules-root (concat emacs-config-root "modules/")
 package-repos-root (concat emacs-config-root "straight/repos/")
 dotfiles-config-root "~/Workspace/dotfiles/")

(provide '+env)
