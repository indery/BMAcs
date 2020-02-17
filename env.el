(defun add-to-exec-path (&rest items)
  (progn
    (setq exec-path (append items exec-path ) ) 
    (setenv "PATH" (string-join exec-path ":") )))

(add-to-exec-path "/usr/local/opt/bin") 
(add-to-exec-path "/usr/local/bin") 

;; setup global directory config locations
(setq
 home-dir (getenv "$HOME")
 system-config-root "~/Workspace/dotfiles/"
 org-root "~/org"
 projects-root (concat org-root "/projects"))

(provide '+env)
