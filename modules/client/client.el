
(defun eshell-client-frame ()
  (let ((default-directory home-dir))
    (progn
      (x-focus-frame nil)
      (+eshell-switch-to-window) 
      )
    )  
  ) 

(defun shell-client-frame ()
  (let ((default-directory home-dir))
    (progn
      (x-focus-frame nil)
      (switch-to-buffer (shell-get-buffer-create))
      )
    )  
  ) 
