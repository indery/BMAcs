
(use-package magit)

(use-package evil-magit
  :after +evil
  )


(use-package forge
  :after magit
  )


(add-to-list 'display-buffer-alist
	     `("magit:"
	       (display-buffer-in-side-window)
	       (side . right)
	       (window-width . 100)
	       ))
