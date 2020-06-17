

(with-eval-after-load '+hydra

  (pretty-hydra-define hydra-window-resize
    (:color teal :title "Window Resize" )

    (
     "Resize"
     (
      ("k" (lambda() (interactive) (enlarge-window 1)) "Enlarge Window" :exit nil)
      ("j" (lambda() (interactive) ( shrink-window 1)) "Shrink Window" :exit nil)

      ("C-k" (lambda() (interactive) (enlarge-window 5)) "Enlarge Window" :exit nil)
      ("C-j" (lambda() (interactive) ( shrink-window 5)) "Shrink Window" :exit nil)

      ("l" (lambda() (interactive) (enlarge-window-horizontally 1)) "Enlarge Window Horizontally" :exit nil)
      ("h" (lambda() (interactive) (shrink-window-horizontally 1)) "Shrink Window Horizontally" :exit nil)

      ("C-l" (lambda() (interactive) (enlarge-window-horizontally 5)) "Enlarge Window Horizontally" :exit nil)
      ("C-h" (lambda() (interactive) (shrink-window-horizontally 5)) "Shrink Window Horizontally" :exit nil)
      )

     "Quit"
     ((
       "q" nil "quit"
       ) 
      )

     )


    )


  )
