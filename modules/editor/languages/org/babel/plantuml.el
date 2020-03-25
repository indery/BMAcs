

(with-eval-after-load  '+plantuml
  ;; active Org-babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(;; other Babel languages
     (plantuml . t)))



  (setq org-plantuml-jar-path plantuml-jar-path)



  ) 


;; example src block
;;#+begin_src plantuml :file ticket_service_objects.utxt
;;title Authentication Sequence
;;
;;Alice->Bob: Authentication Request
;;note right of Bob: Bob thinks about it
;;Bob->Alice: Authentication Response
;;#+end_src
