
;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))

(setq org-plantuml-jar-path
      (expand-file-name "/usr/local/Cellar/plantuml/1.2020.0/libexec/plantuml.jar"))
