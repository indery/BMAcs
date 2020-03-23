(defvar postgres-dbs '(("nickname"
			"host"
			"dbname"
			"username"
			"password"
			)))


(defun build-psql-connect-command (hostname dbname username password)

  (format "psql --host=%s --dbname=%s --username=%s --password=%s" hostname dbname username password)
  )



(defun +postgres-get-connection-string ()
  (interactive)
  (let (
	(db-connect-data (cdr (assoc (completing-read "Which Postgres DB?" postgres-dbs) postgres-dbs)))
	)


    (build-psql-connect-command
     (eval (nth 0 db-connect-data))
     (eval (nth 1 db-connect-data))
     (eval (nth 2 db-connect-data))
     (eval (nth 3 db-connect-data))
     )
    ))

