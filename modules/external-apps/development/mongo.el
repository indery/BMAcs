

(defvar mongo-clusters '(("cluster-nickname"
			  "cluster-url"
			  "cluster-username-vault-path"
			  "cluster-password-vault-path")))


(use-package inf-mongo
  :straight (:host github :repo "endofunky/inf-mongo")
  )


(defun build-mongo-connect-command (cluster-url db-name username password)

  (format 
   "/usr/local/bin/mongo mongodb+srv://%s/%s --username %s --password %s"
   cluster-url
   db-name
   username
   password
   )

  )


(defun +mongo-connect ()
  (interactive)
  (let (
	(cluster-data (cdr (assoc (completing-read "Which Mongo Cluster?" mongo-clusters) mongo-clusters)))
	)


    (inf-mongo (build-mongo-connect-command
		(car cluster-data)
		""
		(+vault-get-secret (nth 1 cluster-data))
		(+vault-get-secret (nth 2 cluster-data))
		))
    ))



