(defun +vault-refresh-token ()
  (interactive)

  (let-alist (request-response-data (request
				      (format "%s/v1/auth/github/login" (getenv "VAULT_ADDR")) 
				      :data (json-encode `(("token" . ,(getenv "GITHUB_TOKEN")))) 
				      :sync t
				      :parser 'json-read
				      
				      )

				    ) 

    (setenv "VAULT_TOKEN" .auth.client_token)
    )

  )


(defun +vault-get-secret (secret-path)

  (let-alist (request-response-data (request
				      (format "%s/v1/%s" (getenv "VAULT_ADDR") secret-path) 
				      :type "GET"
				      :headers `(("X-Vault-Token" . ,(getenv "VAULT_TOKEN")))
				      :sync t
				      :parser 'json-read
				      )

				    )
    .data.value 

    )

  )
