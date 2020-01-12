

(dap-debug
 (list
  :type "python"
  :module "flask"
  :justMyCode :json-false
  :debugOptions ["DebugStdLib" "ShowReturnValue" "RedirectOutput" ]
  :environment-variables '(
			   ("FLASK_APP" . "./src/api_lambda_handler.py")
			   ("FLASK_ENV" . "development")
			   ("FLASK_DEBUG" . "0")
			   )
  :request "launch"
  :name "Python :: Run Configuration"
  :program nil
  :args (concat 
	 "run"
	 " --no-debugger"
	 " --no-reload"
	 ))

 ) 


(request
  "http://localhost:5000/v1/store_types"
  :type "GET"
  )
