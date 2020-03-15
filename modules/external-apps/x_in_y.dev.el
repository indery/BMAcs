

(defun xiny ()
  (interactive)
  (let ((lang-filename (read-string "Grab X-in-Y file: ")))
    (browse-url-emacs
     (format
      "https://learnxinyminutes.com/docs/files/learn%s"
      lang-filename
      ))
    )


  )





