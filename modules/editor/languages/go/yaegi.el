(defvar yaegi-executable-file-path "/Users/jeff/go/bin/yaegi"
  "Path to the program used by `run-yaegi'")

(defvar yaegi-executable-arguments '()
  "Commandline arguments to pass to `yaegi-cli'")

(defvar yaegi-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-yaegi'")

(defvar yaegi-prompt-regexp "^>"
  "Prompt for `run-yaegi'.")


(defun run-yaegi ()
  (interactive)
  (let* ((yaegi-program yaegi-executable-file-path)
	 (buffer (comint-check-proc "Yaegi")))


    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'yaegi-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*Yaegi*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "Yaegi" buffer
             yaegi-program yaegi-executable-arguments)
      (yaegi-mode)
      )

    )

  )


(defun yaegi--initialize ()
  "Helper function to initialize yaegi"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode yaegi-mode comint-mode "Yaegi"
  "Major mode for `run-yaegi'.

\\<yaegi-mode-map>"
  nil "Yaegi"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp yaegi-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(yaegi-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) yaegi-prompt-regexp))
