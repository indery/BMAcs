
;;;###autoload
(defun +eval/buffer ()
  "Evaluate the whole buffer."
  (interactive)
  (if (or (assq major-mode +eval-runners)
          (and (fboundp '+eval--ensure-in-repl-buffer)
               (ignore-errors
                 (get-buffer-window (or (+eval--ensure-in-repl-buffer)
                                        t)))))
      (+eval/region (point-min) (point-max))
    (quickrun)))

;;;###autoload
(defun +eval/region (beg end)
  "Evaluate a region between BEG and END and display the output."
  (interactive "r")
  (let ((load-file-name buffer-file-name))
    (if (and (fboundp '+eval--ensure-in-repl-buffer)
             (ignore-errors
               (get-buffer-window (or (+eval--ensure-in-repl-buffer)
                                      t))))
        (+eval/send-region-to-repl beg end)
      (if-let (runner (cdr (assq major-mode +eval-runners)))
          (funcall runner beg end)
        (quickrun-region beg end)))))

;;;###autoload
(defun +eval/line-or-region ()
  "Evaluate the current line or selected region."
  (interactive)
  (if (use-region-p)
      (call-interactively #'+eval/region)
    (+eval/region (line-beginning-position) (line-end-position))))

;;;###autoload
(defun +eval/buffer-or-region ()
  "Evaluate the whole buffer."
  (interactive)
  (call-interactively
   (if (use-region-p)
       #'+eval/region
     #'+eval/buffer)))

;;;###autoload
(defun +eval/region-and-replace (beg end)
  "Evaluation a region between BEG and END, and replace it with the result."
  (interactive "r")
  (cond ((eq major-mode 'emacs-lisp-mode)
         (kill-region beg end)
         (condition-case nil
             (prin1 (eval (read (current-kill 0)))
                    (current-buffer))
           (error (message "Invalid expression")
                  (insert (current-kill 0)))))
        ((quickrun-replace-region beg end))))
