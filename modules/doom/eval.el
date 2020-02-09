
(require '+doom/core)

(use-package quickrun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config.el

(defvar +eval-popup-min-lines 4
  "The output height threshold (inclusive) before output is displayed in a popup
buffer rather than an overlay on the line at point or the minibuffer.")

;; remove ellipsis when printing sexps in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level  nil)


;;
;;; Packages
(with-eval-after-load '+doom/popup
(set-popup-rule!
  (lambda (bufname _)
    (when (boundp '+eval-repl-mode)
      (buffer-local-value '+eval-repl-mode (get-buffer bufname))))
  :ttl (lambda (buf)
         (unless (plist-get +eval-repl-plist :persist)
           (when-let (process (get-buffer-process buf))
             (set-process-query-on-exit-flag process nil)
             (kill-process process)
             (kill-buffer buf))))
  :size 0.25 :quit nil))


(after! quickrun

  (setq quickrun-focus-p nil)

(after! '+doom/popup
  (set-popup-rule! "^\\*quickrun" :size 0.3 :ttl 0))

  (defadvice! +eval--quickrun-fix-evil-visual-region-a ()
    "Make `quickrun-replace-region' recognize evil visual selections."
    :override #'quickrun--outputter-replace-region
    (let ((output (buffer-substring-no-properties (point-min) (point-max))))
      (with-current-buffer quickrun--original-buffer
        (cl-destructuring-bind (beg . end)
            ;; Because `deactivate-mark', the function, was used in
            ;; `quickrun--region-command-common' instead of `deactivate-mark',
            ;; the variable, the selection is disabled by this point.
            (if (bound-and-true-p evil-local-mode)
                (cons evil-visual-beginning evil-visual-end)
              (cons (region-beginning) (region-end)))
          (delete-region beg end)
          (insert output))
        (setq quickrun-option-outputter quickrun--original-outputter))))

  (defadvice! +eval--quickrun-auto-close-a (&rest _)
    "Silently re-create the quickrun popup when re-evaluating."
    :before '(quickrun quickrun-region)
    (when-let (win (get-buffer-window quickrun--buffer-name))
      (let ((inhibit-message t))
        (quickrun--kill-running-process)
        (message ""))
      (delete-window win)))

  (add-hook! 'quickrun-after-run-hook
    (defun +eval-quickrun-shrink-window-h ()
      "Shrink the quickrun output window once code evaluation is complete."
      (when-let (win (get-buffer-window quickrun--buffer-name))
        (with-selected-window (get-buffer-window quickrun--buffer-name)
          (let ((ignore-window-parameters t))
            (shrink-window-if-larger-than-buffer)))))
    (defun +eval-quickrun-scroll-to-bof-h ()
      "Ensures window is scrolled to BOF on invocation."
      (when-let (win (get-buffer-window quickrun--buffer-name))
        (with-selected-window win
          (goto-char (point-min))))))

  ;; Display evaluation results in an overlay at the end of the current line. If
  ;; the output is more than `+eval-popup-min-lines' (4) lines long, it is
  ;; displayed in a popup.
  (defadvice! +eval--show-output-in-overlay-a (fn)
    :filter-return #'quickrun--make-sentinel
    (lambda (process event)
      (funcall fn process event)
      (with-current-buffer quickrun--buffer-name
        (when (> (buffer-size) 0)
          (+eval-display-results
           (string-trim (buffer-string))
           quickrun--original-buffer)))) 

    ;; Suppress quickrun's popup window because we're using an overlay instead.
    (defadvice! +eval--inhibit-quickrun-popup-a (buf cb)
      :override #'quickrun--pop-to-buffer
      (setq quickrun--original-buffer (current-buffer))
      (save-window-excursion
	(with-current-buffer (pop-to-buffer buf)
          (setq quickrun-option-outputter #'ignore)
          (funcall cb))))

    ;; HACK Without this, `+eval--inhibit-quickrun-popup-a' throws a
    ;;      window-live-p error because no window exists to be recentered!
    (advice-add #'quickrun--recenter :override #'ignore)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval.el

;;;###autoload
(defun +eval-display-results-in-popup (output &optional _source-buffer)
  "Display OUTPUT in a popup buffer."
  (if (with-temp-buffer
        (insert output)
        (or (>= (count-lines (point-min) (point-max))
                +eval-popup-min-lines)
            (> (string-width
                (buffer-substring (point-min)
                                  (save-excursion
                                    (goto-char (point-min))
                                    (line-end-position))))
               (window-width))))
      (let ((output-buffer (get-buffer-create "*doom eval*"))
            (origin (selected-window)))
        (with-current-buffer output-buffer
          (setq-local scroll-margin 0)
          (erase-buffer)
          (insert output)
          (goto-char (point-min))
          (if (fboundp '+word-wrap-mode)
              (+word-wrap-mode +1)
            (visual-line-mode +1)))
        (when-let (win (display-buffer output-buffer))
          (fit-window-to-buffer
           win (/ (frame-height) 2)
           nil (/ (frame-width) 2)))
        (select-window origin)
        output-buffer)
    (message "%s" output)))

;;;###autoload
(defun +eval-display-results-in-overlay (output &optional source-buffer)
  "Display OUTPUT in a floating overlay next to the cursor."
  (require 'eros)
  (let ((this-command #'+eval/buffer-or-region)
        eros-overlays-use-font-lock)
    (with-current-buffer (or source-buffer (current-buffer))
      (eros--make-result-overlay output
				 :where (line-end-position)
				 :duration eros-eval-result-duration))))

;;;###autoload
(defun +eval-display-results (output &optional source-buffer)
  "Display OUTPUT in an overlay or a popup buffer."
  (funcall (if (or current-prefix-arg
                   (with-temp-buffer
                     (insert output)
                     (or (>= (count-lines (point-min) (point-max))
                             +eval-popup-min-lines)
                         (>= (string-width
                              (buffer-substring (point-min)
                                                (save-excursion
                                                  (goto-char (point-min))
                                                  (line-end-position))))
                             (window-width))))
                   (not (require 'eros nil t)))
               #'+eval-display-results-in-popup
             #'+eval-display-results-in-overlay)
           output source-buffer)
  output)


;;
;;; Commands

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil.el

;;;###autoload (autoload '+eval:region "tools/eval/autoload/evil" nil t)
(evil-define-operator +eval:region (beg end)
  "Evaluate selection or sends it to the open REPL, if available."
  :move-point nil
  (interactive "<r>")
  (+eval/region beg end))

;;;###autoload (autoload '+eval:replace-region "tools/eval/autoload/evil" nil t)
(evil-define-operator +eval:replace-region (beg end)
  "Evaluate selection and replace it with its result."
  :move-point nil
  (interactive "<r>")
  (+eval/region-and-replace beg end))

;;;###autoload (autoload '+eval:repl "tools/eval/autoload/evil" nil t)
(evil-define-operator +eval:repl (_beg _end)
  "Open REPL and send the current selection to it."
  :move-point nil
  (interactive "<r>")
  (+eval/open-repl-other-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; repl.el

(defvar +eval-repl-buffers (make-hash-table :test 'equal)
  "The buffer of the last open repl.")

(defvar-local +eval-repl-plist nil)

(define-minor-mode +eval-repl-mode
  "A minor mode for REPL buffers.")

(defun +eval--ensure-in-repl-buffer (&optional fn plist displayfn)
  (maphash (lambda (key buffer)
             (unless (buffer-live-p buffer)
               (remhash key +eval-repl-buffers)))
           +eval-repl-buffers)
  (let* ((project-root (doom-project-root))
         (key (cons major-mode project-root))
         (buffer (gethash key +eval-repl-buffers)))
    (cl-check-type buffer (or buffer null))
    (unless (or (eq buffer (current-buffer))
                (null fn))
      (setq buffer
            (funcall (or displayfn #'get-buffer-create)
                     (if (buffer-live-p buffer)
                         buffer
                       (setq buffer
                             (save-window-excursion
                               (if (commandp fn)
                                   (call-interactively fn)
                                 (funcall fn))))
                       (cond ((null buffer)
                              (error "REPL handler %S couldn't open the REPL buffer" fn))
                             ((not (bufferp buffer))
                              (error "REPL handler %S failed to return a buffer" fn)))
                       (with-current-buffer buffer
                         (when plist
                           (setq +eval-repl-plist plist))
                         (+eval-repl-mode +1))
                       (puthash key buffer +eval-repl-buffers)
                       buffer))))
    (when (bufferp buffer)
      (with-current-buffer buffer
        (unless (or (derived-mode-p 'term-mode)
                    (eq (current-local-map) (bound-and-true-p term-raw-map)))
          (goto-char (if (and (derived-mode-p 'comint-mode)
                              (cdr comint-last-prompt))
                         (cdr comint-last-prompt)
                       (point-max)))))
      buffer)))

(defun +eval-open-repl (prompt-p &optional displayfn)
  (cl-destructuring-bind (_mode fn . plist)
      (or (assq major-mode +eval-repls)
          (list))
    (when (or (not fn) prompt-p)
      (let* ((choices (or (cl-loop for sym being the symbols
                                   for sym-name = (symbol-name sym)
                                   if (string-match "^\\(?:\\+\\)?\\([^/]+\\)/open-\\(?:\\(.+\\)-\\)?repl$" sym-name)
                                   collect
                                   (format "%s (%s)"
                                           (match-string-no-properties 1 sym-name)
                                           (or (match-string-no-properties 2 sym-name) "default")))
                          (user-error "There are no known available REPLs")))
             (choice (or (completing-read "Open a REPL for: " choices)
                         (user-error "Aborting")))
             (choice-split (split-string choice " " t))
             (module (car choice-split))
             (repl (substring (cadr choice-split) 1 -1)))
        (setq fn
              (intern-soft
               (format "+%s/open-%srepl" module
                       (if (string= repl "default")
                           ""
                         repl))))))
    (let ((region (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning)
                                                      (region-end)))))
      (unless (commandp fn)
        (error "Couldn't find a valid REPL for %s" major-mode))
      (with-current-buffer (+eval--ensure-in-repl-buffer fn plist displayfn)
        (when (bound-and-true-p evil-mode)
          (call-interactively #'evil-append-line))
        (when region
          (insert region))
        t))))


;;
;;; Commands

;;;###autoload
(defun +eval/open-repl-same-window (&optional arg)
  "Opens (or reopens) the REPL associated with the current major-mode and place
the cursor at the prompt.

If ARG (universal argument), prompt for a specific REPL to open."
  (interactive "P")
  (+eval-open-repl arg #'switch-to-buffer))

;;;###autoload
(defun +eval/open-repl-other-window (&optional arg)
  "Does `+eval/open-repl', but in a popup window.

If ARG (universal argument), prompt for a specific REPL to open."
  (interactive "P")
  (+eval-open-repl arg #'pop-to-buffer))

;;;###autoload
(defun +eval/send-region-to-repl (beg end &optional inhibit-auto-execute-p)
  "Execute the selected region in the REPL.
Opens a REPL if one isn't already open. If AUTO-EXECUTE-P, then execute it
immediately after."
  (interactive "rP")
  (let ((selection (buffer-substring-no-properties beg end))
        (buffer (+eval--ensure-in-repl-buffer)))
    (unless buffer
      (error "No REPL open"))
    (let ((origin-window (selected-window))
          (selection
           (with-temp-buffer
             (insert selection)
             (goto-char (point-min))
             (when (> (skip-chars-forward "\n") 0)
               (delete-region (point-min) (point)))
             (indent-rigidly (point) (point-max)
                             (- (skip-chars-forward " \t")))
             (concat (string-trim-right (buffer-string))
                     "\n"))))
      (with-selected-window (get-buffer-window buffer)
        (with-current-buffer buffer
          (dolist (line (split-string selection "\n"))
            (insert line)
            (if inhibit-auto-execute-p
                (insert "\n")
              ;; `comint-send-input' isn't enough because some REPLs may not use
              ;; comint, so just emulate the keypress.
              (execute-kbd-macro (kbd "RET")))
            (sit-for 0.001)
            (redisplay 'force)))
        (when (and (eq origin-window (selected-window))
                   (bound-and-true-p evil-local-mode))
          (call-interactively #'evil-append-line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings.el

;;
;; REPLs

(defvar +eval-repls nil
  "An alist mapping major modes to plists that describe REPLs. Used by
`+eval/open-repl-other-window' and filled with the `:repl' setting.")

;;;###autodef
(defun set-repl-handler! (modes command &rest plist)
  "Defines a REPL for MODES.

MODES is either a single major mode symbol or a list of them. COMMAND is a
function that creates and returns the REPL buffer.

COMMAND can either be a function that takes no arguments, or an interactive
command that will be called interactively. COMMANDS must return either the repl
buffer or a function that takes no arguments and returns the repl buffer.

PLIST is a property list that map special attributes to this repl. These are
recognized:

  :persist BOOL
    If non-nil, this REPL won't be killed when its window is closed."
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (setf (alist-get mode +eval-repls)
          (cons command plist))))


;;
;; Evaluation

;;;###autoload
(defvar +eval-runners nil
  "Alist mapping major modes to interactive runner functions.")

;;;###autodef
(defun set-eval-handler! (modes command)
  "Define a code evaluator for major mode MODES with `quickrun'.

MODES can be list of major mode symbols, or a single one.

1. If MODE is a string and COMMAND is the string, MODE is a file regexp and
   COMMAND is a string key for an entry in `quickrun-file-alist'.
2. If MODE is not a string and COMMAND is a string, MODE is a major-mode symbol
   and COMMAND is a key (for `quickrun--language-alist'), and will be registered
   in `quickrun--major-mode-alist'.
3. If MODE is not a string and COMMAND is an alist, see `quickrun-add-command':
   (quickrun-add-command MODE COMMAND :mode MODE).
4. If MODE is not a string and COMMANd is a symbol, add it to
   `+eval-runners', which is used by `+eval/region'."
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (cond ((symbolp command)
           (push (cons mode command) +eval-runners))
          ((stringp command)
           (after! quickrun
             (push (cons mode command)
                   (if (stringp mode)
                       quickrun-file-alist
                     quickrun--major-mode-alist))))
          ((listp command)
           (after! quickrun
             (quickrun-add-command
              (or (cdr (assq mode quickrun--major-mode-alist))
                  (string-remove-suffix "-mode" (symbol-name mode)))
              command :mode mode))))))


(provide '+eval.el)
