

(defvar imessage-disable-service-check t "Sometimes a message won't send if the service is passed in, disable this if you run into issues.")

(defun imessage-call-osascript (cmd)
  "Simple wrapper around the whole OSAscript call that gets made almost everywhere."
  (setq result "")
  (setq imessage-cmd (format "osascript -e 'tell application \"Messages\" %s'" cmd))
  (message "OSAScript cmd: %s" imessage-cmd)
  (setq result (shell-command-to-string imessage-cmd))
  (message result))

(defun imessage-get-buddies-names ()
  "apple script to get buddy names"
  (imessage-parse-blob (imessage-call-osascript "to get name of buddies")))

(defun imessage-get-service-buddy (buddy)
  "apples script to get the service of the buddy"
  (unless imessage-disable-service-check
    (replace-regexp-in-string "\n\\'" "" (imessage-call-osascript (format "to get name of service of buddy %S" buddy)))))

(defun imessage-send-buddy-message (buddy msg)
  "send message to the buddy and their service"
  (let (service (imessage-get-service-buddy buddy))
    (setq imessage-cmd (format "to send %S to buddy %S" msg buddy))
    (if service
	(setq imessage-cmd (format " of service %S" service)))
    (imessage-call-osascript imessage-cmd)))



(defun imessage-parse-blob (blob)
  "Split the buddies returning list"
  (split-string blob ","))

(defun imessage-s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (s-trim-left (s-trim-right s)))



