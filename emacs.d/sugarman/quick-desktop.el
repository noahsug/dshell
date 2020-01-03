;; Path to store the various desktop directories.
(setq qd--path "~/.emacs.d/desktop/")

;; The user's current desktop, set when the user loads a new desktop.
(setq qd-current-desktop nil)

(require 'desktop)
(require 's)

(defun qd-load ()
  "Loads an existing desktop"
  (interactive)
  (let ((desktop (qd--prompt-desktop "Load desktop: ")))
    (if (qd--desktop-exists desktop) (qd--load-desktop desktop)
      (message "Desktop doesn't exist: %s" desktop))))

(defun qd-save ()
  "Save current desktop file based on your current or provided directory"
  (interactive)
  (qd--maybe-create-dir qd--path)
  (qd--save-desktop (qd--get-current-desktop)))

(defun qd-delete ()
  "Remove desktop file"
  (interactive)
  (let ((desktop (qd--prompt-desktop "Delete desktop: ")))
    (if (qd--desktop-exists desktop) (qd--delete-desktop desktop)
      (message "Desktop doesn't exist: %s" desktop))))

(defun qd--handle-command-line-arguments()
  "Adds --qd=<desktop> command line arg, 'emacs --qd=shell' loads 'shell' desktop"
  (let ((desktop-prefix (elt (s-match "--qd=\\(\\w+\\)" argi) 1))
        (completions nil)
        (desktop nil))
    (setq completions (and desktop-prefix
                           (file-name-all-completions desktop-prefix qd--path)))
    (setq desktop (s-chop-suffix "/" (elt completions 0)))
    (when desktop (qd--load-desktop desktop) t)))
(setq command-line-functions (cons 'qd--handle-command-line-arguments
                                   command-line-functions))


;;
; Private helper functions.
;;

;; Returns the current desktop if exists, else prompts for a desktop name. Uses
;; "temp" if an empty string is given.
(defun qd--get-current-desktop ()
  (if qd-current-desktop qd-current-desktop
    (let ((desktop (qd--prompt-desktop "Save desktop as: ")))
      (if (= (length desktop) 0) "temp" desktop))))

(defun qd--desktop-exists (desktop)
  (file-exists-p (concat qd--path desktop)))

(defun qd--prompt-desktop (msg)
  (completing-read
   msg
   (file-name-all-completions "" qd--path)))

(defun qd--save-desktop (desktop)
  (let ((full-path (concat qd--path desktop)))
    (qd--maybe-create-dir full-path)
    (setq qd-current-desktop desktop)
    (desktop-save full-path)
    (message "Desktop: saved to %s" full-path)))

(defun qd--load-desktop (desktop)
  ;(add-hook 'auto-save-hook 'qd-save)
  (let ((full-path (concat qd--path desktop)))
    (setq qd-current-desktop desktop)
    (desktop-change-dir full-path)))

(defun qd--delete-desktop (desktop)
  (let ((full-path (concat qd--path desktop)))
    (delete-directory full-path t)
    (message "%s has been deleted." desktop)))

;; Creates a new directory if it doesn't exist.
(defun qd--maybe-create-dir (path)
  (if (not (file-exists-p path)) (make-directory path)))

(provide 'sug-quick-desktop)
