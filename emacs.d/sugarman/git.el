(defun run-shell-cmd(cmd)
  (interactive)
  (switch-to-buffer-other-window "*shell*")
  (erase-buffer)
  (shell-command cmd (get-buffer-process (shell)))
  (other-window 1)
)

; git diff master
(defun g-dm ()
  (interactive)
  (run-shell-cmd (concat "git diff master " (buffer-file-name)))
)

; git show
(defun g-show ()
  (interactive)
  (run-shell-cmd (concat "git show " (buffer-file-name)))
)
