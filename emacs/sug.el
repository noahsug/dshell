;;; sug.el -- My personal emacs configuration.

;;; Commentary:

;; Adds aliases, shortcuts and prelude preferences.

;;; Code:
;; general key bindings
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-x h") 'help-command)
(global-set-key (kbd "M-p") (kbd "M-5 C-p"))
(global-set-key (kbd "M-n") (kbd "M-5 C-n"))
(global-set-key (kbd "C-x p") (kbd "M-- C-x o"))
(global-set-key (kbd "C-x C-b") (kbd "C-x b C-m"))
(global-set-key (kbd "C-x b") 'switch-to-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)
(global-set-key (kbd "C-x r e") 'refresh-file)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-j") 'hippie-expand)
;(global-set-key (kbd "C-M-z") (lambda (char) (interactive "cZap to char backwards: ") (zap-to-char -1 char)))
;(global-set-key (kbd "C-M-Z") (lambda (char) (interactive "cZap up to char backwards: ") (zap-up-to-char -1 char)))
(global-set-key (kbd "C-x C-p") (kbd "C-x p"))
(global-set-key (kbd "C-x C-o") (kbd "C-x o"))
(global-set-key (kbd "C-x C-k") (kbd "C-x k"))
(global-set-key (kbd "C-x C-z") (kbd "C-x z"))

;; projectile key bindings
(global-set-key (kbd "C-c j") 'projectile-test-project)
(global-set-key (kbd "C-c C-j") 'projectile-test-project)
(global-set-key (kbd "C-x C-j") 'projectile-test-project)

(global-set-key (kbd "C-c l") 'projectile-compile-project)
(global-set-key (kbd "C-c C-l") 'projectile-compile-project)

(global-set-key (kbd "C-c a") 'projectile-ack)
(global-set-key (kbd "C-c C-a") 'projectile-ack)
(global-set-key (kbd "C-x C-a") 'projectile-ack)

(global-set-key (kbd "C-c f") 'projectile-find-file)
(global-set-key (kbd "C-c C-f") 'projectile-find-file)

(global-set-key (kbd "C-c K") 'projectile-kill-buffers)

(global-set-key (kbd "C-c R") 'projectile-replace)

(global-set-key (kbd "C-c S") 'projectile-switch-project)

;; refresh file
(defun refresh-file ()
  (interactive)
  (revert-buffer t t t)
)

;; don't word wrap
(setq toggle-truncate-lines nil)

;; allow arrow keys
(setq prelude-guru nil)

;; follow symlinks and don't ask
(setq vc-follow-symlinks t)

;; disable menu bar
(menu-bar-mode 0)

;; highlight current line
(set-face-background 'hl-line "#444")

;; jump-char
(require 'jump-char "~/.emacs.d/personal/sug/jump-char/jump-char.el")
(global-set-key [(meta m)] 'jump-char-forward)
(global-set-key [(control meta m)] 'jump-char-backward)

;; revbufs
(require 'revbufs "~/.emacs.d/personal/sug/revbufs.el")
(global-set-key (kbd "C-x r a") 'revbufs)

;; switch to corresponding test/impl file
(require 'swap-impl-test "~/.emacs.d/personal/sug/swap-impl-test.el")
(global-set-key (kbd "C-c t") 'swap-impl-test)
(global-set-key (kbd "C-c C-t") 'swap-impl-test)

;; flycheck
;(require 'flycheck "~/.emacs.d/personal/sug/flycheck.el")
;(add-hook 'after-init-hook #'global-flycheck-mode)

;; fix for "lisp nesting exceeds `max-lisp-eval-depth'" error
(setq max-lisp-eval-depth 1000)

;; fix for "variable binding depth exceeds max-specpdl-size" error
(setq max-specpdl-size 1000)

;; ido
(setq
  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
    "^\*compilation" "^\*GTAGS" "^session\.*" "^\*"))

;; color for compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; helper functions
(defun delete-line ()
  "Deletes the current line without putting it into the kill ring."
  (delete-region (line-beginning-position) (line-end-position)))

(defun un-camelcase (s &optional sep &optional start)
  "Convert CamelCase string to lower case with separator."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "_")
        (downcase (match-string 0 s)))
        t nil s)))
    (downcase s)))

;; expands a phrase to fit into a template
(defun template-expand ()
  "Expand a line into a template."
  (interactive)
  (let (phrase)
    (setq phrase
      (replace-regexp-in-string "\n" "" (thing-at-point 'line)))
    (cond

      ((string-match "^class\s\\w+" phrase)
        "Coffeescript + node.js class expansion."
        (let (className)
          (setq className (elt (split-string phrase " ") 1))
          (delete-line)
          (insert (format
              "exports.%s = class %s\n  " className className))))

      ((string-match "^req\s\\w+" phrase)
        "Coffeescript + node.js require."
        (let (className)
          (setq className (elt (split-string phrase " ") 1))
          (delete-line)
          (insert (format
              "{%s} = require \"%s.coffee\"\n" className (un-camelcase className)))))

      ((string-match "^req\s\\.\\./\\w+" phrase)
       "Coffeescript + node.js require."
       (let (className)
         (setq className
             (substring (elt (split-string phrase " ") 1) 3 nil))
         (delete-line)
         (insert (format
                  "{%s} = require \"../coffee/%s.coffee\"\n" className (un-camelcase className)))))

      (t
        (message "unknown phrase \"%s\"" phrase)))))
(global-set-key (kbd "M-j") template-expand)

(provide 'sug)
;;; sug.el ends here
