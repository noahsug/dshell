;; if you can only set one variable, set this one
(setq-default indent-tabs-mode nil)

;; go. away.
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq inhibit-startup-screen t)

;; "advanced" features
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; essential when working across branches
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq visible-bell t)

;; remember where we were
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/places")
(require 'saveplace)

;; clean old buffers
(require 'midnight)
(setq clean-buffer-list-delay-general 7)
(timer-activate midnight-timer)

;; go big or go home
(setq large-file-warning-threshold 67108864)

;; move mouse cursor when it impacts text
(mouse-avoidance-mode 'exile)

;; save state on exit
(desktop-save-mode 1)
(setq desktop-globals-to-save '(desktop-missing-file-warning search-ring regexp-search-ring register-alist file-name-history))
;; save desktop periodically
(run-with-timer (* 60 60) (* 30 60) 'desktop-save (car desktop-path))

;; can't hack lisp without it
(show-paren-mode t)
(setq show-paren-delay 0.5)

(column-number-mode)

(setq recentf-save-file "~/.emacs.d/recentf")
(recentf-mode)

(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq default-abbrev-mode t)
(setq save-abbrevs 'silently)

;; make dabbrev more functional
(setq dabbrev-case-fold-search nil)

;; misc useful settings
(setq-default case-fold-search t)
(setq enable-recursive-minibuffers t)

;; live dangerously
(setq dired-recursive-deletes 'top)

(setq compilation-scroll-output t)

(when (package-installed-p 'auto-complete)
  (require 'auto-complete-config)
  (setq ac-trigger-key "TAB")
  (ac-config-default))

;; start off find-file in a convenient place
(cd (expand-file-name "~"))

;; Put autosave files (ie #foo#) and backup files (ie foo~) different folder.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))
; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; don't word wrap
(setq toggle-truncate-lines nil)

;; follow symlinks and don't ask
(setq vc-follow-symlinks t)

;; delete selection on keypress
(delete-selection-mode 1)

;; whitespace mode
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; mini-buffer color
(set-face-foreground 'minibuffer-prompt "white")

;; Advise find-file to transparently create necessary directories.
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

;; disable version control
(setq vc-handled-backends ())

;; Allow C-x C-l to downcase-region.
(put 'downcase-region 'disabled nil)

;; system specific customizations

(if (string-equal system-type "darwin")
  ;; mac os x
  (setq ns-alternate-modifier 'meta
        ns-command-modifier 'meta)

  ;; non-mac x
  (scroll-bar-mode -1))

;; Choosing not to use this since we use screen.
;(server-start)
