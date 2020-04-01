(load-theme 'base16-default-dark t)

; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; "advanced" features
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; essential when working across branches
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

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
;(desktop-save-mode 1)
;(setq desktop-globals-to-save '(desktop-missing-file-warning search-ring regexp-search-ring register-alist file-name-history))
;; save desktop periodically
;(run-with-timer (* 60 60) (* 30 60) 'desktop-save (car desktop-path))

;(setq recentf-save-file "~/.emacs.d/recentf")
;(recentf-mode)

(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq default-abbrev-mode t)
(setq save-abbrevs 'silently)

;; make dabbrev more functional
(setq dabbrev-case-fold-search nil)

;; misc useful settings
(setq enable-recursive-minibuffers t)

;; live dangerously
(setq dired-recursive-deletes 'top)

(setq compilation-scroll-output t)

;; Company Mode
(global-company-mode t)
; Number the candidates (use M-1, M-2 etc to select completions).
;(setq company-show-numbers t)
; Trigger completion immediately.
(setq company-idle-delay 0.15)
; Use the tab-and-go frontend.
; Allows TAB to select and complete at the same time.
(company-tng-configure-default)
; (add-to-list 'company-backends 'company-dabbrev)
; (setq company-dabbrev-downcase nil)
(setq company-frontends '(
  company-tng-frontend
  company-preview-frontend
  ;company-pseudo-tooltip-frontend
  ;company-pseudo-tooltip-unless-just-one-frontend
  ;company-echo-metadata-frontend
  ;company-quickhelp-frontend
))

;; deep tabnine
(use-package company-tabnine :ensure t)
(add-to-list 'company-backends #'company-tabnine)

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

;; color for compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

; Flyspell mode
;(require 'flyspell)
;(flyspell-mode 0)

;; ido-mode
;(setq ido-enable-flex-matching t)
;(setq ido-everywhere t)
;(ido-mode 1)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; projectile
; (projectile-global-mode)
(setq projectile-completion-system 'ivy)

;; Ivy
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

;; system specific customizations

(if (string-equal system-type "darwin")
  ;; mac os x
  (setq ns-alternate-modifier 'meta
        ns-command-modifier 'meta)

  ;; non-mac x
  (scroll-bar-mode -1))

;; Helm
(helm-flx-mode +1)

(setq helm-codesearch-global-csearchindex "~/.csearchindex")

(tool-bar-mode -1)
(menu-bar-mode -1)

;; Choosing not to use this since we use screen.
;(server-start)
