;; useful default bindings
; C-x C-h c <key-sequence> : Find out what key is bound to.
; C-x C-space : Jump to last position between buffers.
; C-x z : Repeat last command, press 'z' to keep repeating it.
; M-x describe-unbound 5 : Find all unbound keys.
; C-u C-x = : See font face at cursor (near the bottom of desc)
; M-x list-faces-display = : See list of all font faces
; C-xB -> t -> U -> S : Regex replace across all buffers (Q for normal replace)
; C-M-s : regexp search

(global-set-key "\C-x\C-b" (kbd "C-x b C-m"))
(global-set-key "\C-x\C-p" (kbd "M-- C-x o"))
(global-set-key "\C-x\C-o" (kbd "C-x o"))
(global-set-key "\C-x\C-k" (kbd "C-x k"))
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)

(global-set-key "\M-p" (kbd "M-5 C-p"))
(global-set-key "\M-n" (kbd "M-5 C-n"))
(global-set-key "\M-g" 'goto-line)

(global-set-key "\C-\M-k" 'kill-whole-line)
(global-set-key "\C-\M-o" 'duplicate-line)

(global-set-key (kbd "C-x r e") 'refresh-file)
(global-set-key "\C-xx" 'replace-regexp)

(global-set-key "\C-xW" 'write-file)

;; duplicate line
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

(defun refresh-file ()
  (interactive)
  (revert-buffer t t t)
)

;;;
;; global
;;;

(global-subword-mode 1)
(setq-default indent-tabs-mode nil)

(setq inhibit-startup-screen t)

(setq visible-bell t)

(show-paren-mode t)
(setq show-paren-delay 0.5)

(column-number-mode)

(setq case-fold-search t)

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
(delete-selection-mode t)
; Reapply it every 10 seconds (since it randomly disables itself)
(defun enable-delete-selection-mode ()
  (delete-selection-mode t))
(run-at-time "5 sec" 5 'enable-delete-selection-mode)

;; always end a file with a newline
(setq require-final-newline t)

;; yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; whitespace mode
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Allow C-x C-l to downcase-region.
(put 'downcase-region 'disabled nil)

(put 'upcase-region 'disabled nil)

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-region-up)
(global-set-key (kbd "M-<down>") 'move-region-down)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(column-number-mode t)
 '(css-indent-offset 2)
 '(delete-selection-mode t)
 '(global-whitespace-mode t)
 '(helm-buffers-fuzzy-matching t)
 '(helm-fuzzy-matching-highlight-fn (quote helm-flx-fuzzy-highlight-match))
 '(helm-fuzzy-sort-fn (quote helm-flx-fuzzy-matching-sort))
 '(helm-locate-fuzzy-match t)
 '(helm-ls-git-fuzzy-match t)
 '(ido-use-faces nil)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-highlight-level 3)
 '(whitespace-line-column 100))
