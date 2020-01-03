(when (package-installed-p 'yasnippet)
  (let ((snippets "~/.emacs.d/snippets"))
    (add-to-list 'load-path snippets)
    (setq yas-snippet-dirs (make-list 1 snippets)))

  (yas-global-mode 1)
  (setq yas-triggers-in-field t)
  ; (define-key yas-minor-mode-map (kbd "M-o") 'yas-expand)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ; Must be defined in yas-minor-mode-map for (yas-expand) to work.
  (define-key yas-minor-mode-map (kbd "M-o") 'smart-key)

  (setq yas/prompt-functions '(yas/ido-prompt
                               yas/dropdown-prompt
                               yas/completing-prompt
                               yas/x-prompt
                               yas/no-prompt)))
