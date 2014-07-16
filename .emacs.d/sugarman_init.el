;; Loads everything else.
(setq uid "sugarman")
(setq elisp-root (concat "~/.emacs.d/" uid))

;; Load packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Load personalization
(dolist (library (directory-files elisp-root nil "\\.el$"))
  (load (concat elisp-root "/" (file-name-sans-extension library))))
