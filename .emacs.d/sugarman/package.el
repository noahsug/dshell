(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;(when (not package-archive-contents)
;  (package-refresh-contents))
;
;(defvar default-packages '(
;                           key-chord
;                           ace-jump-mode
;                           yasnippet
;                           yasnippet-bundle
;                           coffee-mode
;                           haskell-mode
;                           sass-mode
;                           markdown-mode
;                           js-comint
;                           js2-mode
;                           protobuf-mode
;                           yaml-mode
;                           closure-lint-mode
;                           closure-template-html-mode
;                           ))
;
;(dolist (p default-packages)
;  (when (not (package-installed-p p))
;    (package-install p)))
