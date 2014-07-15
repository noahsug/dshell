;;; template-expand.el -- Expands a phrase to fit into a template

;; Copyright (c) 2013 Noah Sugarman <noahsug@gmail.com>
;;
;; Author: Noah Sugarman <noahsug@gmail.com>

;;; Commentary:

;; Given a snippet like 'class Bob', expand it to 'exports.Bob = class Bob'.

;;; Code:

(require 'sug-utils)

(defun template-expand ()
  "Expand a line into a template."
  (interactive)
  (let (phrase args)
    (setq phrase
          (cond
           ((equal (buffer-size) 0)
            "")
           (t
            (replace-regexp-in-string "\n" "" (thing-at-point 'line)))))
    (setq args (split-string phrase " "))
    (cond

     ((string-match "^class\s\\w+" phrase)
      "Coffeescript + node.js class expansion."
      (let ((className (elt args 1)))
        (replace-line
         (format "exports.%s = class %s\n  " className className))))

     ((string-match "^req\s\\w+" phrase)
      "CoffeeScript + node.js require."
      (let ((className (elt args 1)))
        (replace-line
         (format "{%s} = require '../coffee/%s.coffee'"
                 className (un-camelcase className)))))

     ((string-match "^\s\scons$" phrase)
      "CoffeeScript constructor"
      (replace-line "  constructor: ->\n    "))

     ((and (equal (buffer-size) 0) (has-extension "_spec.coffee"))
      "Generate CoffeeScript file test header"
      (insert (format (concat "{%s} = require '../coffee/%s.coffee'\n\n"
                              "describe '%s', ->\n"
                              "  %s = undefined\n\n"
                              "  beforeEach ->\n"
                              "    %s = new %s\n\n"
                              "  it '', ->\n    ")
                      (camelcase (get-base-file-name))
                      (get-base-file-name)
                      (camelcase (get-base-file-name))
                      (var-case (get-base-file-name))
                      (var-case (get-base-file-name))
                      (camelcase (get-base-file-name))
                      )))

     ((and (equal (buffer-size) 0) (has-extension ".coffee"))
      "Generate CoffeeScript file header"
      (insert (format "exports.%s = class %s\n  "
                      (camelcase (get-extensionless-file-name))
                      (camelcase (get-extensionless-file-name)))))

     (t
      (message "unknown phrase '%s'" phrase)))))
(provide 'template-expand)
;;; template-expand.el ends here
