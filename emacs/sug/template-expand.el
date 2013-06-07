;;; template-expand.el -- Expands a phrase to fit into a template

;; Copyright (c) 2013 Noah Sugarman <noahsug@gmail.com>
;;
;; Author: Noah Sugarman <noahsug@gmail.com>

;;; Commentary:

;; Given a snippet like 'class Bob', expand it to 'exports.Bob = class Bob'.

;;; Code:

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

(defun replace-line (text)
  "Replace the text in the current line with TEXT."
  (delete-line)
  (insert text))

(defun template-expand ()
  "Expand a line into a template."
  (interactive)
  (let (phrase args)
    (setq phrase
          (replace-regexp-in-string "\n" "" (thing-at-point 'line)))
    (setq args (split-string phrase " "))
    (cond

     ((string-match "^class\s\\w+" phrase)
      "Coffeescript + node.js class expansion."
      (let ((className (elt args 1)))
        (replace-line
         (format "exports.%s = class %s\n  " className className))))

     ((string-match "^req\s\\w+" phrase)
      "Coffeescript + node.js require."
      (let ((className (elt args 1)))
        (replace-line
         (format "{%s} = require \"%s.coffee\""
                 className (un-camelcase className)))))

     ((string-match "^req\s\\.\\.\\w+" phrase)
      "Coffeescript + node.js require."
      (let ((className (substring (elt args 1) 2 nil)))
        (replace-line
         (format "{%s} = require \"../coffee/%s.coffee\""
                 className (un-camelcase className)))))

     (t
      (message "unknown phrase \"%s\"" phrase)))))
(provide 'template-expand)
;;; template-expand.el ends here
