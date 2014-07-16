(defun get-current-provide-string ()
  "Returns the first goog.provide() string in the current buffer, or nil if not found."
  (save-excursion
    (beginning-of-buffer)
      (when (re-search-forward "goog\.provide *( *[\"']\\([A-Za-z0-9.]+\\)[\"'] *)" nil t)
        (match-string-no-properties 1))))

(provide 'closure-snippets-support)
