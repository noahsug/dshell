(defun smart-key ()
  (interactive)
  (setq m-symbol (symbol-at-point-no-properties))
  (setq m-line (buffer-substring-no-properties
                (line-beginning-position nil) (line-end-position)))
  (save-excursion
    (forward-line 1)
    (setq m-line-2 (buffer-substring-no-properties
                (line-beginning-position nil) (line-end-position))))
  (setq m-extension (buffer-extension))

  (cond
   ((and (string= "elm" m-extension) (s-empty? m-line) (s-suffix? " =" m-line-2))
    (macro-elm-annotate))

   ((and (string= "elm" m-extension) mark-active)
    (macro-elm-debug))

   ((and (string= "elm" m-extension) (s-matches? "(Debug.log \"" m-line))
    (macro-elm-undebug))

   ((s-prefix? "@inject" m-line)
    (macro-aurelia-inject))

   ((s-suffix? "=> ({" m-line)
    (macro-manual-object-return))

   ((and (s-suffix? "=> {" m-line) (s-suffix? "return {" m-line-2))
    (macro-remove-manual-object-return))

   ((s-suffix? "=> (" m-line)
    (macro-manual-return))

   ((s-suffix? "=> {" m-line)
    (macro-remove-manual-return))

   ((s-empty? m-line)
    (macro-block-comment))

   (t (yas-expand)))
  )
