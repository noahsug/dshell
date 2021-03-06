; To add a new macro, use 'save-macro' bound to \C-x\C-g\C-m

(fset 'macro-jest-unmock
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("from  w<jest.unmock()" 0 "%d")) arg)))

(fset 'macro-block-comment
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote (" w/** * */ " 0 "%d")) arg)))

(fset 'macro-manual-return
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("=> ( {(} 
	returnm  " 0 "%d")) arg)))

(fset 'macro-remove-manual-return
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("=> { ({) return  m" 0 "%d")) arg)))

(fset 'macro-aurelia-inject
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote (" w@injimport  from './'xto-hyphen-caseconstructor(),   w
	this. = inject()" 0 "%d")) arg)))

(fset 'macro-manual-object-return
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("}  return { 	" 0 "%d")) arg)))

(fset 'macro-remove-manual-object-return
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("()   	" 0 "%d")) arg)))

(fset 'macro-search-symbol
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("xhighlight-symbolw" 0 "%d")) arg)))

(fset 'macro-elm-annotate
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote (" w : " 0 "%d")) arg)))

(fset 'macro-elm-debug
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("(Debug.log \"\" ())\"" 0 "%d")) arg)))

(fset 'macro-elm-undebug
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("(Debug.log(debug.log ff \"" 0 "%d")) arg)))

(fset 'macro-diff-accept-theirs
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("<<<<<<< =======>>>>>>> " 0 "%d")) arg)))

(fset 'macro-diff-accept-ours
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("<<<<<<< ======= >>>>>>>" 0 "%d")) arg)))

(fset 'macro-change-log-pr
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("https://b w<(# f / /))htt[#](" 0 "%d")) arg)))
