Correct the misspelled word with `flyspell' in popup menu.

Usage
=====

To use, put your cursor on or after the misspelled word and call
`flyspell-popup-correct'. You can of course bind it to a key as well by adding
this to your Emacs initialization file, e.g. ~/.emacs.d/init.el:

  (define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)
