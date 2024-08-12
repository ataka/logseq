# logseq
Logseq Markdown mode for Emacs

# Setup

Put the following code into `.dir-locals.el` in your top directory of Logseq.

``` emacs-lisp
((nil . ((eval
          (lambda nil
            (when
                (and (not (null buffer-file-name))
                     (string-match-p "\\.md\\'" buffer-file-name)
                     (not (string= major-mode "logseq-markdown-mode")))
              (logseq-markdown-mode)))))))
```
