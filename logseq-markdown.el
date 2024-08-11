;;; -*- lexical-binding: t -*-
;; Author: Masayuki Ataka <masayuki.ataka@gmail.com>
;;
;; Put the following code into .dir-locals.el in your top directory of Logseq.
;;
;; ((nil (eval
;;        (lambda ()
;;          (when (and (not (null buffer-file-name))
;;                     (string-match-p "\\.md\\'" buffer-file-name)
;;                     (not (string= major-mode "logseq-markdown-mode")))
;;            (logseq-markdown-mode))))))
;;

(provide 'logseq-markdown)
