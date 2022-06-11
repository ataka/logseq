;; Put the following code into .dir-locals.el in your top directory of Logseq.
;;
;; ((nil (eval
;;        (lambda ()
;;          (when (and (not (null buffer-file-name))
;;                     (string-match-p "\\.md\\'" buffer-file-name)
;;                     (not (string= major-mode "logseq-mode")))
;;            (logseq-mode))))))
;;


(define-derived-mode logseq-mode markdown-mode "Logseq"
  "Edit Logseq journals and pages with Markdown"
  ;; Use TAB char instead of space
  (setq indent-tabs-mode t
        tab-width 4))

;; (add-hook 'logseq-mode-hook '(lambda ()
;;                                 (setq indent-tabs-mode t)))

(defun logseq-make-temp-buffer ()
  (interactive)
  "Logseq 用の temporary buffer を作る"
  (switch-to-buffer " *Logseq*")
  (logseq-mode t))

(provide 'logseq)
