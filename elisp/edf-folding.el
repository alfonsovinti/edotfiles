;(use-package origami
;  :hook (yaml-mode . origami-mode)
;  :config
;
;  (add-hook 'prog-mode-hook
;    (lambda ()
;      ;; parsers see in variable origami-parser-alist
;      (setq-local origami-fold-style 'triple-braces)
;      (origami-mode)
;      (origami-close-all-nodes (current-buffer))))
;
;  (define-key evil-normal-state-map "za" 'origami-forward-toggle-node)
;  (define-key evil-normal-state-map "zR" 'origami-close-all-nodes)
;  (define-key evil-normal-state-map "zM" 'origami-open-all-nodes)
;  (define-key evil-normal-state-map "zr" 'origami-close-node-recursively)
;  (define-key evil-normal-state-map "zm" 'origami-open-node-recursively)
;  (define-key evil-normal-state-map "zo" 'origami-show-node)
;  (define-key evil-normal-state-map "zc" 'origami-close-node)
;  (define-key evil-normal-state-map "zj" 'origami-forward-fold)
;  (define-key evil-normal-state-map "zk" 'origami-previous-fold)
;  (define-key evil-visual-state-map "zf"
;    '(lambda ()
;      "create fold and add comment to it"
;      (interactive)
;      (setq start (region-beginning))
;      (setq end (region-end))
;      (deactivate-mark)
;      (and (< end start)
;        (setq start (prog1 end (setq end start))))
;      (goto-char start)
;      (beginning-of-line)
;      (indent-according-to-mode)
;      (if (equal major-mode 'emacs-lisp-mode)
;        (insert ";; ")
;        ;; (indent-according-to-mode)
;        (insert comment-start " "))
;      ;; (insert comment-start " ")
;      (setq start (point))
;      (insert "Folding" " {{{")
;      (newline-and-indent)
;      (goto-char end)
;      (end-of-line)
;      (and (not (bolp))
;        (eq 0 (forward-line))
;        (eobp)
;        (insert ?\n))
;      (indent-according-to-mode)
;      (if (equal major-mode 'emacs-lisp-mode)
;        (insert ";; }}}")
;        (if (equal comment-end "")
;          (insert comment-start " }}}")
;          (insert comment-end "}}}")))
;      (newline-and-indent)
;      (goto-char start)))
;  ;; set origami mode
;  (global-origami-mode 1))



(provide 'edf-folding)
