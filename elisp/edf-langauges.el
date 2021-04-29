(use-package markdown-mode
  :mode "\\.md\\'")
  ;; :config
  ;; (setq markdown-command "marked")
  ;; (defun dw/set-markdown-header-font-sizes ()
  ;;   (dolist (face '((markdown-header-face-1 . 1.2)
  ;;                   (markdown-header-face-2 . 1.1)
  ;;                   (markdown-header-face-3 . 1.0)
  ;;                   (markdown-header-face-4 . 1.0)
  ;;                   (markdown-header-face-5 . 1.0)))
  ;;     (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

  ;; (defun dw/markdown-mode-hook ()
  ;;   (dw/set-markdown-header-font-sizes))

  ;; (add-hook 'markdown-mode-hook 'dw/markdown-mode-hook))

(use-package lua-mode)

(provide 'edf-langauges)
