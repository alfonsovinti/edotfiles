;;       __  ___      ___
;;      /""\|"  \    /"  |
;;     /    \\   \  //  /   Alfonso Vinti (alfonsovinti)
;;    /' /\  \\\  \/. ./    https://www.alfonsovinti.it
;;   //  __'  \\.    //     https://github.com/alfonsovinti
;;  /   /  \\  \\\   /
;; (___/    \___)\__/
;;
;; edf-lsp-dap.el

(use-package lsp-mode
  :commands lsp
  :hook ((typescript-mode js2-mode web-mode) . lsp)
  :bind (:map lsp-mode-map
         ("TAB" . completion-at-point))
  :custom (lsp-headerline-breadcrumb-enable nil))

(edf-leader-key-def
  "l"  '(:ignore t :which-key "lsp")
  "ld" 'xref-find-definitions
  "lr" 'xref-find-references
  "ln" 'lsp-ui-find-next-reference
  "lp" 'lsp-ui-find-prev-reference
  "ls" 'counsel-imenu
  "le" 'lsp-ui-flycheck-list
  "lS" 'lsp-ui-sideline-mode
  "lX" 'lsp-execute-code-action)

(use-package lsp-ui
  :commands lsp-ui-mode)
  ;; :hook (lsp-mode . lsp-ui-mode)
  ;; :config
  ;; (setq lsp-ui-sideline-enable t)
  ;; (setq lsp-ui-sideline-show-hover nil)
  ;; (setq lsp-ui-doc-position 'bottom)
  ;; (lsp-ui-doc-show))

(use-package dap-mode)
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  ;; (dap-tooltip-mode 1)
  ;; (require 'dap-node)
  ;; (dap-node-setup))

(use-package flycheck)
  ;; :hook (lsp-mode . flycheck-mode))

;;; end edf-lsp-dap.el
(provide 'edf-lsp-dap)
