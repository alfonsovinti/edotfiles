;;       __  ___      ___
;;      /""\|"  \    /"  |
;;     /    \\   \  //  /   Alfonso Vinti (alfonsovinti)
;;    /' /\  \\\  \/. ./    https://www.alfonsovinti.it
;;   //  __'  \\.    //     https://github.com/alfonsovinti
;;  /   /  \\  \\\   /
;; (___/    \___)\__/
;;
;; edf-indentation.el

;; set default indentation size
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

;; use spaces instead of tabs for indentation
(setq-default indent-tabs-mode nil)

;; indentation guides
(use-package highlight-indent-guides
  :hook
  ((prog-mode . highlight-indent-guides-mode)
    (python-mode . highlight-indent-guides-mode)
    (yaml-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character))

(provide 'edf-indentation)
