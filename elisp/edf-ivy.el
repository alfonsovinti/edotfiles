;;       __  ___      ___
;;      /""\|"  \    /"  |
;;     /    \\   \  //  /   Alfonso Vinti (alfonsovinti)
;;    /' /\  \\\  \/. ./    https://www.alfonsovinti.it
;;   //  __'  \\.    //     https://github.com/alfonsovinti
;;  /   /  \\  \\\   /
;; (___/    \___)\__/
;;
;; edf-ivy.el

(use-package ivy
  :defer 0.1
  :diminish
  :bind (:map ivy-minibuffer-map
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-wrap t)
  (setq enable-recursive-minibuffers t)
  (evil-collection-ivy-setup)
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :config (counsel-mode 1))

(use-package swiper
  :after ivy
  :bind ("C-s" . swiper))

(use-package all-the-icons-ivy-rich
  :after ivy
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))

; (use-package ivy-prescient
;   :after counsel
;   :custom
;   (ivy-prescient-enable-filtering nil)
;   :config
;   ;; sorting remembered across sessions!
;   ;(prescient-persist-mode 1)
;   (ivy-prescient-mode 1))

(provide 'edf-ivy)
