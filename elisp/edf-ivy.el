(use-package ivy
  :diminish
  ;; :bind (("C-s" . swiper)
  ;;        :map ivy-minibuffer-map
  ;;        ("TAB" . ivy-alt-done)
  ;;        ("C-l" . ivy-alt-done)
  ;;        ("C-j" . ivy-next-line)
  ;;        ("C-k" . ivy-previous-line)
  ;;        :map ivy-switch-buffer-map
  ;;        ("C-k" . ivy-previous-line)
  ;;        ("C-l" . ivy-done)
  ;;        ("C-d" . ivy-switch-buffer-kill)
  ;;        :map ivy-reverse-i-search-map
  ;;        ("C-k" . ivy-previous-line)
  ;;        ("C-d" . ivy-reverse-i-search-kill))
  :custom
  (setq ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1))

(use-package counsel
  :requires ivy
  ;; :bind (("C-M-j" . 'counsel-switch-buffer)
  ;;        :map minibuffer-local-map
  ;;        ("C-r" . 'counsel-minibuffer-history))
  ;; :custom
  ;; (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package swiper
  :requires ivy
  :bind (("C-s" . swiper)))

;(use-package ivy-rich
;  :requires ivy
;  :init
;  (ivy-rich-mode 1))

;; (use-package ivy-prescient
;;   :requires counsel
;;   :custom
;;   (ivy-prescient-enable-filtering nil)
;;   :config
;;   ;; Uncomment the following line to have sorting remembered across sessions!
;;   ;(prescient-persist-mode 1)
;;   (ivy-prescient-mode 1))

(provide 'edf-ivy)
