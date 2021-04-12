(use-package nord-theme
  :ensure t
  :init
    ;; Use `nord4` from Nord's "Snow Storm" palette as background color.
    (setq nord-region-highlight "snowstorm")
  :config (load-theme 'nord t))

(use-package all-the-icons)

(provide 'edf-theme)
