;; You must run (all-the-icons-install-fonts) one time after
;; installing this package!
(use-package all-the-icons)

;(use-package nord-theme
;  :init
;    ;; Use `nord4` from Nord's "Snow Storm" palette as background color.
;    (setq nord-region-highlight "snowstorm")
;  :config (load-theme 'nord t))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
 )

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
;  :after eshell ;; make sure it gets hooked after eshell
  :init (doom-modeline-mode 1)
;  :hook (after-init . doom-modeline-init)
  :custom-face
  (mode-line ((t (:height 0.95))))
  (mode-line-inactive ((t (:height 0.95))))
  :custom
  (doom-modeline-height 26)
  (doom-modeline-bar-width 6)
  (doom-modeline-minor-modes t)
  (doom-modeline-indent-info t)
;  (doom-modeline-lsp t)
  (doom-modeline-irc nil)
;  (doom-modeline-persp-name nil)
;  (doom-modeline-buffer-file-name-style 'truncate-except-project)
; (doom-modeline-major-mode-icon nil))
)

(provide 'edf-theme)
