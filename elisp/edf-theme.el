;;       __  ___      ___
;;      /""\|"  \    /"  |
;;     /    \\   \  //  /   Alfonso Vinti (alfonsovinti)
;;    /' /\  \\\  \/. ./    https://www.alfonsovinti.it
;;   //  __'  \\.    //     https://github.com/alfonsovinti
;;  /   /  \\  \\\   /
;; (___/    \___)\__/
;;
;; edf-theme.el

;; You must run (all-the-icons-install-fonts) one time after
;; installing this package!
(use-package all-the-icons)
;; (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
;; (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
;; (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
;; (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
;; (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
;; (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append)

;; FIX nf-mdi-skull
(set-fontset-font t
                  (cons (decode-char 'ucs #xfb8a)
                        (decode-char 'ucs #xfb8a))
                  "FiraCode Nerd Font")

;; sidebar
(use-package neotree)

;(use-package nord-theme
;  :init
;    ;; Use `nord4` from Nord's "Snow Storm" palette as background color.
;    (setq nord-region-highlight "snowstorm")
;  :config (load-theme 'nord t))

(use-package doom-themes
  :init
  (add-to-list 'custom-theme-load-path "~/.emacs.d/config/themes")
  ;; :custom
  ;; (doom-purple-nord-region-highlight 'snowstorm)
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-purple-nord t)
  ;; Configure neotree theme
  (setq doom-themes-neotree-file-icons t)
  (doom-themes-neotree-config)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
;  :after eshell ;; make sure it gets hooked after eshell
;  :hook (after-init . doom-modeline-init)
  :init (doom-modeline-mode 1)
  :custom-face
  (mode-line ((t (:height 0.95))))
  (mode-line-inactive ((t (:height 0.95))))
  :custom
  (doom-modeline-height 26)
  (doom-modeline-bar-width 6)
  (doom-modeline-minor-modes t)
  (doom-modeline-indent-info t)
;  (doom-modeline-lsp t)
;  (doom-modeline-persp-name nil)
;  (doom-modeline-buffer-file-name-style 'truncate-except-project)
; (doom-modeline-major-mode-icon nil))
  (doom-modeline-irc nil))

(provide 'edf-theme)
