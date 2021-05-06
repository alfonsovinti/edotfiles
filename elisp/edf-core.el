;;       __  ___      ___
;;      /""\|"  \    /"  |
;;     /    \\   \  //  /   Alfonso Vinti (alfonsovinti)
;;    /' /\  \\\  \/. ./    https://www.alfonsovinti.it
;;   //  __'  \\.    //     https://github.com/alfonsovinti
;;  /   /  \\  \\\   /
;; (___/    \___)\__/
;;
;; edf-core.el

;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil)

;; UTF-8 everything!
(set-charset-priority        'unicode)
(set-language-environment    'utf-8)
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)

(setq locale-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
;; (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp buffer-file-coding-system)
    (setq buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Let me write `y` or `n` even for important stuff
;; that would normally require me to fully type `yes` or `no`.
(defalias 'yes-or-no-p 'y-or-n-p)

;; do not show the startup screen.
(setq inhibit-startup-message t)

;; disable bell
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; disable non selected window highlight
;(setq cursor-in-non-selected-windows nil)
;(setq highlight-nonselected-windows nil)

;; scrolling
(setq scroll-conservatively 101) ;; value greater than 100 gets rid of half page jumping
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; how many lines at a time
(setq mouse-wheel-progressive-speed t) ;; accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; no make backup files
(setq make-backup-files nil)

;; lockfiles are evil.
(setq create-lockfiles nil)

;; keep .emacs.d clean
(use-package no-littering
  :init
  (setq no-littering-etc-directory
        (expand-file-name "config/" user-emacs-directory))
  (setq no-littering-var-directory
        (expand-file-name "data/" user-emacs-directory))
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq custom-file
      (no-littering-expand-etc-file-name "custom.el"))

(setq temporary-file-directory
      (no-littering-expand-var-file-name "tmp/"))

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; keeping buffers automatically up-to-date
(global-auto-revert-mode 1)

;; This isn't a typewriter (even if it is a terminal); one space after sentences,
;; please.
(setq sentence-end-double-space nil)

;; require a trailing newline
(setq require-final-newline t)

;; delete trailing whitespace before save
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; don't put intitial text in scratch buffer
(setq initial-scratch-message nil)

;; The default of 16 is too low. Give me a 64-object mark ring.
;; Across all files, make it 128.
(setq mark-ring-max 64)
(setq global-mark-ring-max 128)

;; minibuffer history
(setq history-length 25)

;; disable tool bar, menu bar, scroll bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; diable tooltip
(tooltip-mode -1)

;; disable right-side fringes
;(if (fboundp 'set-fringe-style) (set-fringe-style '(8 . 0)))
;; fringes appear outside the display margins
;(setq fringes-outside-margins t)
(set-fringe-mode 8)

;; number columns in the status bar
;(column-number-mode)

;; enable the mouse in terminal mode.
(xterm-mouse-mode 1)

;; find file hook
(defun lib-find-file-hook ()
  "Check the size of files when loading, and don't let me break them."
  (when (> (buffer-size) (* 1024 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hook 'lib-find-file-hook )

;; do not kill buffers
(defun lib-do-not-kill-buffers ()
  "Don't let the dashboard, scratch and Messages buffers die."
  (if (member (buffer-name (current-buffer)) '("*dashboard*" "*scratch*" "*Messages*"))
      (progn
        (bury-buffer)
        nil)
    t))
(add-hook 'kill-buffer-query-functions 'lib-do-not-kill-buffers)

;; break long lines at word boundaries
(visual-line-mode 1)

;; display line number
(setq display-line-numbers-type 'relative)
;(global-display-line-numbers-mode)
;; enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; show matching parentheses
(show-paren-mode 1)
;; https://www.emacswiki.org/emacs/ShowParenMode
;; (use-package paren
;;   :straight nil
;;   :config
;;   (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
;;   (show-paren-mode 1))

;; highlight current line.
(global-hl-line-mode t)
(dolist (mode '(eshell-mode-hook
                shell-mode-hook
                term-mode-hook))
  (add-hook mode (lambda () (setq-local global-hl-line-mode nil))))

;; doom-emacs core
;; https://github.com/hlissner/doom-emacs/blob/develop/core/core.el
;; (setq frame-inhibit-implied-resize t)
(setq inhibit-compacting-font-caches t)
;; (setq redisplay-skip-fontification-on-input t)

;; Needed if using emacsclient.
;; Otherwise, your fonts will be smaller than expected.
(if is-windows
    (add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font-11"))
	  (add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font-13")))

;; set font
(set-frame-font "FiraCode Nerd Font" nil t)

(set-face-attribute 'default nil
  :font "FiraCode Nerd Font"
  :height 110
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "FiraCode Nerd Font"
  :height 120
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "FiraCode Nerd Font"
  :height 110
  :weight 'medium)

(use-package ligature
  :straight (:type git :host github :repo "mickeynp/ligature.el")
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  ;; (global-ligature-mode nil)
  )

;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; changes certain keywords to symbols, such as lamda!
(setq global-prettify-symbols-mode t)

;; add emoji
(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun edf-minibuffer-setup-hook ()
  (setq gc-cons-threshold gc-cons-threshold-max-val))

(defun edf-minibuffer-exit-hook ()
  (setq gc-cons-threshold gc-cons-threshold-min-val))

(add-hook 'minibuffer-setup-hook #'edf-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'edf-minibuffer-exit-hook)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode)
(use-package diminish)
(use-package fringe-helper)

;; zoom
(use-package zoom
  :config
  (setq zoom-size '(0.85 . 0.85)))

(defvar edf-toggle-window-zoom--selected-window nil
  "Variable to store the name of selected window before zoom.")

(defvar edf-toggle-window-zoom--window-configuration nil
  "Variable to store the window configuration before zoom.")

(defun edf-toggle-window-zoom ()
  "Toggle window zoom."
  (interactive)
  (if edf-toggle-window-zoom--window-configuration
    (if (equal edf-toggle-window-zoom--selected-window (selected-window))
      (progn
        (set-window-configuration edf-toggle-window-zoom--window-configuration)
        (setq edf-toggle-window-zoom--selected-window nil)
        (setq edf-toggle-window-zoom--window-configuration nil))
      (progn
        (setq edf-toggle-window-zoom--selected-window (selected-window))
        (zoom)))
    (progn
      (setq edf-toggle-window-zoom--selected-window (selected-window))
      (setq edf-toggle-window-zoom--window-configuration (current-window-configuration))
      (zoom))))

;; alert
;; showing notifications from other packages in a variety of ways
(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'notifications))

;; autosave file
(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  (setq super-save-remote-files nil))

;; Set default connection mode to SSH, use Cygwin on windows
(if is-windows
  (setq tramp-default-method "sshx")
  (setq tramp-default-method "ssh"))

(provide 'edf-core)
