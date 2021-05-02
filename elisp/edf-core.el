;;       __  ___      ___
;;      /""\|"  \    /"  |
;;     /    \\   \  //  /   Alfonso Vinti (alfonsovinti)
;;    /' /\  \\\  \/. ./    https://www.alfonsovinti.it
;;   //  __'  \\.    //     https://github.com/alfonsovinti
;;  /   /  \\  \\\   /
;; (___/    \___)\__/
;;
;; edf-core.el

;; UTF-8 everything!
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; please
(prefer-coding-system        'utf-8)   ; with sugar on top
(set-default-coding-systems  'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

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

;; keeping buffers automatically up-to-date
(global-auto-revert-mode 1)

;; This isn't a typewriter (even if it is a terminal); one space after sentences,
;; please.
(setq sentence-end-double-space nil)

;; require a trailing newline
(setq require-final-newline t)

;; delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; highlight current line.
(global-hl-line-mode t)
(dolist (mode '(eshell-mode-hook
                shell-mode-hook
                term-mode-hook))
  (add-hook mode (lambda () (setq-local global-hl-line-mode nil))))

;; doom-emacs core
;; https://github.com/hlissner/doom-emacs/blob/develop/core/core.el
;; (setq frame-inhibit-implied-resize t)
;; (setq inhibit-compacting-font-caches t)
;; (setq redisplay-skip-fontification-on-input t)

;; set font
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

;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; Needed if using emacsclient.
;; Otherwise, your fonts will be smaller than expected.
(add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font-13"))

;; changes certain keywords to symbols, such as lamda!
(setq global-prettify-symbols-mode t)

(if is-windows
    (set-frame-font "FiraCode Nerd Font" t t)
	  (set-frame-font "FiraCode Nerd Font" nil t))

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

(provide 'edf-core)
