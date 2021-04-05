;; do not show the startup screen.
(setq inhibit-startup-message t)

;; common
(setq visible-bell t
      ring-bell-function 'ignore
      make-backup-files nil
      display-line-numbers-type 'relative)

;; This isn't a typewriter (even if it is a terminal); one space after sentences,
;; please.
(setq sentence-end-double-space nil)

;; lockfiles are evil.
(setq create-lockfiles nil)

;; require a trailing newline
(setq require-final-newline t)

;; don't put intitial text in scratch buffer
(setq initial-scratch-message nil)

;; The default of 16 is too low. Give me a 64-object mark ring.
;; Across all files, make it 128.
(setq mark-ring-max 64)
(setq global-mark-ring-max 128)

;; disable tool bar, menu bar, scroll bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; diable tooltip
;(tooltip-mode -1)

;; break long lines at word boundaries
(visual-line-mode 1)

;; disable right-side fringes
(if (fboundp 'set-fringe-style) (set-fringe-style '(8 . 0)))
;(set-fringe-mode 8)

;; number columns in the status bar
;(column-number-mode)

;; keeping buffers automatically up-to-date
(global-auto-revert-mode 1)

;; UTF-8 everything!
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; enable the mouse in terminal mode.
(xterm-mouse-mode 1)

;; Let me write `y` or `n` even for important stuff that would normally require
;; me to fully type `yes` or `no`.
(defalias 'yes-or-no-p 'y-or-n-p)

;; find file hook
(defun lib-find-file-hook ()
  "Check the size of files when loading, and don't let me break them."
  (when (> (buffer-size) (* 1024 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))
(add-hook 'find-file-hook 'lib-find-file-hook )

;; do not kull buffers
(defun lib-do-not-kill-buffers ()
  "Don't let the scratch and Messages buffers die."
  (if (member (buffer-name (current-buffer)) '("*scratch*" "*Messages*"))
      (progn
        (bury-buffer)
        nil)
    t))
(add-hook 'kill-buffer-query-functions 'lib-do-not-kill-buffers)

;; display line number
;(global-display-line-numbers-mode)
;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; show matching parentheses
(show-paren-mode 1)

;; highlight current line.
(global-hl-line-mode t)

(set-face-attribute 'default nil :font "Fira Code Nerd Font" :height 120)
(set-frame-font "Fira Code Nerd Font" nil t)

(provide 'lib-settings)
