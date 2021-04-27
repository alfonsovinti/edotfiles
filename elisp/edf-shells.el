;; ------------------------------------------------------------ ;;
;;        __  ___      ___                                      ;;
;;       /""\|"  \    /"  |                                     ;;
;;      /    \\   \  //  /     Alfonso Vinti (alfonsovinti)     ;;
;;     /' /\  \\\  \/. ./      https://www.alfonsovinti.it      ;;
;;    //  __'  \\.    //       https://github.com/alfonsovinti  ;;
;;   /   /  \\  \\\   /                                         ;;
;;  (___/    \___)\__/                                          ;;
;; ------------------------------------------------------------ ;;
;; edf-shells.el                                                ;;
;; ------------------------------------------------------------ ;;

(unless is-windows
  (use-package eterm-256color
    :hook (term-mode . eterm-256color-mode)))

(unless is-windows
  (use-package vterm
    :commands vterm
    :config
    (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
    ;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
    ;(setq shell-file-name "/bin/fish")
    (setq vterm-max-scrollback 5000)))

(unless is-windows
  (use-package term
    :straight nil
    :commands term
    :config
      (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
      ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args
      ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
      (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")))

(when is-windows
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(defun edf--eshell-init ()
  (require 'evil-collection-eshell)
  (evil-collection-eshell-setup)

  (use-package xterm-color)

  (push 'eshell-tramp eshell-modules-list)
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  ;; We want to use xterm-256color when running interactive commands
  ;; in eshell but not during other times when we might be launching
  ;; a shell command to gather its output.
  (add-hook 'eshell-pre-command-hook
            '(lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            '(lambda () (setenv "TERM" "dumb")))

  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Use completion-at-point to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  ;; Initialize the shell history
  (eshell-hist-initialize)

  (setenv "PAGER" "cat")

  ;; bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  ;; fix window focus
  (evil-define-key '(normal motion) eshell-mode-map (kbd "M-l") 'evil-window-right)
  (evil-define-key '(normal motion) eshell-mode-map (kbd "M-h") 'evil-window-left)
  (evil-normalize-keymaps)

  (setq eshell-prompt-function 'edf--eshell-prompt
        eshell-prompt-regexp edf--eshell-prompt-regexp
        eshell-aliases-file (no-littering-expand-etc-file-name "eshell/aliases")
        eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t
        eshell-highlight-prompt t
        eshell-prefer-lisp-functions nil))

(use-package eshell
  :straight nil
  :hook (eshell-first-time-mode . edf--eshell-init)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("bash" "fish" "htop" "ssh" "top" "zsh"))))

; TODO
;(use-package fish-completion
;  :hook (eshell-mode . fish-completion-mode))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

;; history autocompletion
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 0.5)
  (set-face-foreground 'company-preview-common "#4c566a")
  (set-face-background 'company-preview nil))

(provide 'edf-shells)
