;; ------------------------------------------------------------ ;;
;;        __  ___      ___                                      ;;
;;       /""\|"  \    /"  |                                     ;;
;;      /    \\   \  //  /     Alfonso Vinti (alfonsovinti)     ;;
;;     /' /\  \\\  \/. ./      https://www.alfonsovinti.it      ;;
;;    //  __'  \\.    //       https://github.com/alfonsovinti  ;;
;;   /   /  \\  \\\   /                                         ;;
;;  (___/    \___)\__/                                          ;;
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

;;; eshell prompt
(defun read-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun edf--get-current-package-version ()
  (interactive)
  (let ((package-json-file (concat (eshell/pwd) "/package.json")))
    (when (file-exists-p package-json-file)
      (let* ((package-json-contents (read-file package-json-file))
             (package-json (ignore-errors (json-parse-string package-json-contents))))
        (when package-json
          (ignore-errors (gethash "version" package-json)))))))

(defun edf--map-line-to-status-char (line)
  (cond ((string-match "^?\\? " line) "?")))

(defun edf--get-git-status-prompt ()
  (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
    (seq-uniq (seq-filter 'identity (mapcar 'edf--map-line-to-status-char status-lines)))))

(defun edf--get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
      (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

;; This prompt function mostly replicates my custom zsh prompt setup
;; that is powered by github.com/denysdovhan/spaceship-prompt.
(defun edf--eshell-prompt ()
  (let ((current-branch (magit-get-current-branch))
        (package-version (edf--get-current-package-version)))
    (concat
      "\n"
      ;(propertize (system-name) 'face `(:foreground "#5e81ac"))
      ;(all-the-icons-wicon "tornado")
      ;" "
      (propertize (edf--get-prompt-path) 'face `(:foreground "#88c0d0"))
      (when current-branch
        (concat
          " "
          (propertize (all-the-icons-faicon "angle-right")
            'face `(:family ,(all-the-icons-faicon-family) :height 1.2 :foreground "#eceff4")
            'display '(raise -0.1))
          " "
          (propertize (all-the-icons-octicon "git-branch")
            'face `(:family ,(all-the-icons-octicon-family) :height 1.2 :foreground "#b48ead")
            'display '(raise -0.1))
          " "
          (propertize current-branch 'face `(:foreground "#b48ead"))))
      (when package-version
        (concat
          " "
          (propertize (all-the-icons-faicon "angle-right")
            'face `(:family ,(all-the-icons-faicon-family) :height 1.2 :foreground "#eceff4")
            'display '(raise -0.1))
          " "
          (propertize (all-the-icons-alltheicon "nodejs")
            'face `(:family ,(all-the-icons-alltheicon-family) :height 1.2 :foreground "#a3be8c")
            'display '(raise -0.1))
          " "
          (propertize package-version 'face `(:foreground "#a3be8c"))))
      " "
      (propertize (all-the-icons-faicon "angle-right")
        'face `(:family ,(all-the-icons-faicon-family) :height 1.2 :foreground "#eceff4")
        'display '(raise -0.1))
      " "
      (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground "#4c566a"))
      (if (= (user-uid) 0)
        (propertize "\n#" 'face `(:foreground "#bf616a"))
        (propertize "\n$" 'face `(:foreground "#a3be8c")))
      (propertize " " 'face `(:foreground "#eceff4")))))

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
        eshell-prompt-regexp "$ "
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
