;; ------------------------------------------------------------ ;;
;;        __  ___      ___                                      ;;
;;       /""\|"  \    /"  |                                     ;;
;;      /    \\   \  //  /     Alfonso Vinti (alfonsovinti)     ;;
;;     /' /\  \\\  \/. ./      https://www.alfonsovinti.it      ;;
;;    //  __'  \\.    //       https://github.com/alfonsovinti  ;;
;;   /   /  \\  \\\   /                                         ;;
;;  (___/    \___)\__/                                          ;;
;; ------------------------------------------------------------ ;;
;; edf-git.el                                                   ;;
;; ------------------------------------------------------------ ;;

(use-package magit
  ;:bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
;(use-package forge
;  :requires magit)

(use-package git-link
  :commands git-link
  :config
  (setq git-link-open-in-browser t))

(use-package git-gutter
  :straight git-gutter-fringe
  :diminish
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 2)
  (when (display-graphic-p)
    (require 'git-gutter-fringe)
    (setq git-gutter-fr:side 'right-fringe)
    (set-face-foreground 'git-gutter-fr:added "#a3be8c")
    (fringe-helper-define 'git-gutter-fr:added nil
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX")
    (set-face-foreground 'git-gutter-fr:modified "#ebcb8b")
    (fringe-helper-define 'git-gutter-fr:modified nil
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX")
    (set-face-foreground 'git-gutter-fr:deleted "#bf616a")
    (fringe-helper-define 'git-gutter-fr:deleted nil
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"
      ".......XXX"))
  ;; These characters are used in terminal mode
  (setq git-gutter:modified-sign " ")
  (setq git-gutter:added-sign " ")
  (setq git-gutter:deleted-sign " ")
  (set-face-background 'git-gutter:added "#a3be8c")
  (set-face-background 'git-gutter:modified "#ebcb8b")
  (set-face-background 'git-gutter:deleted "#bf616a"))

(edf-leader-key-def
  "g"   '(:ignore t :which-key "git")
  "gs"  'magit-status
  "gd"  'magit-diff-unstaged
  "gc"  'magit-branch-or-checkout
  "gl"   '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gL"  'git-link
  "gb"  'magit-branch
  "gP"  'magit-push-current
  "gp"  'magit-pull-branch
  "gf"  'magit-fetch
  "gF"  'magit-fetch-all
  "gr"  'magit-rebase)

(provide 'edf-git)
