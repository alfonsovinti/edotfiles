;;       __  ___      ___
;;      /""\|"  \    /"  |
;;     /    \\   \  //  /   Alfonso Vinti (alfonsovinti)
;;    /' /\  \\\  \/. ./    https://www.alfonsovinti.it
;;   //  __'  \\.    //     https://github.com/alfonsovinti
;;  /   /  \\  \\\   /
;; (___/    \___)\__/
;;
;; edf-keymap.el

;; ESC Cancels all
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; evil mode take over for buffer scrolling
(global-set-key (kbd "C-M-u") 'universal-argument)

;; use ctrl + mouse scroll to change text scale
(global-set-key (kbd "<C-whell-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-whell-up>") 'text-scale-increase)

(edf-leader-key-def
;; "*"    Search for symbol in project
  "."    '(counsel-find-file :which-key "Find file")
  "/"    '(swiper :which-key "Seach in project")
;; ";"    Comment operator
;;        Switc to last buffer
  "S"    '(save-buffer :which-key "Save buffer")
;; "b"    '(:ignore t :which-key "buffer")
  "c"    '(:ignore t :which-key "code")
  "cc"   '(evilnc-comment-or-uncomment-lines :which-key "toggle comment")
  "cC"   '(evilnc-copy-and-comment-lines :which-key "copy and comment")
  "e"    '(:ignore t :which-key "explore")
  "eb"   '(ibuffer :which-key "buffer")
  "ef"   '(dired-jump :which-key "file")
;; "f"    '(:ignore t :which-key "file")
;; "g"    '(:ignore t :which-key "git")
;; "h"    '(:ignore t :which-key "help")
;; "i"    '(:ignore t :which-key "insert")
;; "m"    '(:ignore t :which-key "bookmarks")
;; "n"    '(:ignore t :which-key "notes")
;; "o"    '(:ignore t :which-key "open")
;; "p"    '(:ignore t :which-key "project")
  "q"    '(:ignore t :which-key "quit/session")
  "qq"   '(kill-emacs :which-key "quit emacs")
  "qQ"   '(save-buffers-kill-terminal :which-key "save and quit emacs")
  "qZ"   '(save-buffers-kill-emacs :which-key "kill emacs (and daemon)")
  "qb"   '(kill-current-buffer :which-key "kill buffer")
  "qw"   '(evil-window-delete :which-key "kill window")
;; "s"    '(:ignore t :which-key "search")
  "t"    '(:ignore t :which-key "toggles")
  "tb"   '(neotree-toggle :which-key "neotree")
  "tc"   '(rainbow-mode :which-key "rainbow mode")
  "tz"   '(zoom-mode :which-key "zoom mode")
  "u"    '(:ignore t :which-key "ui")
  "uf"   '((lambda ()
             (interactive)
             (let ((old-face-attribute (face-attribute 'default :height)))
               (set-face-attribute 'default nil :height (+ old-face-attribute 10)))) :which-key "font increase")
  "uF"   '((lambda ()
             (interactive)
             (let ((old-face-attribute (face-attribute 'default :height)))
               (set-face-attribute 'default nil :height (- old-face-attribute 10)))) :which-key "font decrease")
  "ut"   '(counsel-load-theme :which-key "choose theme")
;; "w"    '(:ignore t :which-key "window")
;; "x"    '(:ignore t :which-key "execute")
  )

(provide 'edf-keymap)
