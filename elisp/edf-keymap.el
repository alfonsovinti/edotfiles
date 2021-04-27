;; ------------------------------------------------------------ ;;
;;        __  ___      ___                                      ;;
;;       /""\|"  \    /"  |                                     ;;
;;      /    \\   \  //  /     Alfonso Vinti (alfonsovinti)     ;;
;;     /' /\  \\\  \/. ./      https://www.alfonsovinti.it      ;;
;;    //  __'  \\.    //       https://github.com/alfonsovinti  ;;
;;   /   /  \\  \\\   /                                         ;;
;;  (___/    \___)\__/                                          ;;
;; ------------------------------------------------------------ ;;
;; edf-keymap.el                                                ;;
;; ------------------------------------------------------------ ;;

;; ESC Cancels all
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; evil mode take over for buffer scrolling
(global-set-key (kbd "C-M-u") 'universal-argument)

;; use ctrl + mouse scroll to change text scale
(global-set-key (kbd "<C-whell-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-whell-up>") 'text-scale-increase)

(edf-leader-key-def
  "e"    '(dired-jump :which-key "explore")
  "F"    '(:ignore t :which-key "font")
  "Fi"   '((lambda ()
             (interactive)
             (let ((old-face-attribute (face-attribute 'default :height)))
               (set-face-attribute 'default nil :height (+ old-face-attribute 10)))) :which-key "increase")
  "Fd"   '((lambda ()
             (interactive)
             (let ((old-face-attribute (face-attribute 'default :height)))
               (set-face-attribute 'default nil :height (- old-face-attribute 10)))) :which-key "decrease")
  "t"    '(:ignore t :which-key "toggles")
  "tc"   '(rainbow-mode :which-key "rainbow mode")
  "tt"   '(counsel-load-theme :which-key "choose theme"))

;; KEYS
;; "*"    Search for symbol in project
;; "."    Find file
;; "/"    Seach in project
;;        Switc to last buffer
;; "b"    '(:ignore t :which-key "buffer")
;; "c"    '(:ignore t :which-key "code")
;; "f"    '(:ignore t :which-key "file")
;; "g"    '(:ignore t :which-key "git")
;; "h"    '(:ignore t :which-key "help")
;; "i"    '(:ignore t :which-key "insert")
;; "o"    '(:ignore t :which-key "open")
;; "m"    '(:ignore t :which-key "bookmarks")
;; "n"    '(:ignore t :which-key "notes")
;; "p"    '(:ignore t :which-key "project")
;; "q"    '(:ignore t :which-key "quit/session")
;; "s"    '(:ignore t :which-key "search")
;; "t"    '(:ignore t :which-key "toggles")
;; "u"    '(:ignore t :which-key "ui")
;; "w"    '(:ignore t :which-key "window")
;; "x"    '(:ignore t :which-key "execute")



(provide 'edf-keymap)
