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

(provide 'edf-keymap)
