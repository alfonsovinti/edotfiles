;;       __  ___      ___
;;      /""\|"  \    /"  |
;;     /    \\   \  //  /   Alfonso Vinti (alfonsovinti)
;;    /' /\  \\\  \/. ./    https://www.alfonsovinti.it
;;   //  __'  \\.    //     https://github.com/alfonsovinti
;;  /   /  \\  \\\   /
;; (___/    \___)\__/
;;
;; edf-evil-mode.el

(use-package undo-fu)

;; TODO run macros in selected region
;; run last registered macro
;(defun edf-@@ ()
;; (interactive)
;; (evil-execute-macro 1 last-kbd-macro))
;; (evil-ex-normal (region-beginning) (region-end) "@q"))
;; (evil-define-key 'visual 'global "Q" #'edf-@q )

(defun edf-evil-shift-right ()
  (interactive)
  (evil-shift-right evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defun edf-evil-shift-left ()
  (interactive)
  (evil-shift-left evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))


;; evil mode
(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil) ;; to fix https://github.com/emacs-evil/evil-collection/issues/60
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-undo-system 'undo-fu)
  :bind
  (:map evil-normal-state-map
    ; buffers
    ("gb" . 'evil-next-buffer)
    ("gB" . 'evil-prev-buffer)
    ; windows
    ("M-h" . 'evil-window-left)
    ("M-j" . 'evil-window-down)
    ("M-k" . 'evil-window-up)
    ("M-l" . 'evil-window-right)
    ; split current window horizzontally
    ("M-_" . 'evil-window-split)
    ; split current window vertically
    ("M-|" . 'evil-window-vsplit)
    ; mooving windows
    ("M-H" . 'evil-window-move-far-left)
    ("M-J" . 'evil-window-move-very-bottom)
    ("M-K" . 'evil-window-move-very-top)
    ("M-L" . 'evil-window-move-far-right)
    ; resize windows
    ("M-C-h" . 'evil-window-decrease-width)
    ("M-C-j" . 'evil-window-decrease-height)
    ("M-C-k" . 'evil-window-increase-height)
    ("M-C-l" . 'evil-window-increase-width)
    ("M-=" . 'balance-windows)
    ("M-f" . 'edf-toggle-window-zoom)
    ;; run macro in the q register
    ;;("Q" . "@p") FIX THIS
    :map evil-visual-state-map
    ("<" . 'edf-evil-shift-left)
    (">" . 'edf-evil-shift-right)
    ;  :map evil-insert-state-map
    ;    ("kj" . 'lib/evil-maybe-exit)
    ;  :map evil-replace-state-map
    ;    ("kj" . 'lib/evil-maybe-exit)
  )
  :config
  (evil-mode 1)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-escape
  :requires evil
  :init
  (setq evil-escape-key-sequence "kj")
  :config
  (evil-escape-mode 1))

(use-package evil-nerd-commenter
  :init
  (evilnc-default-hotkeys t))

(use-package evil-collection
  :requires evil
  :init
  (setq evil-collection-company-use-tng nil)  ;; is this a bug in evil-collection?
  :config
  (evil-collection-init))

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer edf-leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer edf-ltrl-c-keys
    :prefix "C-c"))

(provide 'edf-evil-mode)
