;; evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-Y-yank-to-eol t)
  :bind
  (:map evil-normal-state-map
    ; windows
    ("M-h" . 'evil-window-left)
    ("M-j" . 'evil-window-down)
    ("M-k" . 'evil-window-up)
    ("M-l" . 'evil-window-right)
    ; mooving windows
    ("M-H" . 'evil-window-move-far-left)
    ("M-J" . 'evil-window-move-very-bottom)
    ("M-K" . 'evil-window-move-very-top)
    ("M-L" . 'evil-window-move-far-right)
    ; split current window horizzontally
    ("M-_" . 'evil-window-split)
    ; split current window vertically
    ("M-|" . 'evil-window-vsplit)
;    :map evil-visual-state-map
;      ("kj" . 'lib/evil-maybe-exit)
;    :map evil-insert-state-map
;      ("kj" . 'lib/evil-maybe-exit)
;    :map evil-replace-state-map
;      ("kj" . 'lib/evil-maybe-exit)
  )
  :config
  (evil-mode 1)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-escape
  :init
  (setq evil-escape-key-sequence "kj")
  :config
  (evil-escape-mode 1))

(provide 'lib-evil-mode)
