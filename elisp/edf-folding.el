;;; Thanks doom-emacs, the best solution
;;; https://github.com/hlissner/doom-emacs/blob/develop/modules/editor/fold
;; Helpers

;(defun edf--ensure-hideshow-mode ()
;  (unless (bound-and-true-p hs-minor-mode)
;    (hs-minor-mode +1)))

;(defun edf--vimish-fold-p ()
;  (and (featurep 'vimish-fold)
;       (cl-some #'vimish-fold--vimish-overlay-p
;                (overlays-at (point)))))

;(defun edf--outline-fold-p ()
;  (and (or (bound-and-true-p outline-minor-mode)
;           (derived-mode-p 'outline-mode))
;       (outline-on-heading-p)))

;(defun edf--hideshow-fold-p ()
;  (edf--ensure-hideshow-mode)
;  (save-excursion
;    (ignore-errors
;      (or (hs-looking-at-block-start-p)
;        (hs-find-block-beginning)
;        (unless (eolp)
;          (end-of-line)
;          (edf--hideshow-fold-p))))))

(use-package hideshow
  :straight nil ; built-in
  :commands (hs-toggle-hiding
             hs-hide-block
             hs-hide-level
             hs-show-all
             hs-hide-all)
  :hook ((yaml-mode . hs-minor-mode)
        (prog-mode . hs-minor-mode)))

;; load custom hideshowvis
(require 'edf-hideshowvis)
(dolist (mode '(yaml-mode-hook
                prog-mode-hook))
  (add-hook mode (lambda ()
    (hideshowvis-enable)
    (hideshowvis-symbols))))

;(use-package vimish-fold
;  :requires evil
;  :config
;  (vimish-fold-global-mode 1))

;(use-package evil-vimish-fold
;  :requires vimish-fold
;  :config
;  (global-evil-vimish-fold-mode))

(provide 'edf-folding)
