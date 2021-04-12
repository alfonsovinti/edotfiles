;; display startup time
(defun edf-display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'edf-display-startup-time)

;; platform recon
(defconst is-linux   (eq system-type 'gnu/linux))
(defconst is-windows (memq system-type '(cygwin windows-nt ms-dos)))
(defconst is-mac     (eq system-type 'darwin))
(defconst is-bsd     (or is-mac (eq system-type 'berkeley-unix)))

;; elisp files
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

;; package
(require 'edf-package)

;; core settings
(require 'edf-core)

;; theme
(require 'edf-theme)

;; evil-mode
(require 'edf-evil-mode)

;; dired
(require 'edf-dired)

;; which-key
(require 'edf-which-key)

;; keymap
(require 'edf-keymap)

;; restore gc threshold value.
(setq gc-cons-threshold gc-cons-threshold-min-val)
