;; startup optimizations
(defconst gc-cons-threshold-min-val gc-cons-threshold)
(defconst gc-cons-threshold-max-val (* 50 1000 1000))

;; set gc threshold to max value.
(setq gc-cons-threshold gc-cons-threshold-max-val)

(defconst is-linux   (eq system-type 'gnu/linux))
(defconst is-windows (memq system-type '(cygwin windows-nt ms-dos)))
(defconst is-mac     (eq system-type 'darwin))
(defconst is-bsd     (or is-mac (eq system-type 'berkeley-unix)))

(defun edf-display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'edf-display-startup-time)

;; elisp files
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

;; settings
(require 'edf-core)

;; package
(require 'edf-package)

;; theme
(require 'edf-theme)

;; evil-mode
(require 'edf-evil-mode)

;; keymap
(require 'edf-keymap)

;; which-key
(require 'edf-which-key)

;; restore gc threshold value.
(setq gc-cons-threshold gc-cons-threshold-min-val)
