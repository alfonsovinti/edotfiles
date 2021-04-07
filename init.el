(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

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
