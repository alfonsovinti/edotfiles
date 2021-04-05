(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

;; elisp files
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

;; settings
(require 'lib-settings)

;; package
(require 'lib-package)

;; theme
(require 'lib-theme)

;; evil-mode
(require 'lib-evil-mode)

;; keymap
(require 'lib-keymap)

;; which-key
(require 'lib-which-key)
