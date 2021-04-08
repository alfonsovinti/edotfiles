(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; bootstrap install use-package
(straight-use-package 'use-package)
;(setq use-package-always-ensure t)

;; so package-list-packages includes them
;(require 'package)
;(setq package-archives '(("melpa" . "https://melpa.org/packages/")
;                         ("org" . "https://orgmode.org/elpa/")
;                         ("gnu" . "https://elpa.gnu.org/packages/")))

(provide 'edf-package)
