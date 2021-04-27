;; ------------------------------------------------------------ ;;
;;        __  ___      ___                                      ;;
;;       /""\|"  \    /"  |                                     ;;
;;      /    \\   \  //  /     Alfonso Vinti (alfonsovinti)     ;;
;;     /' /\  \\\  \/. ./      https://www.alfonsovinti.it      ;;
;;    //  __'  \\.    //       https://github.com/alfonsovinti  ;;
;;   /   /  \\  \\\   /                                         ;;
;;  (___/    \___)\__/                                          ;;
;; ------------------------------------------------------------ ;;
;; edf-package.el                                               ;;
;; ------------------------------------------------------------ ;;

;;; straight.el
;; https://github.com/raxod502/straight.el/blob/develop/README.md#getting-started
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

;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
;; effectively replace use-package with straight-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(provide 'edf-package)
