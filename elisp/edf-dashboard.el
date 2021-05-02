;;       __  ___      ___
;;      /""\|"  \    /"  |
;;     /    \\   \  //  /   Alfonso Vinti (alfonsovinti)
;;    /' /\  \\\  \/. ./    https://www.alfonsovinti.it
;;   //  __'  \\.    //     https://github.com/alfonsovinti
;;  /   /  \\  \\\   /
;; (___/    \___)\__/
;;
;; edf-dashboard.el

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner 2)
  ;(setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  (setq dashboard-center-content t)
;  (setq dashboard-items '((recents . 5)
;                          (agenda . 5 )
;                          (registers . 3)))
  (setq dashboard-items '((recents  . 5)
                          (agenda . 5)
                          (bookmarks . 5)
                          ;(projects . 3)
                          (registers . 5)))
;  (dashboard-modify-heading-icons '((recents . "file-text")
;                                  (bookmarks . "book")))
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
    ;; Format: "(icon title help action face prefix suffix)"
    `((("" "" "" nil nil "" "")) ;; line1
        ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (browse-url "https://github.com/alfonsovinti/edotfiles"))))))
  (setq dashboard-set-footer nil))

;; dashboard in emacsclient
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(provide 'edf-dashboard)
