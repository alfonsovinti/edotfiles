;;       __  ___      ___
;;      /""\|"  \    /"  |
;;     /    \\   \  //  /   Alfonso Vinti (alfonsovinti)
;;    /' /\  \\\  \/. ./    https://www.alfonsovinti.it
;;   //  __'  \\.    //     https://github.com/alfonsovinti
;;  /   /  \\  \\\   /
;; (___/    \___)\__/
;;
;; early-init.el - Emacs 27+ pre-initialisation config

;; startup optimizations
(defconst gc-cons-threshold-min-val gc-cons-threshold)
(defconst gc-cons-threshold-max-val (* 50 1000 1000))

;; set gc threshold to max value.
(setq gc-cons-threshold gc-cons-threshold-max-val)

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

(setq package-enable-at-startup nil)
