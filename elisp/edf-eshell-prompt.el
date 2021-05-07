;;       __  ___      ___
;;      /""\|"  \    /"  |
;;     /    \\   \  //  /   Alfonso Vinti (alfonsovinti)
;;    /' /\  \\\  \/. ./    https://www.alfonsovinti.it
;;   //  __'  \\.    //     https://github.com/alfonsovinti
;;  /   /  \\  \\\   /
;; (___/    \___)\__/
;;
;; edf-eshell-prompt.el

(setq edf--eshell-prompt-symbol "")
(setq edf--eshell-prompt-vicmd-symbol "")

(defun read-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun edf--get-current-package-version ()
  (interactive)
  (let ((package-json-file (concat (eshell/pwd) "/package.json")))
    (when (file-exists-p package-json-file)
      (let* ((package-json-contents (read-file package-json-file))
             (package-json (ignore-errors (json-parse-string package-json-contents))))
        (when package-json
          (ignore-errors (gethash "version" package-json)))))))

(defun edf--map-line-to-status-char (line)
  (cond ((string-match "^?\\? " line) "?")))

(defun edf--get-git-status-prompt ()
  (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
    (seq-uniq (seq-filter 'identity (mapcar 'edf--map-line-to-status-char status-lines)))))

(defun edf--get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
      (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

(defun edf--eshell-prompt ()
  (let ((current-branch (magit-get-current-branch))
        (package-version (edf--get-current-package-version)))
    (concat
      "\n"
      ;(propertize (system-name) 'face `(:foreground "#5e81ac"))
      ;(all-the-icons-wicon "tornado")
      ;" "
      (propertize (edf--get-prompt-path) 'face `(:foreground "#88c0d0"))
      (when current-branch
        (concat
          (propertize " on " 'face `(:foreground "#eceff4"))
          (propertize (all-the-icons-octicon "git-branch")
            'face `(:family ,(all-the-icons-octicon-family) :height 1.2 :foreground "#b48ead")
            'display '(raise -0.1))
          " "
          (propertize current-branch 'face `(:foreground "#b48ead"))))
      (when package-version
        (concat
          (propertize " via " 'face `(:foreground "#eceff4"))
          (propertize (all-the-icons-alltheicon "nodejs")
            'face `(:family ,(all-the-icons-alltheicon-family) :height 1.2 :foreground "#a3be8c")
            'display '(raise -0.1))
          " "
          (propertize package-version 'face `(:foreground "#a3be8c"))))
      (propertize " at " 'face `(:foreground "#eceff4"))
      (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground "#ebcb8b"))
      "\n"
      (propertize "ﮊ" 'face `(:foreground "#b48ead" :height 1.2 :weight 'bold))
      " "
      (if (= (user-uid) 0)
        (propertize edf--eshell-prompt-symbol 'face `(:foreground "#bf616a" :weight 'bold))
        (propertize edf--eshell-prompt-symbol 'face `(:foreground "#a3be8c" :weight 'bold :height 1.2)))
      (propertize " " 'face `(:foreground "#eceff4")))))

(setq edf--eshell-prompt-regexp (concat "^ﮊ " edf--eshell-prompt-symbol " "))

(provide 'edf-eshell-prompt)
