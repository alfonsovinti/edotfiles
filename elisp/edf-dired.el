;;;
;; TODO: add key map to open on "v" o "s" split window
;;

;; https://emacs.stackexchange.com/questions/15117/how-to-use-o-to-open-from-dired-ibuffer-into-another-frame
(defun edf-display-buffer (buffer-or-name alist direction &optional size pixelwise)
    "BUFFER: The buffer that will be displayed.
    ALIST: See the doc-string of `display-buffer' for more information.
    DIRECTION: Must use one of these symbols: 'left 'right 'below 'above
    SIZE: See the doc-string for `split-window'.
    PIXELWISE: See the doc-string for `split-window'.
    There are three possibilities:
    - (1) If a window on the frame already displays the target buffer,
    then just reuse the same window.
    - (2) If there is already a window in the specified direction in relation
    to the selected window, then display the target buffer in said window.
    - (3) If there is no window in the specified direction, then create one
    in that direction and display the target buffer in said window."
    (let* ((buffer
	(if (bufferp buffer-or-name)
	    buffer-or-name
	    (get-buffer buffer-or-name)))
	(window
	    (cond
		((get-buffer-window buffer (selected-frame)))
		((window-in-direction direction))
		(t
		    (split-window (selected-window) size direction pixelwise)))))
	(window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
	window))

(defun dired-display-buffer (&optional direction alist)
    "Display a dired-mode buffer or a file underneath point in a dired-mode buffer."
    (interactive)
    (let* ((file-or-dir (or (and (eq major-mode 'dired-mode) (dired-get-file-for-visit))
		(read-directory-name "Directory:  ")))
	    (buffer (find-file-noselect file-or-dir))
	    (direction
		(if direction
		    direction
		    (let ((char (read-char-exclusive (concat
			"["
			(propertize "l" 'face '(:foreground "red"))
			"]"
			(propertize "eft" 'face '(:foreground "blue"))
			" | ["
			(propertize "r" 'face '(:foreground "red"))
			"]"
			(propertize "ight" 'face '(:foreground "blue"))
			" | ["
			(propertize "a" 'face '(:foreground "red"))
			"]"
			(propertize "bove" 'face '(:foreground "blue"))
			" | ["
			(propertize "b" 'face '(:foreground "red"))
			"]"
			(propertize "elow" 'face '(:foreground "blue"))))))
		    (cond
		    ((eq char ?l)
			'left)
		    ((eq char ?r)
			'right)
		    ((eq char ?a)
			'above)
		    ((eq char ?b)
			'below)
		    ;;; FIXME:  @lawlist may add a loop similar to `org-capture'
		    ;;; whereby a new `read-char-exclusive' will be initiated if
		    ;;; a user did not initially choose a valid option (l/r/a/b).
		    (t
			(let ((debug-on-quit nil)
			    (msg (concat "dired-display-buffer:  "
					"You did not select l/r/a/b "
					"-- exiting.")))
			(signal 'quit `(,msg)))))))))
    (edf-display-buffer buffer alist direction)))

(use-package all-the-icons-dired)

(use-package dired
    :straight nil
    :defer 1
    :commands (dired dired-jump)
    :config
    (setq dired-listing-switches "-alh"
        dired-hide-details-hide-symlink-targets nil
        ls-lisp-verbosity nil ; hide the link count, user, and group columns - default is '(links uid gid)
        ls-lisp-dirs-first t ; only windows support ?
        ; use ISO dates (the first is for recent dates, second for old dates)
        ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")
        ls-lisp-use-localized-time-format t
	    ;dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil)

    (autoload 'dired-omit-mode "dired-x")

    (add-hook 'dired-load-hook
    	(lambda ()
    	    (interactive)
    	    (dired-collapse)))

    (add-hook 'dired-mode-hook
    	(lambda ()
    	    (interactive)
    	    (dired-omit-mode 1)
    	    ;(dired-hide-details-mode 1)
    	    ;(all-the-icons-dired-mode 1) ; FIXME non funzione su windows
    	    (hl-line-mode 1)))

    (use-package dired-rainbow
    	:defer 2
    	:config
    	(dired-rainbow-define-chmod directory "#81a1c1" "d.*")
    	(dired-rainbow-define html "#b48ead" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    	(dired-rainbow-define xml "#ebcb8b" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    	(dired-rainbow-define document "#b48ead" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    	(dired-rainbow-define markdown "#ebcb8b" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    	(dired-rainbow-define database "#b48ead" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    	(dired-rainbow-define media "#d08770" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    	(dired-rainbow-define image "#88c0d0" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    	(dired-rainbow-define log "#8fbcbb" ("log"))
    	(dired-rainbow-define shell "#d08770" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    	(dired-rainbow-define interpreted "#a3be8c" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    	(dired-rainbow-define compiled "#8fbcbb" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    	(dired-rainbow-define executable "#5e81ac" ("exe" "msi"))
    	(dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    	(dired-rainbow-define packaged "#d08770" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    	(dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    	(dired-rainbow-define fonts "#81a1c1" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    	(dired-rainbow-define partition "#bf616a" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    	(dired-rainbow-define vc "#5e81ac" ("git" "gitignore" "gitattributes" "gitmodules"))
    	(dired-rainbow-define-chmod executable-unix "#a3be8c" "-.*x.*"))

    (use-package dired-single
	   :defer t)

    (use-package dired-ranger
	   :defer t)

    (use-package dired-collapse
	   :defer t)

    (evil-collection-define-key 'normal 'dired-mode-map
	   "h" 'dired-single-up-directory
	   "H" 'dired-omit-mode
	   "l" 'dired-single-buffer
	   "y" 'dired-ranger-copy
	   "mv" 'dired-ranger-move
	   "p" 'dired-ranger-paste))

(provide 'edf-dired)