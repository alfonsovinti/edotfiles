;;; doom-purple-nord-theme.el --- inspired by Nord -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-purple-nord-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-purple-nord-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-purple-nord-theme
  :type 'boolean)

(defcustom doom-purple-nord-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-purple-nord-theme
  :type 'boolean)

(defcustom doom-purple-nord-comment-bg doom-purple-nord-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-purple-nord-theme
  :type 'boolean)

(defcustom doom-purple-nord-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-purple-nord-theme
  :type '(choice integer boolean))

(eval-and-compile
  (defcustom doom-purple-nord-region-highlight t
    "Determines the selection highlight style. Can be 'frost, 'snowstorm or t
(default)."
    :group 'doom-purple-nord-theme
    :type 'symbol))

;;
(def-doom-theme doom-purple-nord
  "A dark theme inspired by Nord."

  ;; name        default   256       16
  ((bg         '("#2E3440" nil       nil            ))
   (bg-alt     '("#3b4252" nil       nil            ))
   (base0      '("#2e3440" "black"   "black"        ))
   (base1      '("#3b4252" "#1e1e1e" "brightblack"  ))
   (base2      '("#3b4252" "#2e2e2e" "brightblack"  ))
   (base3      '("#434c5e" "#262626" "brightblack"  ))
   (base4      '("#434C5E" "#3f3f3f" "brightblack"  ))
   (base5      '("#4C566A" "#525252" "brightblack"  ))
   (base6      '("#4c566a" "#6b6b6b" "brightblack"  ))
   (base7      '("#D8DEE9" "#979797" "brightblack"  ))
   (base8      '("#eceff4" "#dfdfdf" "white"        ))
   (fg         '("#ECEFF4" "#ECECEC" "white"        ))
   (fg-alt     '("#E5E9F0" "#bfbfbf" "brightwhite"  ))

   (grey       base4)
   (red        '("#bf616a" "#ff6655" "red"          )) ;; Nord11
   (orange     '("#d08770" "#dd8844" "brightred"    )) ;; Nord12
   (green      '("#a3be8c" "#99bb66" "green"        )) ;; Nord14
   (teal       '("#8fbcbb" "#44b9b1" "brightgreen"  )) ;; Nord7
   (yellow     '("#ebcb8b" "#ECBE7B" "yellow"       )) ;; Nord13
   (blue       '("#81a1c1" "#51afef" "brightblue"   )) ;; Nord9
   (dark-blue  '("#5e81ac" "#2257A0" "blue"         )) ;; Nord10
   (magenta    '("#b48ead" "#c678dd" "magenta"      )) ;; Nord15
   (violet     '("#b48ead" "#a9a1e1" "brightmagenta")) ;; ??
   (cyan       '("#88c0d0" "#46D9FF" "brightcyan"   )) ;; Nord8
   (dark-cyan  '("#88c0d0" "#5699AF" "cyan"         )) ;; ??

   ;; face categories -- required for all themes
   (highlight      violet)
   (vertical-bar   (doom-darken base1 0.2))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-purple-nord-brighter-comments dark-cyan (doom-lighten base5 0.2)))
   (doc-comments   (doom-lighten (if doom-purple-nord-brighter-comments dark-cyan base5) 0.25))
   (constants      blue)
   (functions      teal)
   (keywords       blue)
   (methods        teal)
   (operators      blue)
   (type           teal)
   (strings        green)
   (variables      base7)
   (numbers        magenta)
   (region         (pcase doom-purple-nord-region-highlight
                     (`frost teal)
                     (`snowstorm base7)
                     ;; (_ base4)))
                     (_ base6)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-purple-nord-brighter-modeline)
   (-modeline-pad
    (when doom-purple-nord-padded-modeline
      (if (integerp doom-purple-nord-padded-modeline) doom-purple-nord-padded-modeline 4)))

   (region-fg
    (when (memq doom-purple-nord-region-highlight '(frost snowstorm))
      base0))

   (modeline-fg     nil)
   (modeline-fg-alt base6)

   (modeline-bg
    (if -modeline-bright
        (doom-blend bg base5 0.2)
      base1))
   (modeline-bg-l
    (if -modeline-bright
        (doom-blend bg base5 0.2)
      `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  (((region &override) :foreground region-fg)

   ((line-number &override) :foreground (doom-lighten 'base5 0.2))
   ((line-number-current-line &override) :foreground base7)
   ((paren-face-match &override) :foreground red :background base3 :weight 'ultra-bold)
   ((paren-face-mismatch &override) :foreground base3 :background red :weight 'ultra-bold)
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background base3 :weight 'light)
   ((vimish-fold-fringe &override)  :foreground teal)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-purple-nord-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    ;; :foreground (if -modeline-bright base8 highlight))
    :foreground (if -modeline-bright base8 blue))

   (doom-modeline-project-root-dir :foreground base6)
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; ediff
   (ediff-fine-diff-A    :background (doom-darken violet 0.4) :weight 'bold)
   (ediff-current-diff-A :background (doom-darken base0 0.25))

   ;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ;; highlight-symbol
   (highlight-symbol-face :background (doom-lighten base4 0.1) :distant-foreground fg-alt)

   ;; highlight-thing
   (highlight-thing :background (doom-lighten base4 0.1) :distant-foreground fg-alt)

   ;; ivy
   ((ivy-current-match &override) :foreground region-fg :weight 'semi-bold)


   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ()
  )

;;; doom-purple-nord-theme.el ends here
