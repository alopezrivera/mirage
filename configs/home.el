;; Initial frame size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width  . 70))

;; Inhibit startup message
(setq inhibit-startup-message t)

(shapes-extend "general")

(shapes-module "straight")

(shapes-module "no-littering")

(shapes-module "el-patch")

(global-set-key (kbd "C-x e") 'eval-buffer)

(global-set-key (kbd "C-c SPC") #'whitespace-mode)

(setq debug-on-error t)

(shapes-module "god-mode")

(shapes-module "vundo")

(shapes-module "multiple-cursors")

(shapes-extend "editing")

(desktop-save-mode 1)

(shapes-module "workgroups")

(tab-bar-mode 1)

;; Disable visible scroll bar
(scroll-bar-mode -1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable tooltips
(tooltip-mode -1)

;; Disable menu bar
(menu-bar-mode -1)

;; Frame name
(setq-default frame-title-format '("Emacs [%m] %b"))

;; Fringe mode
(set-fringe-mode nil)

;; Enable visual bell
(setq visible-bell t)

(advice-add 'yes-or-no-p :override #'y-or-n-p)

(setq scroll-step 1)
(setq scroll-conservatively 101)
(setq scroll-preserve-screen-position 1)

;; Display line numbers by side
(global-set-key (kbd "C-c l") #'display-line-numbers-mode)

;; Display column number
(column-number-mode)

(shapes-extend "display")

(shapes-module "swiper")

(shapes-module "rg")

(shapes-module "yasnippet")

(shapes-module "counsel")

(shapes-module "helpful")

(shapes-module "which-key")

;; Create new frame
(global-set-key (kbd "C-S-n") #'make-frame-command)

;; ace-window
(straight-use-package 'ace-window)
(require 'ace-window)

(global-set-key (kbd "C-x o") 'ace-window)

;; winner mode
(winner-mode)

(global-set-key (kbd "C-x -") #'balance-windows)

(setq split-width-threshold 70)

(shapes-extend "navigation")

(shapes-module "ivy")

(shapes-module "magit")

(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 2)
(setq kept-old-versions 2)

(shapes-module "dirvish")

(shapes-extend "file-management")

(shapes-module "projectile")

(shapes-module "treemacs")

;; default face
(set-face-attribute 'default nil        :font "Fira Code Retina" :height 93)

;; fixed pitch face
(set-face-attribute 'fixed-pitch nil    :font "Fira Code Retina" :height 93)

;; variable pitch face
(set-face-attribute 'variable-pitch nil :font "PT Sans"  :height 105 :weight 'regular)

;; italic
(defface custom/italic
  '((t :font "Victor Mono" :height  86 :weight  bold :slant italic))
  "Italic typeface")

;; titles
(setq typeface-title "Ringbearer")

;; heading face
(setq typeface-heading "Century Gothic")

;; Mode line
(set-face-attribute 'mode-line nil :height 85 :inherit 'fixed-pitch)

(display-time-mode t)

(shapes-module "delight")

(shapes-module "all-the-icons")

(shapes-module "rainbow-mode")

(shapes-extend "ui")

(setq light    'modus-operandi)

(setq dark     'modus-vivendi)

(setq modeline 'doom-modeline-mode)

(add-hook 'after-init-hook modeline)

(shapes-outfit "themes")
(shapes-outfit "mode-lines")

(shapes-module "circadian")

(shapes-extend "theme")

(shapes-module "hideshow")

(shapes-module "company")

(shapes-module "flycheck")

(shapes-module "elpy")

(shapes-extend "ide")

;; remove duplicates in shell history
(setq comint-input-ignoredups t)

(shapes-module "org")

;; title face

(defun custom/org-typeface-title ()
  (with-eval-after-load 'org-faces
    (set-face-attribute 'org-document-title nil :font typeface-title :height 300 :weight 'regular :foreground 'unspecified)))

(add-hook 'org-mode-hook #'custom/org-typeface-title)

(defun custom/org-typefaces-body ()
  (with-eval-after-load 'org-faces

    ;; Code
    (set-face-attribute 'org-block                 nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code                  nil                 :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim              nil                 :inherit '(shadow fixed-pitch))

    ;; Tables
    (set-face-attribute 'org-table                 nil                 :inherit '(shadow fixed-pitch))

    ;; Lists
    (set-face-attribute 'org-checkbox              nil                 :inherit 'fixed-pitch)

    ;; Meta
    (set-face-attribute 'org-meta-line             nil                 :inherit 'fixed-pitch)
    (set-face-attribute 'org-document-info         nil                 :inherit 'fixed-pitch)
    (set-face-attribute 'org-document-info-keyword nil                 :inherit 'fixed-pitch)
    (set-face-attribute 'org-special-keyword       nil                 :inherit 'fixed-pitch)))

(add-hook 'org-mode-hook #'custom/org-typefaces-body)

(defun custom/org-typeface-indent ()
  "Indent typeface used in `org-indent-mode' and `visual-line-mode'."
  (with-eval-after-load 'org-indent-mode
    (set-face-attribute 'org-indent                nil                 :inherit '(org-hide fixed-pitch))))

(add-hook 'org-mode-hook #'custom/org-typeface-indent)

;; use levels 1 through 8
(setq org-n-level-faces 8)

;; do not cycle header style after 8th level
(setq org-cycle-level-faces nil)

;; hide leading stars
(setq org-hide-leading-starts t)

;; font sizes
(defun custom/org-header-setup () 
  (with-eval-after-load 'org-faces

    ;; heading font sizes
    (dolist (face '((org-level-1 . 1.6)
                    (org-level-2 . 1.4)
                    (org-level-3 . 1.3)
                    (org-level-4 . 1.2)
                    (org-level-5 . 1.15)
                    (org-level-6 . 1.10)
                    (org-level-7 . 1.05)
                    (org-level-8 . 1.00)))
         (set-face-attribute (car face) nil :font typeface-heading :weight 'bold :height (cdr face)))))

(add-hook 'org-mode-hook #'custom/org-header-setup)

(add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))

(add-hook 'org-mode-hook (lambda () (progn (visual-line-mode 1) (setq line-move-visual t))))

;; Change ellipsis ("...") to remove clutter
(setq org-ellipsis " ♢")

(setq org-image-actual-width nil)

;; List indentation
(setq-default org-list-indent-offset 1)

;; symbols, super- and subscripts
(setq org-pretty-entities nil)

(setq org-format-latex-options
        (list :foreground 'default
              :scale      1.2))

;; Justify equation labels - [fleqn]
;; Preview page width      - 10.5cm
(setq org-format-latex-header
      (string-join '("\\documentclass[fleqn]{article}"
		        "\\usepackage[usenames]{color}"
			
			"\\usepackage{bm}"
			
			"\\pagestyle{empty}"
			"\\setlength{\\textwidth}{10.5cm}"
			"\\addtolength{\\textwidth}{-3cm}"
			"\\setlength{\\oddsidemargin}{1.5cm}"
			"\\addtolength{\\oddsidemargin}{-2.54cm}"
			"\\setlength{\\evensidemargin}{\\oddsidemargin}"
			"\\setlength{\\textheight}{\\paperheight}"
			"\\addtolength{\\textheight}{-\\headheight}"
			"\\addtolength{\\textheight}{-\\headsep}"
			"\\addtolength{\\textheight}{-\\footskip}"
			"\\addtolength{\\textheight}{-3cm}"
			"\\setlength{\\topmargin}{1.5cm}"
			"\\addtolength{\\topmargin}{-2.54cm}")
		   "\n"))

;; SVG LaTeX equation preview
(setq org-latex-create-formula-image-program 'dvisvgm)

;; theme-specific LaTeX preview directory
(defun custom/latex-preview-directory ()
  "Set `org-preview-latex-image-directory' to the SVG
LaTeX preview directory of the current theme"
  (setq org-preview-latex-image-directory
   (concat "/tmp/ltximg/" (custom/current-theme) "/")))

(defun custom/latex-preview-reload ()
  "Reload all LaTeX previews in buffer,
ensuring the LaTeX preview directory
matches the current theme."
  (if (custom/in-mode "org-mode")
      (progn (org-latex-preview '(64))
	        (custom/latex-preview-directory)
		(org-latex-preview '(16)))))

(add-hook 'org-mode-hook #'custom/latex-preview-reload)

(shapes-module "org-modern")

(shapes-module "org-appear")

(shapes-module "org-fragtog")

(shapes-module "org-paragraph")

(shapes-module "org-diary")

(shapes-module "org-roam")

(shapes-module "ox-rst")

(shapes-module "org-agenda")

(shapes-module "org-contacts")

(shapes-module "org-calendar")

(shapes-module "org-tempo")

(shapes-module "org-capture")

(shapes-module "org-babel")

(shapes-extend "org")

;; Conclude initialization file
(provide 'home)
