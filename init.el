;;; Emacs initialization file

;; -----------------------------



;;     Package management



;; -----------------------------

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa"          . "https://melpa.org/packages/")
			 ("org"            . "https://orgmode.org/elpa/")
			 ("elpa"           . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; If true, Emacs will attempt to download packages in use-package declarations
(setq use-package-always-ensure t)

;; Customize interface code blocks
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; -----------------------------



;;       System defaults



;; -----------------------------

(setq default-directory "~/.emacs.d/")

;; -----------------------------



;;             UI



;; -----------------------------

;; Inhibit startup message
(setq inhibit-startup-message t)

;; Disable visible scroll bar
(scroll-bar-mode -1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable tooltips
(tooltip-mode -1)

;; Set width of side fringes
(set-fringe-mode 10)

;; Disable menu bar
(menu-bar-mode -1)

;; Enable visual bell
(setq visible-bell t)

;; Theme
(use-package doom-themes
  :init (load-theme 'doom-dracula t))
(load-theme 'deeper-blue)

;; Initial window size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width  . 100))

;; Default face
(set-face-attribute 'default nil :font "Fira Code Retina" :height 110)

;; Fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 110)

;; Variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 110 :weight 'regular)

;; Modeline font
(set-face-attribute 'mode-line nil :font "Fira Code Retina" :height 100)

;; -----------------------------



;;        Key remappings



;; -----------------------------

;; Make keyboard ESC quit dialogs, equivalent to C-g
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; -----------------------------



;;         Key bindings



;; -----------------------------

;; Create binding for evaluating buffer
(global-set-key (kbd "C-x e") 'eval-buffer)

;; -----------------------------



;;     Search and completion



;; -----------------------------

;; Search
(use-package swiper)

;; Completion framework
(use-package counsel)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


;; -----------------------------



;;          Org Mode



;; -----------------------------

;; Face setup
(defun custom/org-face-setup ()
  ;;  Headers variable font sizes
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))
  ;; Set fixed and variable faces
  (progn (set-face-attribute 'org-block            nil :foreground nil :inherit 'fixed-pitch)
	 (set-face-attribute 'org-code             nil                 :inherit '(shadow fixed-pitch))
	 (set-face-attribute 'org-table            nil                 :inherit '(shadow fixed-pitch))
	 (set-face-attribute 'org-verbatim         nil                 :inherit '(shadow fixed-pitch))
	 (set-face-attribute 'org-special-keyword  nil                 :inherit '(font-lock-comment-face fixed-pitch))
	 (set-face-attribute 'org-meta-line        nil                 :inherit '(font-lock-comment-face fixed-pitch))
	 (set-face-attribute 'org-checkbox         nil                 :inherit 'fixed-pitch)
	 (set-face-attribute 'org-indent           nil                 :inherit '(org-hide fixed-pitch))))

;; Org hook
(defun custom/org-mode-setup ()

  ;; Enter variable pitch mode
  (variable-pitch-mode 1)

  ;; Enter visual line mode:  wrap long lines at the end of the buffer, as opposed to truncating them
  (visual-line-mode    1)

  ;; Enter indent mode: indent truncated lines appropriately
  (org-indent-mode      )

  (custom/org-face-setup))

;; Load Org Mode
(use-package org
  :hook (org-mode . custom/org-mode-setup)
  :config

  ;; Change ellipsis ("...") to remove clutter
  (setq org-ellipsis " ▾")

  ;; Hide markup symbols
  (setq org-hide-emphasis-markers t)
)

;; Custom header bullets
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphens with dots
(font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Center text
(use-package olivetti
  :diminish
  )

(add-hook 'olivetti-mode-on-hook (lambda () (olivetti-set-width 0.9)))

(add-hook 'org-mode-hook 'olivetti-mode)

;; -----------------------------
;;          Org Babel
;; -----------------------------

;; Set indentation of code blocks to 0
(setq org-edit-src-content-indentation 0)

;; Indent code blocks appropriately when inside lists
(setq org-src-preserve-indentation     nil)

;; Make code editing reasonable
(setq org-src-tab-acts-natively        t)

;; Language packages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python     . t)))

;; Suppress security confirmation when evaluating code
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("emacs-lisp" "python"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; -----------------------------



;;           Closing



;; -----------------------------
(provide 'init)
