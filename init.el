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

;; Give some breathing room
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

;; Font
(set-face-attribute 'default nil :font "Fira Code Retina" :height 110)

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

(use-package org)

;; Hide markup symbols
;; (setq org-hide-emphasis-markers t)

;; Support shift selection
;; (setq org-support-shift-select  nil)

;; By default, wrap long lines at the end of the buffer, as opposed to truncating them
;; (setq org-startup-truncated     nil)

;; By default, indent truncated lines appropriately
;; (setq org-startup-indented      t)

;; Set indentation of code blocks to 2 by default
;; (setq org-edit-src-content-in dentation 2)

;; ;; Make code editing reasonable
;; (setq org-src-tab-acts-natively nil)

;; -----------------------------



;;           Closing



;; -----------------------------
(provide 'init)
