;;; -*- lexical-binding: t; -*-

;;; Commentary

;; Minimal Emacs Config
;; ====================
;; Contents
;; --------
;; Setup
;;   - Infrastructure
;;   - Startup
;; Looks
;;   - UI
;;   - Themes
;; Niceties
;;   - Navigation
;;   - Session
;;   - Editing
;;   - Completion
;;   - Search
;; Functions
;;   - IDE
;;   - Org Mode
;;   - Version Control
;; --------------------

;;; Code

;; ===================
;;   Infrastructure
;; ===================

;; package manager
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

;; ===================
;;      Startup
;; ===================

(setq inhibit-startup-message t)

(defun spawn-startup-buffers ()
  (cl-loop for buffer in startup-buffers
	   collect (find-file-noselect buffer)))
(add-hook 'after-init-hook #'spawn-startup-buffers)

;; ===================
;;        UI
;; ===================

;; display
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(tab-bar-mode 1)

(setq-default frame-title-format '("Emacs [%m] %b"))

;; warnings
(setq visible-bell t)
(advice-add 'yes-or-no-p :override #'y-or-n-p)

;; typefaces
(set-face-attribute 'default nil :height 85)

;; guides
(global-set-key (kbd "C-c l") #'display-line-numbers-mode)

;; ===================
;;       Themes
;; ===================

;; modus
(straight-use-package 'modus-themes)
(modus-themes-load-themes)
(modus-themes-load-operandi)

;; ===================
;;      Navigation
;; ===================

;; winner
(winner-mode)

;; ace-window
(straight-use-package 'ace-window)
(require 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)

;; ===================
;;       Session
;; ===================

;; desktop
(desktop-save-mode 1)

;; workgroups
(straight-use-package 'workgroups)
(require 'workgroups)
(setq wg-prefix-key (kbd "C-c w"))
(workgroups-mode 1)

;; ===================
;;       Editing
;; ===================

;; tabs
(setq-default indent-tabs-mode nil)

;; multiple cursors
(straight-use-package 'multiple-cursors)
(require 'multiple-cursors)

(setq mc/list-file "~/.emacs.d/persistent/mc-lists.el")
(global-set-key (kbd "C-.")                  'mc/mark-next-like-this)
(global-set-key (kbd "C-;")                  'mc/mark-previous-like-this)
(global-set-key (kbd "C-<mouse-1>")          'mc/add-cursor-on-click)
(global-unset-key [C-down-mouse-1]) ; necessary
(define-key mc/keymap (kbd "<return>")       'electric-newline-and-maybe-indent)
(define-key mc/keymap (kbd "<escape>")       'multiple-cursors-mode)
(define-key mc/keymap (kbd "<mouse-1>")      'multiple-cursors-mode)
(define-key mc/keymap (kbd "<down-mouse-1>")  nil) ; necessary

;; ===================
;;     Completion
;; ===================

;; ivy
(straight-use-package 'ivy)
(require 'ivy)
(ivy-mode 1)

(let ((map ivy-minibuffer-map))
  (cl-loop for binding in '(("<tab>"       . ivy-alt-done)
			    ("<up>"        . ivy-previous-line-or-history)
			    ("C-l"         . ivy-alt-done)
			    ("C-j"         . ivy-next-line)
			    ("C-k"         . ivy-previous-line)
			    ("<backspace>" . ivy-backward-delete-char))
            collect (define-key map (kbd (car binding)) (cdr binding))))

(let ((map ivy-switch-buffer-map))
  (cl-loop for binding in '(("C-k"   . ivy-previous-line)
 			    ("C-l"   . ivy-done)
			    ("C-d"   . ivy-switch-buffer-kill))
            collect (define-key map (kbd (car binding)) (cdr binding))))

(let ((map ivy-reverse-i-search-map))
  (cl-loop for binding in '(("C-k"   . ivy-previous-line)
			    ("C-d"   . ivy-reverse-i-search-kill))
            collect (define-key map (kbd (car binding)) (cdr binding))))

;; ===================
;;       Search
;; ===================

;; swiper
(straight-use-package 'swiper)
(require 'swiper)

(defun custom/swiper-isearch (orig-fun &rest args)
  "`swiper-isearch' the selected region. If none are, `swiper-isearch'."
  (if (region-active-p)
      (let ((beg (region-beginning))
	    (end (region-end)))
	(deactivate-mark)
	(apply orig-fun (list (buffer-substring-no-properties beg end))))
    (apply orig-fun args)))
(advice-add 'swiper-isearch :around #'custom/swiper-isearch)
(define-key global-map (kbd "C-s") #'swiper-isearch)

(defun custom/swiper-multiple-cursors ()
  (interactive)
  (swiper-mc)
  (minibuffer-keyboard-quit))
(define-key swiper-map (kbd "M-<return>") 'custom/swiper-multiple-cursors)

;; ===================
;;         IDE
;; ===================

;; remove duplicates in shell history
(setq comint-input-ignoredups t)

;; file tree
(straight-use-package 'treemacs)
(require 'treemacs)
(global-set-key (kbd "C-x t t") 'treemacs)

(defvar custom/treemacs-ignored '(".*__pycache__.*")
  "Files, directories and patterns ignored by treemacs.")
(defun custom/treemacs-ignore-filter (file _)
  "If FILE matches any entry of `treemacs-ignored', return t."
  (cl-loop for ignored in custom/treemacs-ignored
	   if (string-match ignored file)
	      return t
	   finally return nil))
(push #'custom/treemacs-ignore-filter treemacs-ignored-file-predicates)

;; completion
(straight-use-package 'company)
(require 'company)

;; syntax checker
(straight-use-package 'flycheck)
(require 'flycheck)
(add-hook 'prog-mode-hook #'flycheck-mode)

;; lisp
(straight-use-package 'rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; python
(straight-use-package 'elpy)
(elpy-enable)

(setq elpy-rpc-timeout 5)
(setq elpy-rpc-backend "jedi")
(setq elpy-rpc-python-command "python3")

(define-key elpy-mode-map (kbd "C-M-n") 'elpy-nav-forward-block)
(define-key elpy-mode-map (kbd "C-M-p") 'elpy-nav-backward-block)

;; ===================
;;       Org Mode
;; ===================

(setq org-src-tab-acts-natively        t)
(setq org-src-preserve-indentation     nil)
(setq org-edit-src-content-indentation 0)
(setq org-pretty-entities t)

;; suppress babel block execution confirmation
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("emacs-lisp" "python" "shell" "bash"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python     . t)
   (shell      . t)))

;; tempo
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; ===================
;;   Version Control
;; ===================

;; magit
(straight-use-package 'magit)

(provide 'wild)
;;; wild.el ends here
