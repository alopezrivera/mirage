;;; -*- lexical-binding: t; -*-

;; display
(setq inhibit-startup-message t)
(menu-bar-mode -1)

;; dialogues
(advice-add 'yes-or-no-p :override #'y-or-n-p)

;; line numbers
(global-set-key (kbd "C-c l") #'display-line-numbers-mode)

;; no tabs
(setq-default indent-tabs-mode nil)

;; bindings
(global-set-key (kbd "C-p") (lambda () (interactive) (insert
"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")))

;; Package manager =============================================================

;; straight.el
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

;; Emacs packages ==============================================================

;; modus
(straight-use-package 'modus-themes)
(modus-themes-load-themes)
(modus-themes-load-vivendi)

;; External packages ===========================================================

;; magit
(straight-use-package 'magit)

;; god-mode
(load-file (concat config-directory "modules/god-mode.el"))

;; ivy -------------------------------------------------------------------------
(straight-use-package 'ivy)
(require 'ivy)
(ivy-mode 1)

;; minibuffer bindings
(let ((map ivy-minibuffer-map))
  (cl-loop for binding in '(("<tab>"       . ivy-alt-done)
			    ("<up>"        . ivy-previous-line-or-history)
			    ("C-l"         . ivy-alt-done)
			    ("C-j"         . ivy-next-line)
			    ("C-k"         . ivy-previous-line)
			    ("<backspace>" . ivy-backward-delete-char))
            collect (define-key map (kbd (car binding)) (cdr binding))))
;; switch-buffer bindings
(let ((map ivy-switch-buffer-map))
  (cl-loop for binding in '(("C-k"   . ivy-previous-line)
 			    ("C-l"   . ivy-done)
			    ("C-d"   . ivy-switch-buffer-kill))
            collect (define-key map (kbd (car binding)) (cdr binding))))
;; reverse-i-search bindings
(let ((map ivy-reverse-i-search-map))
  (cl-loop for binding in '(("C-k"   . ivy-previous-line)
			    ("C-d"   . ivy-reverse-i-search-kill))
            collect (define-key map (kbd (car binding)) (cdr binding))))

;; swiper ----------------------------------------------------------------------
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

;; org -------------------------------------------------------------------------
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

;; org-tempo
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; org-diary
(require 'org-diary (concat config-directory "packages/org-diary.el"))

;; org-paragraph
(require 'org-paragraph (concat config-directory "packages/org-paragraph.el"))

(provide 'termux)
