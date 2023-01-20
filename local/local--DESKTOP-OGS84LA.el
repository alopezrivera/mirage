;; prevent Emacs from dabbling inside /mnt/c/WINDOWS/system32/
(setq default-directory "/tmp/")
(setq command-line-default-directory "/tmp/")

;; mount disk
(defun mount ()
  (interactive)
  (shell-command (concat "echo " (shell-quote-argument (read-passwd (concat "[sudo] password for "
                                                                            (shell-command-to-string "echo $USER"))))
                         " | sudo -S mount -t drvfs E: /mnt/e")))

;; sudo find-file
(defun sudo-find-file (orig-fun FILENAME &optional WILDCARDS)
  (condition-case nil
    (funcall orig-fun FILENAME WILDCARDS)
    (error (funcall orig-fun (concat "/sudo::" FILENAME) WILDCARDS))))
(advice-add 'find-file :around #'sudo-find-file)

;; load org-mode early to prevent version conflicts
(straight-use-package 'org)

;; local emacs config
(setq config "home")

;; home
(setq home "/mnt/e/")

(setq startup-buffers (list (concat home "studio/agenda.org")
                            (concat home "studio/projects/projects.org")
                            (concat config-directory "shapes.org")))

;; dashboard image
(setq dashboard-image-banner-max-width  250)
(setq dashboard-image-banner-max-height 250)

;; org-agenda
(setq org-agenda-files (list (concat home "studio/agenda.org")
                             (concat home "studio/contact book.org")))

;; org-contacts
(setq org-contacts-files (list (concat home "studio/contact book.org")))

;; org-roam directory
(setq org-roam-directory (concat home "home/scribbles/roam"))

;; org-diary directory
(setq bitacora-directory (concat home "home/scribbles/journal/diary/"))

;; projectile
(setq projectile-project-search-path (list (concat home "studio/")
				           (concat home "home/")))
