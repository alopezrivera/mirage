;; local emacs config

(setq config "home")

(setq home "/media/antonio/T7/")

(setq startup-buffers (list (concat config-directory "backlog.org")
                            (concat home "backlog.org")))

;; dashboard image
(setq dashboard-startup-banner (concat config-directory "art/e-red.png"))

;; org-agenda
(setq org-agenda-files (list (concat home "studio/contacts.org")))

;; org-contacts
(setq org-contacts-files (list (concat home "studio/contacts.org")))

;; org-roam directory
(setq org-roam-directory (concat home "home/library/zettelkasten"))

;; bitacora directory
(setq bitacora-directory (concat home "home/scribbles/journal/diary/"))

;; projectile
(setq projectile-project-search-path (list `(,(concat home "home/")   . 1)
                                           `(,(concat home "studio/") . 1)
                                           `(,(concat home "system/") . 1)))
