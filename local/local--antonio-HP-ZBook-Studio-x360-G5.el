;; local emacs config

(setq config "home")

(setq home "/media/antonio/T7/")

(setq startup-buffers (list (concat home "studio/agenda.org")
                            (concat home "studio/projects/projects.org")
                            (concat config-directory "backlog.org")))

;; dashboard image
(setq dashboard-startup-banner (concat home "home/images/space/jupiter/NASA Juno - Jupiter (2) Dither.png"))
(setq dashboard-image-banner-max-width  250)
(setq dashboard-image-banner-max-height 250)

;; org-agenda
(setq org-agenda-files (list (concat home "studio/agenda.org")
                             (concat home "studio/contact book.org")))

;; org-contacts
(setq org-contacts-files (list (concat home "studio/contact book.org")))

;; org-roam directory
(setq org-roam-directory (concat home "home/library/zettelkasten"))

;; bitacora directory
(setq bitacora-directory (concat home "home/scribbles/journal/diary/"))

;; projectile
(setq projectile-project-search-path (list `(,(concat home "home/")   . 1)
                                           `(,(concat home "studio/") . 1)
                                           `(,(concat home "system/") . 1)))
