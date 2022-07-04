;; local emacs config

(setq config "home")

(setq home "/mnt/e/")

(setq startup-buffers
      (list (concat home "studio/backlog.org")
	    (concat home "studio/professional/work/DFKI/repos/hopping_leg/backlog.org")))


;; org-agenda
(setq org-agenda-files (list (concat home "studio/contacts.org")
			     (concat home "studio/professional/work/DFKI/repos/hopping_leg/backlog.org")))

;; org-contacts
(setq org-contacts-files (list (concat home "studio/contacts.org")))

;; org-roam directory
(setq org-roam-directory (concat home "home/roam"))

;; org-diary directory
(setq custom/org-diary-directory (concat home "home/journal/diary/"))


;; projectile
(setq projectile-project-search-path (list (concat home "studio/")
				           (concat home "home/")))
