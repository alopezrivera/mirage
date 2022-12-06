;; prevent Emacs from dabbling inside /mnt/c/WINDOWS/system32/
(setq default-directory "/tmp/")
(setq command-line-default-directory "/tmp/")

;; sudo find-file
(defun sudo-find-file (orig-fun FILENAME &optional WILDCARDS)
  (condition-case nil
    (funcall orig-fun FILENAME WILDCARDS)
    (error (funcall orig-fun (concat "/sudo::" FILENAME) WILDCARDS))))
(advice-add 'find-file :around #'sudo-find-file)

;; local emacs config
(setq config "home")

(setq home "/mnt/e/")

(setq startup-buffers
      (list (concat config-directory "backlog.org")
            (concat home "backlog.org")
	    (concat home "studio/professional/work/DFKI/repos/hopping_leg/backlog.org")))


;; org-agenda
(setq org-agenda-files (list (concat home "studio/contacts.org")
			     (concat home "studio/professional/work/DFKI/repos/hopping_leg/backlog.org")))

;; org-contacts
(setq org-contacts-files (list (concat home "studio/contacts.org")))

;; org-roam directory
(setq org-roam-directory (concat home "home/scribbles/roam"))

;; org-diary directory
(setq bitacora-directory (concat home "home/scribbles/journal/diary/"))

;; projectile
(setq projectile-project-search-path (list (concat home "studio/")
				           (concat home "home/")))
