;; requirements
(seaman-module 'projectile)
(seaman-module 'all-the-icons)
(seaman-module 'page-break-lines)

;; dashboard
(seaman-module 'emacs-dashboard)

;; init info
(setq dashboard-set-init-info t)

;; center content
(setq dashboard-center-content t)

;; dashboard items
(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (bookmarks . 5)
                        (registers . 5)
                        (agenda . 5)))

;; banner
(setq dashboard-startup-banner (concat user-emacs-directory "seaman/art/gwd-light.png"))
(add-hook 'seaman/enable-or-load-theme-hook (lambda () (let ((active-theme (car custom-enabled-themes)))
                                                         (setq dashboard-startup-banner (concat user-emacs-directory "seaman/art/"
                                                                                                (if (eq active-theme light-theme)
                                                                                                    "gwd-light.png"
                                                                                                  "gwd-dark.png")))
                                                         (if (string-equal (buffer-name (current-buffer)) "*dashboard*")
                                                             (revert-buffer)))))
(setq dashboard-image-banner-max-width 300)

;; title
(setq dashboard-banner-logo-title "⚓ Welcome on Board! ⚓")

;; footer
(setq dashboard-set-footer t)
(setq dashboard-footer-icon (all-the-icons-fileicon "emacs"
                                                    :height 1
                                                    :v-adjust -0.15
                                                    :face 'font-lock-constant-face))

(provide 'seaman-layer-dashboard)
;;; seaman-dashboard.el ends here
