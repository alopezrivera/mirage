;; evil
(straight-use-package 'evil)
(require 'evil)

;; bindings
(global-set-key (kbd "C-<escape>") #'evil-mode)

;; eliminate blinking cursor in pdf-pdf-view
(add-hook 'pdf-view-mode-hook (lambda ()
                                (set (make-local-variable 
                                      'evil-emacs-state-cursor)
                                     (list nil))))

;; evil god state
(straight-use-package 'evil-god-state)
(require 'evil-god-state)

(evil-define-key 'normal global-map (kbd ",") #'evil-execute-in-god-state)

(evil-define-key 'god    global-map (kbd "<escape>") #'evil-god-state-bail)

;; mode indicators
(setq evil-normal-state-tag   (propertize " COMMAND " 'face '((:background "dark khaki"     :foreground "black")))
      evil-emacs-state-tag    (propertize "  EMACS  " 'face '((:background "turquoise"      :foreground "black")))
      evil-insert-state-tag   (propertize " ------- " 'face '((:background "dark sea green" :foreground "black")))
      evil-replace-state-tag  (propertize " REPLACE " 'face '((:background "dark orange"    :foreground "black")))
      evil-motion-state-tag   (propertize "  MOTION " 'face '((:background "khaki"          :foreground "black")))
      evil-visual-state-tag   (propertize "  VISUAL " 'face '((:background "light salmon"   :foreground "black")))
      evil-operator-state-tag (propertize " OPERATE " 'face '((:background "sandy brown"    :foreground "black"))))

(setq evil-default-cursor (quote (t "#750000"))
      evil-visual-state-cursor '("green" hollow)
      evil-normal-state-cursor '("green" box)
      evil-insert-state-cursor '("pink" (bar . 2)))

(straight-use-package 'evil-org)

(require 'evil-org)
(evil-org-set-key-theme  '(navigation insert textobjects additional calendar))

(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

;; hooks
(add-hook 'org-mode-hook  (lambda () (if evil-mode (evil-org-mode))))
(add-hook 'evil-mode-hook (lambda () (if (string-equal major-mode "org-mode") (evil-org-mode))))

(provide 'shapes-module-evil)
;;; shapes-evil.el ends here
