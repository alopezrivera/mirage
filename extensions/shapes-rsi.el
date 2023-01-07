;; requirements
(pixel-scroll-precision-mode)
(shapes-module "pdf-tools")

;; save-buffer
(global-set-key (kbd "C-=") #'save-buffer)
(global-set-key (kbd "C-x C-s") nil)

;; buffer switching
(global-unset-key (kbd "C-x <right>"))
(global-unset-key (kbd "C-x <left>"))
(cl-loop for map in (list
                     global-map
                     ;; plus mode maps that override the keys
                     pdf-view-mode-map
                     pixel-scroll-precision-mode-map)
         do (define-key map (kbd "<next>")  #'next-buffer)
         do (define-key map (kbd "<prior>") #'previous-buffer))

(provide 'shapes-extension-rsi)
;;; shapes-rsi.el ends here
