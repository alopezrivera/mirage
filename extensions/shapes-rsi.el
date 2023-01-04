;; save-buffer
(global-set-key (kbd "C-=") #'save-buffer)
(global-set-key (kbd "C-x C-s") nil)

;; buffer switching
(cl-loop for map in (list
                     global-map
                     ;; plus minor mode maps that override the keys
                     pixel-scroll-precision-mode-map)
         do (define-key map (kbd "<next>")  #'next-buffer)
         do (define-key map (kbd "<prior>") #'previous-buffer))

(provide 'shapes-extension-rsi)
;;; shapes-rsi.el ends here
