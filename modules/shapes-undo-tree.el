;; undo-tree
(use-package undo-tree
  :bind (("M-/" . undo-tree-visualize)
         :map undo-tree-visualizer-mode-map
         ("RET" . undo-tree-visualizer-quit)
         ("ESC" . undo-tree-visualizer-quit))
  :config
  (global-undo-tree-mode))

;; visualize in side buffer
(defun custom/undo-tree-split-side-by-side (orig-fun &rest args)
  "Split undo-tree side-by-side"
  (let ((split-height-threshold nil)
        (split-width-threshold 0))
    (apply orig-fun args)))

(advice-add 'undo-tree-visualize :around #'custom/undo-tree-split-side-by-side)

(provide 'shapes-undo-tree)
;;; shapes-undo-tree.el ends here
