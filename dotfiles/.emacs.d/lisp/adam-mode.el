;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'adam)

(define-minor-mode adam-mode
  "Adam global mode for Adam based sheringans!"
  1
  :global t
  :group 'adam
  :lighter " adam-mode"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-x") 'adam/M-x)
            (define-key map (kbd "C-x SPC") 'adam/M-x)
            (define-key map (kbd "C-x j") 'adam/fuzzy-find)
            (define-key map (kbd "C-x .") 'adam/find-file)
            (define-key map (kbd "C-x ;") 'counsel-linux-app)
            (define-key map (kbd "C-x b") 'adam/switch-buffer)
            (define-key map (kbd "C-x C-b") 'adam/ibuffer)
            
            (define-key map (kbd "C-x k") 'kill-buffer)
            (define-key map (kbd "C-x K") 'kill-buffer-and-window)

            (define-key map (kbd "C-x w 1") 'delete-other-windows)
            (define-key map (kbd "C-x w n") 'evil-window-split)
            (define-key map (kbd "C-x w v") 'evil-window-vsplit)
            (define-key map (kbd "C-x w w") 'evil-window-next)
            (define-key map (kbd "C-x w k") 'evil-window-delete)
            (define-key map (kbd "C-x w h") 'windmove-left)
            (define-key map (kbd "C-x w j") 'windmove-down)
            (define-key map (kbd "C-x w k") 'windmove-up)
            (define-key map (kbd "C-x w l") 'windmove-right)

            (define-key map (kbd "M-<left>") 'windmove-left)
            (define-key map (kbd "M-<down>") 'windmove-down)
            (define-key map (kbd "M-<up>") 'windmove-up)
            (define-key map (kbd "M-<right>") 'windmove-right)

            (define-key map (kbd "M-h") 'windmove-left)
            (define-key map (kbd "M-j") 'windmove-down)
            (define-key map (kbd "M-k") 'windmove-up)
            (define-key map (kbd "M-l") 'windmove-right)

            (define-key map (kbd "M-H") 'windmove-swap-states-left)
            (define-key map (kbd "M-J") 'windmove-swap-states-down)
            (define-key map (kbd "M-K") 'windmove-swap-states-up)
            (define-key map (kbd "M-L") 'windmove-swap-states-right)

            (define-key map (kbd "M-m") 'delete-other-windows)

            (define-key map (kbd "C-x c f")
                        #'(lambda () (interactive) (kill-new (buffer-file-name))))
            (define-key map (kbd "C-x c d")
                        #'(lambda ()
                            (interactive)
                            (kill-new
                             (file-name-directory (buffer-file-name)))))
            map))

(provide 'adam-mode)
;;; adam-mode.el ends here
