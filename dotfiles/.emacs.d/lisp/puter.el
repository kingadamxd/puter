(defun puter/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun puter/async-command (command)
  "Start a shell command asynchronously."
  (interactive (list (read-shell-command ">> ")))
  (start-process-shell-command command nil command))

(defun puter/sync-command (command)
  "Start a shell command synchronously."
  (interactive (list (read-shell-command ">> ")))
  (let ((parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car parts) nil 0 nil ,@(cdr parts)))))

(defun puter/linkup ()
  "Linkup, sneed it or keep it?"
  (interactive)
  (puter/async-command "stow --no-folding -d ~/puter/dotfiles -t ~")
  (message "Linked Up!"))

(defun puter/updoot ()
  "Updoot the puter."
  (interactive)
  (puter/async-command "yay --noconfirm -Syu")
  (message "Updooted!"))

(defun puter/system-packages-needing-updooting ()
  "Number of packages needing"
  (shell-command-to-number "pacman -Qu | wc -l"))

(defun puter/kill-process-dont-care (process)
  "Kill a process, if it has already been killed ignore."
  (when (process-live-p proc)
      (kill-process proc)))

(defun puter/daemon-manage (symbol-binding command)
  "Start or restart a process based upon a symbol binding."
  (interactive)
  (when-let ((proc (symbol-value-or symbol-binding)))
    (puter/kill-process-dont-care proc))
  (set symbol-binding (puter/async-command command)))

(defun puter/restart-nm-applet ()
  "Restart the NM-Applet process daemon."
  (interactive)
  (puter/daemon-manage '*puter/nm-applet* "nm-applet"))

(defun puter/restart-picom ()
  "Restart the Picom process daemon."
  (interactive)
  (puter/daemon-manage '*puter/picom* "picom"))

(defun puter/restart-polybar ()
  "Restart the Polybar process daemon."
  (interactive)
  (puter/daemon-manage '*puter-polybar* "polybar puter"))

(defun puter/restart-dunst ()
  "Restart the Dunst process daemon."
  (interactive)
  (puter/daemon-manage '*puter-dunst* "dunst"))

(defun puter/notification-send (title message &optional urgency)
  "Send a notification to the desktop notification manager."
  (let ((args (list title message)))
    (when urgency
      (setq args (list "-u" urgency) args))
    (setq args (append '("notify-send" nil 0 nil) args))
    (apply #'call-process args))
  t)

(defun puter/notification-clear (&optional all)
  "Clear the current notication of the desktop notification manager."
  (interactive)
  (if all
      (puter/sync-command "dunstctl close-all")
      (puter/sync-command "dunstctl close")))

(defvar puter/xsettings 
  '("setxkbmap gb"
    "xset r rate 200 80"
    "xset s off -dpms"))

(defun puter/xsettings ()
  "Call puter xsettings script."
  (interactive)
  (dolist (el puter/xsettings)
    (puter/async-command el))
  (message "XSettings applied"))

(defun puter/exwm-init ()
  (puter/restart-nm-applet)
  (puter/restart-picom)
  (puter/restart-polybar)
  (puter/restart-dunst)
  (puter/xsettings))

(defun puter/exwm-randr-screen-change ()
  (start-process-shell-command "xrandr" nil "xrandr --output HDMI-1 --primary --output HDMI-2 --right-of HDMI-1"))

(use-package exwm
  :config

  (setq exwm-workspace-warp-cursor t)
  (setq focus-follows-mouse t)
  (setq mouse-autoselect-window t)

  (setq exwm-workspace-number 10)
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j
          ?\C-\ ))

  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  (add-hook 'exwm-update-class-hook #'puter/exwm-update-class)

  (add-hook 'exwm-init-hook #'puter/exwm-init)

  (require 'exwm-randr)

  (setq exwm-randr-workspace-monitor-plist '(0 "HDMI-1" 9 "HDMI-2"))
  (add-hook 'exwm-randr-screen-change-hook #'puter/exwm-randr-screen-change)

  (exwm-randr-mode 1)

  (setq exwm-input-global-keys
        `(
          ([?\s-r] . exwm-reset)

          ([s-left] . windmove-left)
          ([s-down] . windmove-down)
          ([s-up] . windmove-up)
          ([s-right] . windmove-right)

          ([?\s-h] . windmove-left)
          ([?\s-j] . windmove-down)
          ([?\s-k] . windmove-up)
          ([?\s-l] . windmove-right)

          ([?\s-H] . windmove-swap-states-left)
          ([?\s-J] . windmove-swap-states-down)
          ([?\s-K] . windmove-swap-states-up)
          ([?\s-L] . windmove-swap-states-right)

          ([?\s-p] . counsel-linux-app)

          ([?\s-b] . adam/switch-buffer)

          ([?\s-&] . puter/async-command)

          ([?\s-w] . adam/kill-window)

          ([?\s-n] . adam/split-window-below)
          ([?\s-v] . adam/split-window-right)

          ([?\s-e] . adam/empty-buffer)

          ([?\s-a] . exwm-layout-toggle-fullscreen)
          ([?\s-f] . exwm-floating-toggle-floating)
          ([?\s-m] . adam/kill-other-windows)

          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-wm-mode))

(use-package desktop-environment
  :after exwm
  :config
  (setq desktop-environment-brightness-small-increment "2%+")
  (setq desktop-environment-brightness-small-decrement "2%-")
  (setq desktop-environment-brightness-normal-increment "5%+")
  (setq desktop-environment-brightness-normal-decrement "5%-")
  (setq desktop-environment-update-exwm-global-keys :prefix)
  (define-key desktop-environment-mode-map (kbd "s-l") nil)
  (desktop-environment-mode))

(provide 'puter)
