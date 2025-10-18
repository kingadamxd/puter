;;; package -- Summary
;;; Commentary:

;;; Code:

(require 'json)

(defun adam/qoutize-string (str)
  "Surround a string STR in \"\" qoutes."
  (concat "\"" str "\""))

(defun adam/change-file-suffix (path new-suffix)
  "Change the file path PATHs format suffix to NEW-SUFFIX."
  (interactive)
  (concat (car (string-split (car (last (string-split path "/"))) "\\.")) "." new-suffix))

(defun adam/set-font (font-name font-size)
  "Set frame font FONT-NAME and size FONT-SIZE."
  (let ((font-height (* font-size 10)))
    (set-face-attribute
     'default nil
     :font font-name :height font-height)
    (set-face-attribute
     'variable-pitch nil
     :font font-name :height font-height)
    (set-face-attribute
     'fixed-pitch nil
     :font font-name :height font-height))
  (let
      ((font-frame (concat font-name "-" (number-to-string font-size))))
    (add-to-list
     'default-frame-alist
     `(font . ,font-frame))))

(defun adam/set-frame-default-params ()
  "Set all frame params."
  (adam/set-font "Iosevka Nerd Font Mono" 12))

;; Emacs daemon-mode doesn't load frame params correctly.
(if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (adam/set-frame-default-params))))
  (adam/set-frame-default-params))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun adam/goto-init-file ()
  "Open init file."
  (interactive)
  (find-file user-init-file))

(defun adam/goto-homepage ()
  "Find main EMACS page."
  (interactive)
  (find-file "~/adam/master.org"))

(defun adam/reload-init-file ()
  "Reload EMACS config."
  (interactive)
  (load-file user-init-file))

(defun adam/switch-buffer ()
  "Switch to buffer command."
  (interactive)
  (call-interactively #'counsel-switch-buffer))

(defun adam/ibuffer ()
  "Interactive buffer menu."
  (interactive)
  (call-interactively #'ibuffer))

(defun adam/find-file ()
  "Find file."
  (interactive)
  (call-interactively #'find-file-existing))

(defun adam/find-file-new ()
  "File file new."
  (interactive)
  (call-interactively #'counsel-find-file))

(defun adam/imenu ()
  "Interactive menu."
  (interactive)
  (call-interactively #'counsel-imenu))

(defun adam/M-x ()
  "Meta X."
  (interactive)
  (call-interactively #'counsel-M-x))

(defun adam/lookup-func ()
  "Lookup symbol under cursor."
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode)
         (call-interactively #'describe-symbol))
        ((eq lsp-mode t)
         (call-interactively #'lsp-describe-thing-at-point))
        (t (call-interactively #'man))))

(defun adam/fuzzy-find ()
  "Fuzzy find based on the contents of the current buffer."
  (interactive)
  (cond ((eq major-mode 'dired-mode)
         (call-interactively #'adam/find-file))
        ((eq major-mode 'eshell-mode)
         (call-interactively #'adam/find-file))
        ((eq major-mode 'ibuffer-mode)
         (call-interactively #'adam/switch-buffer))
        (t (call-interactively #'adam/imenu))))

;; (defun adam/set-wallpaper (pape &optional window-transparency)
;;   "Set the desktop wallpaper to a filepath PAPE."
;;   (interactive)
;;   (when window-transparency
;;     (set-frame-parameter nil 'alpha-background window-transparency))
;;   (if (shell-command (concat "feh --bg-fill " pape))
;;       (message (concat "wallpaper set: " pape))
;;       (message (concat "wallpaper failed to be set to: " pape))))

(defun adam/tar-file (file-path &optional output-path)
  "Use linux tar util to tar a file FILE-PATH and output to OUTPUT-PATH."
  (interactive "Ftar: ")
  (let* ((output (or output-path (concat "./" (adam/change-file-suffix file-path "tar.gz"))))
         (cmd (concat "tar -cvf" " " output " " file-path)))
    (start-process-shell-command cmd nil cmd)))

(defun adam/untar-file (file-path)
  "Use linux tar util to untar the compressed file FILE-PATH."
  (interactive "Funtar: ")
  (let ((cmd (concat "tar -xvf" " " file-path)))
    (start-process-shell-command cmd nil cmd)))

(defun adam/yt-music (url)
  "Use commandline util yt-dlp to download a youtube link URL as a mp3 file."
  (interactive "surl: ")
  (let ((cmd (concat "yt-dlp -f bestaudio -x --audio-format mp3 --audio-quality 330k" " " url)))
    (start-process-shell-command cmd nil cmd)))

(defun adam/puter-linkup ()
  "Relink all the puter based files."
  (interactive)
  (if (shell-command "~/puter/scriptz/puter-linkup")
      (message "Linked-up!")
      (message "Linked-up failed!")))

(defvar adam/auth-file "~/adam/auth.json")


(defun adam/lookup-auth (auth-sym)
  "Fetch a given auth string from the auth-file with a given symbol: AUTH-SYM."
  (cdr (assoc auth-sym (json-read-file adam/auth-file))))

(defun adam/display-startup-time ()
  "Display EMACS starting time."
  (message "EMACS loaded in: %s, gc collects: %d."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'adam/display-startup-time)

(defun myeshell/clear ()
  "Clears the current eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defvar adam/xsettings/xgfxtablet-name "UGTABLET 10 inch PenTablet Pen (0)")

(defun adam/xsettings/xgfxtablet-apply ()
  "Set up gfxtablet for multi-monitor support."
  (interactive)
  (if-let ((x (string-chop-newline (shell-command-to-string (concat "xinput list --id-only" " " (adam/qoutize-string adam/xsettings/xgfxtablet-name))))))
      (let ((cmd (concat "xinput map-to-output" " " x " " "HDMI-1")))
        (ignore-errors
          (start-process-shell-command cmd nil cmd)
          (message "XGFXtablet set up!")))
    (message "XGFXtablet pen not yet registered!")))

(defvar adam/xsettings/list
  '("setxkbmap gb"
    "xset r rate 200 80"
    "xset s off -dpms"
    "xgfxtablet"
    ;; "xrandr --output HDMI-1 --primary --output HDMI-2 --right-of HDMI-1"
    ))

(defun adam/xsettings/apply ()
  "Call puter xsettings script."
  (interactive)
  (dolist (el adam/xsettings/list)
    (ignore-errors
      (start-process-shell-command el nil el)))
  (adam/xsettings/xgfxtablet-apply)
  (message "XSettings applied"))

(provide 'adam)
;;; adam.el ends here
