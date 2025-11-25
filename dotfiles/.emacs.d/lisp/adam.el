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

(defun adam/goto-init-file ()
  "Open init file."
  (interactive)
  (find-file user-init-file))

(defun adam/goto-homepage ()
  "Find main EMACS page."
  (interactive)
  (find-file "~/adam/homepage.org"))

(defun adam/reload-init-file ()
  "Reload EMACS config."
  (interactive)
  (load-file user-init-file))

(defun adam/lookup-func ()
  "Lookup symbol under cursor."
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode)
         (call-interactively #'describe-symbol))
        ((eq lsp-mode t)
         (call-interactively #'lsp-describe-thing-at-point))
        (t (call-interactively #'man))))

(defvar adam/fuzzy-find-alist
  '((dired-mode . find-file)
    (eshell-mode . find-file)
    (ibuffer-mode . switch-to-buffer)
    (t . imenu)))

(defun adam/fuzzy-find ()
  "Fuzzy find based on the contents of the current buffer."
  (interactive)
  (if-let ((a (assoc major-mode adam/fuzzy-find-alist)))
      (call-interactively (cdr a))
    (if-let ((b (assoc t adam/fuzzy-find-alist)))
        (call-interactively (cdr b))
      (error "no fallback value found"))))

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

(defun adam/xsettings ()
  "Call puter xsettings script."
  (interactive)
  (shell-command "puter-xsettings")
  (message "XSettings applied"))

(defun adam/eshell ()
  "Start eshell mode in the current directory."
  (interactive)
  (let ((cached-cwd default-directory)
        (eshell-buf (get-buffer "*eshell*")))
    (when eshell-buf
      (let ((kill-buf (not (with-current-buffer eshell-buf
                             (equal cached-cwd default-directory)))))
        (when kill-buf
          (kill-buffer eshell-buf))))
    (eshell)))

(defun adam/dump-file (file-path)
  "Dump the contents of a file FILE-PATH as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun million (x)
  "Take a given number X, and return X million."
  (* x 1000000))

(defun thousand (x)
  "Take a given number X, and return X thousand."
  (* x 1000))

(provide 'adam)
;;; adam.el ends here
