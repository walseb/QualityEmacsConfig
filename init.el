(setq debug-on-error t)

(set-face-attribute 'default nil :foreground "white" :background "black")

;; Config locations
(defvar my/config-location (expand-file-name (concat user-emacs-directory "config.el")))
(defvar my/config-compiled-location (expand-file-name (concat user-emacs-directory "config.elc")))

(defun my/is-file-more-up-to-date (file1 file2)
  (interactive)
  (time-less-p (nth 5 (file-attributes file2)) (nth 5 (file-attributes file1))))

(defun my/load-compiled-or-compile ()
  (if (and (file-exists-p my/config-compiled-location) (my/is-file-more-up-to-date my/config-compiled-location my/config-location))
      (progn
	(message "Loading compiled config")
	(load-file my/config-compiled-location))
    (load-file my/config-location)))

;; This doesn't work with straight.el, no idea why
;;(defun my/compile-and-load-config ()
;;  (message "Compiling and loading config")
;;  (setq byte-compile-warnings '(not
;;				nresolved
;;				free-vars
;;				unresolved
;;				callargs
;;				redefine
;;				noruntime
;;				cl-functions
;;				interactive-only))
;;
;;  (byte-compile-file my/config-location t))

(my/load-compiled-or-compile)

(setq debug-on-error nil)
