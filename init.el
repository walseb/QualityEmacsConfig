;; Init packages
(defun my/init-packages ()
  (require 'package)
  (setq package-enable-at-startup nil)

  (setq package-archives
	'(("gnu" . "https://elpa.gnu.org/packages/")
	  ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (eval-when-compile
    (require 'use-package))
  (setq use-package-always-ensure t))

(my/init-packages)

;; Config locations
(defvar my/config-location (expand-file-name (concat user-emacs-directory "config.org")))
(defvar my/config-exported-location (expand-file-name (concat user-emacs-directory "config.el")))
(defvar my/config-compiled-location (expand-file-name (concat user-emacs-directory "config.elc")))
(defvar my/config-error-marker (expand-file-name (concat user-emacs-directory "config-error")))

(defun my/is-file-more-up-to-date (file1 file2)
  (interactive)
  (time-less-p (nth 5 (file-attributes file2)) (nth 5 (file-attributes file1))))

;; Compiled config
(defun my/compile-and-load-config ()
  (message "Compiling and loading config")
  (setq byte-compile-warnings '(not
				nresolved
				free-vars
				unresolved
				callargs
				redefine
				noruntime
				cl-functions
				interactive-only))
  (byte-compile-file my/config-exported-location t)
  (setq byte-compile-warnings t))

(defun my/mark-config-error ()
  (write-region nil nil my/config-error-marker))

(defun my/unmark-config-error ()
  (delete-file my/config-error-marker))

(defun my/has-config-error ()
  (file-exists-p my/config-error-marker))

(defun my/load-stable-config ()
  ;; Compiled config doesn't have any errors
  (if (file-exists-p my/config-compiled-location)
      (progn
	(load-file my/config-compiled-location)
	;; Using error doesn't work here, but doing this does
	(message "Master config broken, using stable config!"))
    (message "Master config broken, no stable config to load!")))

(defun my/load-compiled-or-compile ()
  (if (and (file-exists-p my/config-compiled-location) (my/is-file-more-up-to-date my/config-compiled-location my/config-location))
      (progn
	(message "Loading compiled config")
	(load-file my/config-compiled-location))
    (my/compile-and-load-config)))

(defun my/attempt-load-exported-config ()
  (if (ignore-errors (load-file my/config-exported-location))
      (my/unmark-config-error)
    (my/mark-config-error)
    (my/load-stable-config)))

(require 'ob-tangle)
(defun my/export-config()
  (org-babel-tangle-file my/config-location my/config-exported-location "emacs-lisp")
  (message "Config.el updated!")
  (my/attempt-load-exported-config))

(if (not (file-exists-p my/config-exported-location))
    (my/export-config)
  (if (my/is-file-more-up-to-date my/config-location my/config-exported-location)
      (my/export-config)
    (if (my/has-config-error)
	(my/load-stable-config)
      (my/load-compiled-or-compile))))
