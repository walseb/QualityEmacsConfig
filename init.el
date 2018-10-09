;; Get use-package
(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; Load config.org
(defvar my/config-location (expand-file-name (concat user-emacs-directory "config.org")))
(defvar my/config-exported-location (expand-file-name (concat user-emacs-directory "config.el")))
(defvar my/config-compiled-location (expand-file-name (concat user-emacs-directory "config.elc")))
(defvar my/config-exported-stable-location (expand-file-name (concat user-emacs-directory "config-stable.el")))

(require 'ob-tangle)

(defun my/is-file-more-up-to-date (file1 file2)
  (interactive)
  (not (time-less-p (nth 5 (file-attributes file1)) (nth 5 (file-attributes file2)))))

(defun my/compile-and-load-config ()
  (setq byte-compile-warnings '(not nresolved
				    free-vars
				    unresolved
				    callargs
				    redefine
				    noruntime
				    cl-functions
				    interactive-only
				    ))
  (byte-compile-file my/config-exported-location t)
  (setq byte-compile-warnings t))

(defun my/load-compiled-or-compile ()
  (interactive)
  ;; If config isn't already created and config is more up to date than compiled
  (if (and (file-exists-p my/config-compiled-location) (my/is-file-more-up-to-date my/config-compiled-location my/config-location ) )
      (load-file my/config-compiled-location)
    (my/compile-and-load-config)))

(defun my/attempt-load-exported-config ()
  (interactive)
  (if (ignore-errors (load-file my/config-exported-location))
      (progn
	(message "Config returned no errors, creating/updating stable config")
	(delete-file my/config-exported-stable-location)
	(copy-file my/config-exported-location my/config-exported-stable-location))
    ;; Config returned error, maybe the compiled config works
    (message "New config returned error! Loading stable config")
    (if (ignore-errors (load-file my/config-exported-stable-location))
	;; Run error with a delay
	(run-with-timer 2 nil (lambda () (interactive) (error "Config returned error! Running latest stable config")))
      (run-with-timer 2 nil (lambda () (interactive) (error "Config returned error! No stable config was found! Running broken config"))))))

(if (file-exists-p my/config-exported-location)
(progn
  ;; Export config if it has changed since last export
  (if (my/is-file-more-up-to-date my/config-location my/config-exported-location)
      (progn
	(org-babel-tangle-file my/config-location my/config-exported-location "emacs-lisp")
	(message "Config.el updated!")
	(my/attempt-load-exported-config))
    (progn
      (message "Config.el matches config.org")
      (if (my/is-file-more-up-to-date my/config-exported-location my/config-exported-stable-location)
	  (load-file my/config-exported-stable-location)
	(my/load-compiled-or-compile)))))
  (org-babel-tangle-file my/config-location my/config-exported-location "emacs-lisp")
  (message "Config.el created!")
  (my/attempt-load-exported-config))
