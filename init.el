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

(require 'ob-tangle)

(defun my/compare-last-file-update (isOlder isYounger)
  (interactive)
  (time-less-p (nth 5 (file-attributes isOlder)) (nth 5 (file-attributes isYounger))))

(defun my/load-compiled-or-raw-config ()
  (interactive)
  (if (and (file-exists-p my/config-compiled-location) (my/compare-last-file-update my/config-location my/config-compiled-location))
      (load-file my/config-compiled-location)
    (load-file my/config-exported-location)))

(if (file-exists-p my/config-exported-location)
    (progn
      ;; Export config if it has changed since last export
      (if (my/compare-last-file-update my/config-exported-location my/config-location)
	  (progn
	    (org-babel-tangle-file my/config-location my/config-exported-location "emacs-lisp")
	    (message "Config.el updated!")
	    (my/load-compiled-or-raw-config))
	(progn
	  (message "Config.el matches config.org")
	  (my/load-compiled-or-raw-config))))
  (org-babel-tangle-file my/config-location my/config-exported-location "emacs-lisp")
  (message "Config.el created!")
  (my/load-compiled-or-raw-config))
