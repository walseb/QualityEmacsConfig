(defvar my/config-location (expand-file-name "~/.emacs.d/config.org"))

(if(file-exists-p "~/.emacs.d/config.el")
    (progn
      (defvar my/config-exported-location (expand-file-name "~/.emacs.d/config.el"))
      ;; Export config if it has changed since last export
      (if (time-less-p (nth 5 (file-attributes my/config-exported-location)) (nth 5 (file-attributes my/config-location)))
	  (progn
	    (org-babel-load-file my/config-location)
	    (message "Config.el updated!"))
	(progn
	  (load-file my/config-exported-location)
	  (message "Config.el not updated"))))

  (org-babel-load-file my/config-location)
  (message "Config.el created!"))
