;; -*- lexical-binding:t -*-
;; * Ignore x-resources
(advice-add #'x-apply-session-resources :override #'ignore)

;; * GC
(setq garbage-collection-messages t)
(setq my/gc-mem gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

;; * Disable gui
;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 1) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)
(push '(horizontal-scroll-bar . 0) default-frame-alist)

;; * Disable frame resize on font change
(setq frame-inhibit-implied-resize t)

;; * Stop custom from writing to init
;; Stop custom from editing init.el
(setq custom-file (concat user-emacs-directory ".emacs-custom.el"))

;; * Disable mode-line
(setq mode-line-format nil)
(setq-default mode-line-format nil)

;; * Device config
(defun my/load-if-exists (f)
  "load the elisp file only if it exists and is readable"
  (if (file-readable-p f)
      (load-file f)))

;; If a device config is not made, load the default one
(if (not (my/load-if-exists (concat user-emacs-directory "device.el")))
    (load-file (concat user-emacs-directory "device-template.el")))

;; * Fonts
;; ** Is font installed
(defvar my/font-family-list nil)

(defun my/font-installed (font)
  (unless my/font-family-list
    (setq my/font-family-list (font-family-list)))
  (if (member font my/font-family-list)
      t
    nil))

;; ** Find fonts
(defun my/get-best-font ()
  (if (my/font-installed "Liga Inconsolata LGC")
      "Liga Inconsolata LGC"
    (if (my/font-installed "Inconsolata LGC")
	"Inconsolata LGC"
      (if (my/font-installed "Inconsolata")
	  "Inconsolata"
	(if (my/font-installed "DejaVu Sans Mono")
	    "DejaVu Sans Mono"
	  (if (my/font-installed "Fira Mono")
	      "Fira Mono"
	    (if (my/font-installed "dejavu sans mono")
		"DejaVuSansMono"
	      (if (my/font-installed "Noto Sans Mono")
		  "NotoSansMono"
		(if (my/font-installed "Perfect DOS VGA 437")
		    "Perfect DOS VGA 437")))))))))

(defun my/get-best-symbol-font ()
  (if (my/font-installed "Liga Inconsolata LGC")
      "Liga Inconsolata LGC"
    (if (my/font-installed "DejaVu Sans Mono")
	"DejaVu Sans Mono"
      (if (my/font-installed "dejavu sans mono")
	  "DejaVuSansMono"
	(if (my/font-installed "Noto Sans Mono")
	    "NotoSansMono")))))

;; ** Set fonts
(defun my/update-fonts ()
  (interactive)
  (window-system
   (let* ((font (my/get-best-font))
	  (symbol-font (my/get-best-symbol-font)))
     (if font
	 (set-face-attribute 'default nil
			     :family font
			     :height my/default-face-height
			     ;; :weight 'normal
			     ;; :width 'normal
			     )
       (message "Error: Main font not found"))

     ;; Set symbol font
     (if symbol-font
	 (set-fontset-font t 'symbol symbol-font)
       (message "Error: Symbol font not found")))))

;; Run this after init because (font-family-list) returns nil if run in early-init
(add-hook 'after-init-hook 'my/update-fonts)
