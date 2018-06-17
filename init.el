(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Package manager
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

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

;; Inconsolata
;;'(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "PfEd" :family "Inconsolata"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#14191f" "#d15120" "#81af34" "#deae3e" "#7e9fc9" "#a878b5" "#7e9fc9" "#dcdddd"])
 '(ansi-term-color-vector
   [unspecified "#14191f" "#d15120" "#81af34" "#deae3e" "#7e9fc9" "#a878b5" "#7e9fc9" "#dcdddd"] t)
 '(custom-safe-themes
   (quote
    ("5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "cc3fa22cca4bbb85dcc078087f6dab36ec1c653a6d902be3515643f2cf1fab1f" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(fci-rule-character-color "#192028")
 '(fci-rule-color "#192028")
 '(nil nil t)
 '(package-selected-packages
   (quote
    (highlight-indent-guides minimap farmhouse-theme org-beautify-theme hydra ace-window diff-hl git-gutter evil-space evil-lion evil-indent-plus auto-dim-other-buffers company-box tile minibuffer-line symon twilight-anti-bright-theme auto-package-update ivy-pass password-store avy-flycheck linum-relative nlinum-relative vertigo ivy-rich dired-du zeal-at-point manage-minor-mode uniquify yafolding polymode fsharp-mode yasnippet-snippets yasnippet omnisharp-emacs csharp-mode flycheck projectile company-jedi slime-company slime ox-twbs htmlize org-bullets magit expand-region general exwm diminish company rainbow-delimiters rainbow-mode beacon which-key use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "PfEd" :family "DejaVuSans"))))
 '(company-scrollbar-bg ((t (:background "#281432193e1f"))))
 '(company-scrollbar-fg ((t (:background "#1e0a258c2e8f"))))
 '(company-tooltip ((t (:inherit default :background "#18041e052539"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(org-level-1 ((t (:inherit outline-1 :height 2.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.6))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.4))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.3))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.25))))
 '(org-level-6 ((t (:inherit outline-5 :height 1.2))))
 '(org-level-7 ((t (:inherit outline-5 :height 1.15))))
 '(org-level-8 ((t (:inherit outline-5 :height 1.5))))
 '(org-level-9 ((t (:inherit outline-5 :height 1.25)))))
