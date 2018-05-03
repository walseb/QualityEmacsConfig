(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Package manager
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;Theme
;;(unless (package-installed-p'spacemacs-theme)
					;(package-refresh-contents)
;;(package-install 'spacemacs-theme))

;;(unless (package-installed-p'twilight-anti-bright-theme)
;;  (package-refresh-contents)
;;  (package-install 'twilight-anti-bright-theme))

(use-package twilight-anti-bright-theme)
;;(load-theme twilight-anti-bright t)


;; Load config.org
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(ansi-term-color-vector
   [unspecified "#2f1e2e" "#ef6155" "#48b685" "#fec418" "#06b6ef" "#815ba4" "#06b6ef" "#a39e9b"] t)
 '(auth-source-save-behavior nil)
 ;; '(custom-enabled-themes '(spacemacs-dark))
 ;; '(custom-enabled-themes '(twilight-anti-bright-theme))
 '(custom-safe-themes
   '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "cc3fa22cca4bbb85dcc078087f6dab36ec1c653a6d902be3515643f2cf1fab1f" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(gnus-agent-mark-unread-after-downloaded nil)
 '(gnus-asynchronous t)
 '(gnus-default-adaptive-score-alist
   '((gnus-saved-mark
      (subject 250)
      (from 50))
     (gnus-dormant-mark
      (subject 150)
      (from 50))
     (gnus-forwarded-mark
      (subject 100)
      (from 25))
     (gnus-replied-mark
      (subject 75)
      (from 15))
     (gnus-ticked-mark
      (subject 0)
      (from 0))
     (gnus-read-mark
      (subject 30)
      (from 5))
     (gnus-del-mark
      (subject 5)
      (from 0))
     (gnus-recent-mark
      (subject 0)
      (from 0))
     (gnus-killed-mark
      (subject -5)
      (from -5))
     (gnus-catchup-mark
      (subject -150)
      (from 0))
     (gnus-duplicate-mark
      (subject -150)
      (from 0))
     (gnus-expirable-mark
      (subject -250)
      (from 0))
     (gnus-spam-mark
      (subject -10)
      (from -150))))
 '(gnus-read-newsrc-file nil)
 '(gnus-save-killed-list nil)
 '(gnus-save-newsrc-file nil)
 '(gnus-score-expiry-days 30)
 '(package-selected-packages
   '(twilight-anti-bright-theme auto-package-update ivy-pass password-store avy-flycheck linum-relative nlinum-relative vertigo ivy-rich dired-du zeal-at-point manage-minor-mode uniquify yafolding polymode fsharp-mode yasnippet-snippets yasnippet omnisharp-emacs csharp-mode flycheck projectile company-jedi slime-company slime ox-twbs htmlize org-bullets magit expand-region general exwm diminish company rainbow-delimiters rainbow-mode beacon which-key use-package))
 '(send-mail-function 'smtpmail-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "PfEd" :family "Inconsolata")))))
