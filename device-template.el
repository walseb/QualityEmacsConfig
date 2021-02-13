(setq my/interruptions nil)

(setq my/enable-brightness-binds t)

(defvar my/default-face-height 160)

;; Car is real name
;; Cdr is name to be run through `my/font-installed'
;; (font-family-list)
;; ("Hasklig" . nil)
;; ("Liga Inconsolata LGC" . nil)
;; ("Inconsolata LGC" . nil)
;; ("Inconsolata"  . nil)
;; ("Px437 ATI 8x8" . nil)
;; ("PxPlus VGA SquarePx". nil)
;; ("PxPlus AmstradPC1512-2y" . nil)
;; ;; ("PxPlus IBM VGA8". nil)
;; ;; ("BlockZone" . nil)
;; ("scientifica" . nil)
;; ;; ("Unscii" . nil)
;; ("Iosevka" . nil)
;; ("DejaVu Sans Mono" . nil)
;; ("Fira Mono" . nil)
;; ("DejaVuSansMono" . "dejavu sans mono")
;; ("NotoSansMono" . "Noto Sans Mono")
(setq my/font '("Hasklig" . nil))
(setq my/theme 'turbo)

;; (setq exwm-randr-workspace-monitor-plist '(1 "eDP" 0 "HDMI-A-0"))
(setq exwm-randr-workspace-monitor-plist nil)
(setq exwm-workspace-number 1)

(defvar my/run-command-on-boot nil)

(let ((my/run-mail-on-boot nil))
  (when my/run-mail-on-boot
    (add-hook 'after-init-hook '(lambda () (require 'mu4e)))))

(defvar my/haskell-hie-enable nil)

(defvar my/use-w3m nil)

(setq ellocate-scan-dirs '(("~/" "~/ellocate-home-db")
			   ("/run/" "~/ellocate-nix-run-db")
			   ("/etc/" "~/ellocate-etc-db")
			   ("/bin/" "~/ellocate-bin-db")
			   ("/var/" "~/ellocate-var-db")
			   ("/tmp/" nil)
			   ("/mnt/" nil)))


(setq my/spotify-client-id "")
(setq my/spotify-client-secret "")
