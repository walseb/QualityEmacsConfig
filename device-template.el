(defvar my/default-face-height 160)

;;(defvar my/device/monitor-setup '(1 "DVI-D-1" 0 "DP-1" 2 "DVI-I-1"))
;; For this to work every monitor has to be listed I think
;; Get from doing "xrandr" in shell
(defvar my/device/monitor-setup nil)

;; Xrandr command to run
;; xrandr --auto && xrandr --output LVDS-1 --off to disable laptop screen
(if (member "-my/docked" command-line-args)
    (progn
      (defvar my/carpalx-enable nil)
      (defvar my/device/monitor-setup-command "xrandr --output HDMI-1 --primary --auto --output eDP-1 --off"))
  (defvar my/device/monitor-setup-command "")
  (defvar my/carpalx-enable t))

(defvar my/enable-randr t)

(defvar my/run-command-on-boot nil)

(defvar my/run-mail-on-boot nil)

(setq ellocate-scan-dirs '(("~/" "~/ellocate-home-db")
			   ("/run/" "~/ellocate-nix-run-db")
			   ("/etc/" "~/ellocate-etc-db")
			   ("/bin/" "~/ellocate-bin-db")
			   ("/var/" "~/ellocate-var-db")
			   ("/tmp/" nil)
			   ("/mnt/" nil)))

(defvar my/haskell-hie-enable nil)

(defvar my/use-w3m nil)

(defvar my/disable-touchpad nil)

(setq my/spotify-client-id "")
(setq my/spotify-client-secret "")
