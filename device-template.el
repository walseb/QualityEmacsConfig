(defvar my/default-face-height 160)

;;(defvar my/device/monitor-setup '(1 "DVI-D-1" 0 "DP-1" 2 "DVI-I-1"))
;; Get from doing "xrandr" in shell
(defvar my/device/monitor-setup nil)

;; Xrandr command to run
(defvar my/device/monitor-setup-command "")

(defvar my/device/maildir (concat user-emacs-directory "Maildir"))

(defvar my/ivy-youtube-key nil)

(defvar my/enable-randr t)

(defvar my/run-command-on-boot nil)

(defvar my/carpalx-enable nil)

(defvar my/run-mail-on-boot nil)

(setq ellocate-scan-dirs '(("~/" "~/ellocate-home-db")))

(defvar my/enable-basic-haskell-support nil)
