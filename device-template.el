(defvar my/default-face-height 150)

;;(defvar my/device/resolution '(2560 1440))
(defvar my/device/resolution '(1920 1080))

;;(defvar my/device/monitor-setup '(1 "DVI-D-1" 0 "DP-1" 2 "DVI-I-1"))
;; Get from doing "xrandr" in shell
(defvar my/device/monitor-setup nil)

;; Xrandr command to run
(defvar my/device/monitor-setup-command "")

(defvar my/device/maildir (concat user-emacs-directory "Maildir"))

(defvar my/ivy-youtube-key nil)

(defvar my/enable-randr t)

(defvar my/running-vm nil)

(defvar my/on-boot-run nil)
