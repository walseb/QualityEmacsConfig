;;(defvar my/device/resolution '(2560 1440))
(defvar my/device/resolution '(1920 1080))

;;(defvar my/device/monitor-setup '(1 "DVI-D-1" 0 "DP-1" 2 "DVI-I-1"))
;; Get from doing "xrandr" in shell
(defvar my/device/monitor-setup nil)

(defvar my/device/maildir (concat user-emacs-directory "Maildir"))
