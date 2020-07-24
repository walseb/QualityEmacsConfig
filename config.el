;; -*- lexical-binding:t -*-
;; * Startup processes
;; ** Device config
(defun my/load-if-exists (f)
  "load the elisp file only if it exists and is readable"
  (if (file-readable-p f)
      (load-file f)))

;; If a device config is not made, load the default one
(if (not (my/load-if-exists (concat user-emacs-directory "device.el")))
    (load-file (concat user-emacs-directory "device-template.el")))

;; ** Theme
(unless custom-enabled-themes
  (load-theme 'myTheme t))

;; ** Prevent async command from opening new window
;; Buffers that I don't want popping up by default
(add-to-list 'display-buffer-alist
	     '("\\*Async Shell Command\\*.*" display-buffer-no-window))

;; ** Fonts
;; *** Is font installed
(defun my/font-installed (font-family-list font)
  (if (member font font-family-list)
      t
    nil))

;; Car is real name
;; Cdr is name to be run through `my/font-installed'
(setq my/fonts '(
		 ("Liga Inconsolata LGC" . nil)
		 ("Inconsolata LGC" . nil)
		 ("Inconsolata"  . nil)
		 ("Px437 ATI 8x8" . nil)
		 ("PxPlus VGA SquarePx". nil)
		 ("PxPlus AmstradPC1512-2y" . nil)
		 ;; ("PxPlus IBM VGA8". nil)
		 ;; ("BlockZone" . nil)
		 ("scientifica" . nil)
		 ;; ("Unscii" . nil)
		 ("Iosevka" . nil)
		 ("DejaVu Sans Mono" . nil)
		 ("Fira Mono" . nil)
		 ("DejaVuSansMono" . "dejavu sans mono")
		 ("NotoSansMono" . "Noto Sans Mono")))

;; Works just like my/fonts, but returning nil means to not set a symbol font
;; Thing to note here is that if the car is nil, then it means just use what you were using before. If car is a string, then use that.
(setq my/symbol-fonts '(
			("Liga Inconsolata LGC" . nil)
			(nil . "Inconsolata LGC")
			(nil . "Inconsolata")
			("PxPlus IBM VGA8" . nil)
			("scientifica" . "BlockZone" )
			(nil . "scientifica" )
			(nil . "Iosevka")
			("DejaVuSansMono" .  "dejavu sans mono")
			("NotoSansMono" . "Noto Sans Mono")))

;; *** Find fonts
(defun my/find-font (fonts)
  (let ((font-family-list (font-family-list)))
    (car
     (seq-find (lambda (a) (my/font-installed font-family-list (or (cdr a) (car a)))) fonts))))

(defun my/get-best-font ()
  (my/find-font my/fonts))

(defun my/get-best-symbol-font ()
  (let ((curr-font (my/find-font my/fonts)))
    (car
     (seq-find (lambda (a) (string= curr-font (or (cdr a) (car a)))) my/symbol-fonts))))

;; *** Set fonts
(defun my/update-fonts ()
  (interactive)
  (window-system
   (let* ((font (my/get-best-font))
	  (symbol-font (my/get-best-symbol-font)))
     (if font
	 (set-face-attribute 'default nil
			     :font font
			     :height my/default-face-height
			     ;; :weight 'normal
			     ;; :width 'normal
			     )
       (message "Main font not found"))

     ;; Set symbol font
     (when symbol-font
       (set-fontset-font t 'symbol symbol-font)))))

;; ;; Run this after init because (font-family-list) returns nil if run in early-init
;; (add-hook 'after-init-hook 'my/update-fonts)

(when window-system
  (my/update-fonts))

;; * Security
(setq network-security-level 'medium)

;; ** Cert settings
(setq gnutls-verify-error t)
(setq tls-checktrust t)

;; ** Make authinfo gpg file
(setq netrc-file "~/.authinfo.gpg")
(setq auth-sources '("~/.authinfo.gpg"))

;; * Folders
;; Folder locations
(setq my/notes-folder "~/Notes/")
(setq my/wallpaper-folder (concat my/notes-folder "Wallpapers/"))

;; * Package management
;; Bootstrap straight.el
(eval-and-compile
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	   'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;; * Private config
(defun my/load-if-exists (f)
  "load the elisp file only if it exists and is readable"
  (if (file-readable-p f)
      (load-file f)))

;; ** Private config
(my/load-if-exists (concat user-emacs-directory "private.el"))

;; * Libraries
(straight-use-package 's)
(straight-use-package 'dash)
(straight-use-package 'ov)

;; ** Elpatch
(straight-use-package 'el-patch)

;; * Persistent keys
(defvar my/keys-mode-map (make-sparse-keymap))

(define-minor-mode my/keys-mode nil t nil my/keys-mode-map)

(add-to-list 'emulation-mode-map-alists `((my/keys-mode . ,my/keys-mode-map)))

;; * General functions and variables
;; ** File management
;; *** File path to top directory
;; So for example turn /foo/bar/ -> bar
;; And turn /foo/bar/baz.tar -> baz.tar
(defun my/file-top-path (path)
  ;; Ignore remote files, I'm not sure they work
  (when (not (file-remote-p path))
    (let ((name (file-name-nondirectory
		 (directory-file-name (expand-file-name path)))))
      (if (string= name "")
	  nil
	name))))

;; *** Spaced path to compact path
(defun my/spaced-path-to-compact-path (path)
  "Turns \"~/ABC/A B C/\" into \"~/ABC/A%20B%20C/\""
  (replace-regexp-in-string " " "%20" path))

;; *** Create directory if directory doesn't exist
(defun my/create-dir-if-not-exist (dir)
  (if (not (file-directory-p dir))
      (progn
	(make-directory dir)
	(message (concat "dir: " dir " created!")))))

;; *** Create file if file doesn't exist
(defun my/create-file-if-not-exist (file)
  (if (not (file-exists-p file))
      (progn
	(write-region "" nil file)
	(message (concat "Wrote file: " file " created!")))))

;; *** Create file with content if file doesn't exist
(defun my/create-file-with-content-if-not-exist (file content)
  (if (not (file-exists-p file))
      (progn
	(write-region content nil file)
	(message (concat "Wrote file: " file " with contents")))))
;; " created with content: " content

;; *** Add to content to file or create file with content if file doesn't exist
(defun my/add-to-or-create-file-with-content (file content)
  (write-region (concat "\n" content) nil file t)
  (message (concat "Wrote file: " file " with contents")))

;; *** Append to end of file
(defun my/append-to-file (file string)
  ;; The 1 here is used to stop write-region from printing when it writes something
  (write-region string nil file t 1))

;; *** Open if exists
(defun my/open-if-exists (path)
  (when (file-exists-p path)
    (find-file path)))

;; ** Sudo shell-command
(defun my/sudo-shell-command (command &optional output-buffer error-buffer)
  (let ((default-directory "/sudo::"))
    (shell-command command output-buffer error-buffer)))

(defun my/sudo-shell-command-to-string (command)
  (let ((default-directory "/sudo::"))
    (shell-command-to-string command )))

;; ** List
;; *** Get random element from list
(defun my/get-random-element (list)
  (let* ((size (length list))
	 (index (random size)))
    (nth index list)))

;; ** Notifications
;; *** Print / Message at point
(defun my/message-at-point (str)
  (eros--eval-overlay str (point-at-eol)))

;; *** Fire notification
(require 'notifications)

(defun my/fire-notification (msg &optional path)
  (notifications-notify :title msg
			:timeout (* 1000 5)
			:image-path (when path
				      (concat "file://" (my/spaced-path-to-compact-path (expand-file-name path))))
			;; For example:
			;; :image-path "file:///home/admin/Notes/Wallpapers/Great/Adolph%20von%20Menzel%20.%201875%20.%20Iron%20rolling%20mill.jpg"
			))

;; ** Process
;; *** Run async process shell command
;; As opposed to `async-shell-command', this runs the shell command without a visual terminal, although it does have a buffer where it collects everything printed
(defun my/async-start-process-shell-command (buffer-name package finish-func &rest args)
  (message (concat "Starting async shell command process. Buffer name: " buffer-name " Package: " package))
  (async-start-process buffer-name
		       (executable-find package)
		       finish-func
		       (mapconcat 'identity args " ")))

;; ** Is external package installed
;; Checks variable =exec-path= for package
(defun my/is-system-package-installed (package)
  (if (executable-find (symbol-name package))
      (symbol-name package)
    (message (concat "Package: " (symbol-name package) " not installed"))
    nil))

;; *** Set exec-path by system
;; (if (string-match-p "guixsd" (system-name))
;; (add-to-list 'exec-path "/bin/" ))

;; ** Give buffer unique name
(defun my/give-buffer-unique-name (base-name)
  (rename-buffer base-name t))

;; ** Fake key
;; *** Normal emacs buffers
;; Doesn't work on keys that are not english
;; (defun my/fake-key (key key-symbol)
;;  (interactive)
;;  (setq unread-command-events (listify-key-sequence "ö")))

(defun my/fake-key (key key-symbol)
  (interactive)
  (let ((command (key-binding key)))
    (setq last-command-event key-symbol)
    (setq this-command command)
    (call-interactively command)))

(defun my/fake-open-keymap (key)
  (setq unread-command-events
	(mapcar (lambda (e) `(t . ,e))
		(listify-key-sequence (kbd key)))))

;; ** Fold ellipsis
(defvar my/fold-ellipsis)
(defvar my/fold-ellipsis-char)

(if window-system
    (progn
      (setq my/fold-ellipsis "↴")
      (setq my/fold-ellipsis-char ?↴))
  ;; The terminal probably doesn't support unicode
  (setq my/fold-ellipsis "↓")
  (setq my/fold-ellipsis-char ?↓))

;; ** File size human readable
;; Default file-size-human-readable returns decimal values
(defun my/file-size-human-readable (file-size &optional flavor decimal)
  "Produce a string showing FILE-SIZE in human-readable form.

  Optional second argument FLAVOR controls the units and the display format:

  If FLAVOR is nil or omitted, each kilobyte is 1024 bytes and the produced
  suffixes are \"k\", \"M\", \"G\", \"T\", etc.
  If FLAVOR is `si', each kilobyte is 1000 bytes and the produced suffixes
  are \"k\", \"M\", \"G\", \"T\", etc.
  If FLAVOR is `iec', each kilobyte is 1024 bytes and the produced suffixes
  are \"KiB\", \"MiB\", \"GiB\", \"TiB\", etc.
  If DECIMAL is true, a decimal number is returned"
  (require 'files)

  (setq 1024Decimal (if decimal 1024.0 1024))
  (setq 1000Decimal (if decimal 1000.0 1000))

  (let ((power (if (or (null flavor) (eq flavor 'iec))
		   1024Decimal
		 1000Decimal))
	(post-fixes
	 ;; none, kilo, mega, giga, tera, peta, exa, zetta, yotta
	 (list "" "k" "M" "G" "T" "P" "E" "Z" "Y")))
    (while (and (>= file-size power) (cdr post-fixes))
      (setq file-size (/ file-size power)
	    post-fixes (cdr post-fixes)))
    (format (if (> (mod file-size 1.0) 0.05)
		"%.1f%s%s"
	      "%.0f%s%s")
	    file-size
	    (if (and (eq flavor 'iec) (string= (car post-fixes) "k"))
		"K"
	      (car post-fixes))
	    (if (eq flavor 'iec) "iB" ""))))

;; ** Overlay
(defun my/inline-overlay-print (string)
  (let ((inline-overlay (make-overlay (point) (line-end-position))))
    ;; Put overlay
    (overlay-put inline-overlay 'after-string
		 (propertize
		  (concat
		   " ;=>"
		   string
		   )
		  'face '(:foreground "light blue")
		  ))
    ;; Just sit for 100 seconds
    (sit-for 100)
    ;; Then delete overlay
    (delete-overlay inline-overlay)))

;; ** Repeat char
(defun my/repeat-char (char initial-string n)
  (setq initial-string (concat char initial-string))
  (if (> n 1)
      (my/repeat-char char initial-string (- n 1))
    initial-string))

;; ** Delete old functions
(defun my/delete-everything-older-than (folder time)
  (message (concat "Deleting all files older than " (number-to-string time) " seconds"))
  (let ((time time)
	(current (float-time (current-time))))
    (dolist (file (directory-files folder t))
      (when (and (backup-file-name-p file)
		 (> (- current (float-time (nth 5 (file-attributes file))))
		    time))
	(message "%s" file)
	(delete-file file)))
    (message "Deletion completed!")))

;; ** Is there any line longer than
;; From mm-bodies.el
(defun my/line-longer-than (length)
  "Say whether any of the lines in the buffer is longer than LENGTH."
  (save-excursion
    (goto-char (point-min))
    (end-of-line)
    (while (and (not (eobp))
		(not (> (current-column) length)))
      (forward-line 1)
      (end-of-line))
    (and (> (current-column) length)
	 (current-column))))

;; ** Get position of beginning of next line
(defun my/next-line-pos ()
  (save-excursion
    (forward-line)
    (beginning-of-line)
    (point)))

;; ** Is file gpg protected
;; Send in ~buffer-file-name~ to check current file
(defun my/is-file-gpg-protected (file)
  (let ((file-name (and file (file-name-nondirectory file))))
    (and file-name (string-match-p "\.gpg" file-name))))

;; ** Recursive file get
;; Try this in a large directory
;; (benchmark 10 (split-string (shell-command-to-string "find .") "\n" t))
;; (benchmark 10 '(directory-files-recursively default-directory ""))
(defun my/rec-find-files ()
  (split-string (shell-command-to-string "find .") "\n" t))

;; ** Time
;; *** Is it weekend
(defun my/is-it-weekend ()
  ;; If more than friday
  (> (string-to-number (format-time-string "%u" (seconds-to-time (current-time)))) 5))

;; *** Timers
;; **** Timer allocater
;; Repeat timers are buggy and you don't want all timers to run at the same time

(defvar my/status-line-update-time-max 8)
(defvar my/status-line-update-time 0)

(defun my/allocate-update-time (task &optional repeat offset)
  "TASK is the function to be run.
REPEAT is the time to between each run
OFFSET is the offset to apply. This makes sure the timers spread out."
  (funcall task)
  (unless offset
    (setq my/status-line-update-time (+ my/status-line-update-time 1)))
  (let ((update-time (or offset my/status-line-update-time)))
    (run-with-timer
     (mod update-time my/status-line-update-time-max)
     (+ update-time (or repeat 60))
     (lambda () (ignore-errors (funcall task))))))


;; * Evil
;; Disable switch to emacs-state binding
(setq evil-toggle-key "")

(setq evil-search-module 'evil-search)
(setq evil-vsplit-window-right t)
(setq evil-split-window-below t)
(setq evil-shift-round nil)

;; Makes swiper A LOT faster
(setq evil-ex-interactive-search-highlight t)
(setq evil-ex-search-persistent-highlight nil)

(eval-and-compile
  (straight-use-package 'evil)
  (require 'evil)
  (require 'evil-macros))

;; (fset 'evil-visual-update-x-selection 'ignore)
(evil-mode)

;; ** Evil-goggles support
(straight-use-package 'evil-goggles)

(add-hook 'after-init-hook 'evil-goggles-mode)

;; Disable pulse which both fixes so that you can set foreground color on the pulse font and saves on performance
(setq evil-goggles-pulse nil)
(setq evil-goggles-duration 60)

(with-eval-after-load 'evil-goggles
  (evil-goggles-use-diff-faces))

;; ** Minibuffer
;; Enable evil in minibuffer
(setq evil-want-minibuffer t)

;; This fixes evil minibuffer binds
(add-hook 'minibuffer-setup-hook 'evil-insert-state)

;; *** Set max minibuffer height
;; (setq max-mini-window-height 1)

;; ** Bind evil key functions
(defun my/evil-emacs-define-key (key command)
  (interactive)
  (define-key evil-emacs-state-map (kbd key) command))

(defun my/evil-insert-define-key (key command)
  (interactive)
  (define-key evil-insert-state-map (kbd key) command))

(defun my/evil-normal-define-key (key command)
  (interactive)
  (define-key evil-normal-state-map (kbd key) command)
  (define-key evil-motion-state-map (kbd key) command))

(defun my/evil-replace-define-key (key command)
  (interactive)
  (define-key evil-replace-state-map (kbd key) command))

(defun my/evil-visual-define-key (key command)
  (interactive)
  (define-key evil-visual-state-map (kbd key) command))

(defun my/evil-universal-define-key (key command)
  (interactive)
  (my/evil-insert-define-key key command)
  (my/evil-normal-define-key key command)
  (my/evil-visual-define-key key command)
  (my/evil-replace-define-key key command))

;; ** Evil-multiple cursors
(straight-use-package 'evil-mc)

;; *** Clear default keys
(setq evil-mc-key-map (make-sparse-keymap))

;; *** Start mc-mode in this buffer
(evil-mc-mode)

;; *** Add unsupported commands
(add-to-list 'evil-mc-custom-known-commands
	     '(delete-char . ((:default . evil-mc-execute-default-call-with-count))))

(add-to-list 'evil-mc-custom-known-commands
	     '(org-delete-char . ((:default . evil-mc-execute-default-call-with-count))))

(add-to-list 'evil-mc-custom-known-commands
	     '(csharp-maybe-insert-codedoc . ((:default . evil-mc-execute-default-call-with-count))))

;; *** Enable globally
;; Enable evil-mc in all modes, including fundamental-mode
(add-hook 'evil-local-mode-hook 'evil-mc-mode)

;; *** Disable on keybord-quit (C-g)
(setq evil-mc-undo-cursors-on-keyboard-quit t)

;; *** Keys
(define-key evil-visual-state-map (kbd "A") 'evil-mc-make-cursor-in-visual-selection-end)
(define-key evil-visual-state-map (kbd "I") 'evil-mc-make-cursor-in-visual-selection-beg)

(define-key evil-normal-state-map (kbd "C-S-n") 'evil-mc-make-and-goto-next-match)
(define-key evil-normal-state-map (kbd "C-S-p") 'evil-mc-make-and-goto-prev-match)

;; ** Settings
;; *** Disable messages in echo area
;; Evil spams message area
(setq
 evil-emacs-state-message nil
 evil-operator-state-message nil
 evil-insert-state-message nil
 evil-replace-state-message nil
 evil-motion-state-message nil
 evil-normal-state-message nil
 evil-visual-state-message nil)

;; *** Cursor states
(setq evil-emacs-state-cursor '("purple" box))
(setq evil-normal-state-cursor '("white" box))
(setq evil-visual-state-cursor '("yellow" box))
(setq evil-insert-state-cursor '("orange" box))
(setq evil-replace-state-cursor '("green" box))
(setq evil-operator-state-cursor '("white" hollow))
(setq evil-motion-state-cursor '("blue" box))

;; *** Disable evil-maps override
(setq evil-overriding-maps nil)

;; *** Disable emacs mode
(setq evil-emacs-state-modes nil)

;; *** Set which modes use which evil state by default
;; Example
(setq evil-insert-state-modes nil)

(cl-loop for (mode . state) in '(
				 (eshell-mode . insert)
				 (interactive-haskell-mode . insert)
				 (term-mode . insert)
				 ;;(org-agenda-mode . insert)
				 (magit-popup-mode . insert)
				 (proced-mode . insert)
				 (org-agenda-mode . insert)
				 (emms-playlist-mode . insert)

				 (mu4e-headers-mode . insert)
				 (mu4e-view-mode . insert)
				 (mu4e-loading-mode . insert)

				 (image-mode . insert)
				 )
	 do (evil-set-initial-state mode state))

;; *** Disable motion state
;; Motion state is like normal mode but you can't go into insert-mode. Some modes start up with this restrictive mode, disable it here
(setq evil-motion-state-modes nil)

(add-hook 'evil-motion-state-entry-hook (lambda () (interactive) (evil-force-normal-state) (message "Prevented motion state from being entered")))

;; *** Switching to normal state without moving cursor
(defun my/evil-normal-state (&optional arg)
  (if (not(eq evil-state 'normal))
      (progn
	(evil-normal-state arg)
	(move-to-column (+ 1 (current-column))))))

;; *** Make one space enough to end work for use with evil sentence motion
(setq sentence-end-double-space nil)

;; *** Make dd and cc act on lines
(my/evil-normal-define-key "D" 'evil-delete-whole-line)
(my/evil-normal-define-key "C" 'evil-change-whole-line)

;; ** Text objects
;; *** Evil-entire-buffer
;; Modify entire buffer - for example: "d a e"
;; https://github.com/supermomonga/evil-textobj-entire
;; I eval the expression here because `evil-define-text-object' doesn't work after it's byte-compiled and evaling prevents that from happening
(evil-define-text-object evil-entire-buffer (count &optional beg end type)
  "Select entire buffer"
  (evil-range (point-min) (point-max)))

(define-key evil-outer-text-objects-map "e" 'evil-entire-buffer)
(define-key evil-inner-text-objects-map "e" 'evil-entire-buffer)

;; *** Evil-line
;; https://github.com/syohex/evil-textobj-line
(defun my/evil-line-range (count beg end type &optional inclusive)
  (if inclusive
      (evil-range (line-beginning-position) (line-end-position))
    (let ((start (save-excursion
		   (back-to-indentation)
		   (point)))
	  (end (save-excursion
		 (goto-char (line-end-position))
		 (skip-syntax-backward " " (line-beginning-position))
		 (point))))
      (evil-range start end))))

(evil-define-text-object my/evil-a-line (count &optional beg end type)
  "Select range between a character by which the command is followed."
  (my/evil-line-range count beg end type t))

(evil-define-text-object my/evil-inner-line (count &optional beg end type)
  "Select inner range between a character by which the command is followed."
  (my/evil-line-range count beg end type))

(define-key evil-outer-text-objects-map "l" 'my/evil-a-line)
(define-key evil-inner-text-objects-map "l" 'my/evil-inner-line)

;; *** Evil-indent-plus
;; Allows for using indention as text objects
(straight-use-package 'evil-indent-plus)

(define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
(define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
(define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
(define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
(define-key evil-inner-text-objects-map "C-i" 'evil-indent-plus-i-indent-up-down)
(define-key evil-outer-text-objects-map "C-i" 'evil-indent-plus-a-indent-up-down)

;; *** Evil textobject block
(straight-use-package 'evil-textobj-anyblock)

(define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
(define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block)

(setq evil-textobj-anyblock-blocks
      '(("(" . ")")
	("{" . "}")
	("\\[" . "\\]")
	("<" . ">")
	("\"" . "\"")
	("“" . "”")))

;; *** Evil commentary
(with-eval-after-load 'evil
  (straight-use-package 'evil-commentary)
  (evil-commentary-mode))

(with-eval-after-load 'evil-commentary
  (evil-define-key 'normal evil-commentary-mode-map ":" 'evil-commentary-line)
  (evil-define-key '(normal visual) evil-commentary-mode-map ";" 'evil-commentary)
  (evil-define-key 'normal evil-commentary-mode-map "gY" 'evil-commentary-yank-line))

;; **** Allow commenting empty line
;; Because of some reason emacs crashes with undo tree error if this isn't run late
;; (add-hook 'after-init-hook 'evil-commentary-mode)

(with-eval-after-load 'evil-commentary
  (evil-define-operator evil-commentary-line (beg end type)
    "Comment or uncomment [count] lines."
    :motion evil-line
    :move-point nil
    (interactive "<R>")
    (when (evil-visual-state-p)
      (unless (memq type '(line block))
	(let ((range (evil-expand beg end 'line)))
	  (setq beg (evil-range-beginning range)
		end (evil-range-end range)
		type (evil-type range))))
      (evil-exit-visual-state))
    ;; If current line is blank
    (if (save-excursion
	  (beginning-of-line)
	  (looking-at "[[:space:]]*$"))
	(insert comment-start)
      (evil-commentary beg end type))))

;; *** Evil-eval operator
(evil-define-operator evil-eval (beg end type)
  "Run eval on BEG to END."
  (interactive "<R>")
  (my/auto-eval-region beg end))

(my/evil-normal-define-key "/" 'evil-eval)
(my/evil-normal-define-key "?" 'my/auto-eval)

;; **** Add evil-goggle command
(with-eval-after-load 'evil-goggles
  (add-to-list 'evil-goggles--commands '(evil-eval :face evil-goggles-yank-face :switch evil-goggles-enable-yank :advice evil-goggles--generic-async-advice)))

;; *** Evil-surround
(straight-use-package 'evil-surround)
(add-hook 'after-init-hook (lambda () (global-evil-surround-mode 1)))

;; **** Setup pair binds
(setq evil-surround-pairs-alist
      '(
	;; Default
	(?\( . ("(" . ")"))
	(?\[ . ("[ " . "]"))
	(?\{ . ("{" . "}"))

	;; Default reversed
	(?\) . ("(" . ")"))
	(?\] . ("[" . "]"))
	(?\} . ("{" . "}"))

	;; My meta keys
	(134217841 . ("!" . "!"))
	(134217831 . ("@" . "@"))
	(134217837 . ("#" . "#"))
	(134217836 . ("$" . "$"))
	(134217847 . ("%" . "%"))
	(134217849 . ("*" . "*"))
	(134217830 . ("(" . ")"))
	(134217845 . ("(" . ")"))
	(134217826 . ("&" . "&"))
	(134217787 . ("^" . "^"))

	(134217828 . ("1" . "1"))
	(134217843 . ("2" . "2"))
	(134217844 . ("3" . "3"))
	(134217838 . ("4" . "4"))
	(134217842 . ("5" . "5"))
	(134217833 . ("6" . "6"))
	(134217825 . ("7" . "7"))
	(134217829 . ("8" . "8"))
	(134217839 . ("9" . "9"))
	(134217832 . ("0" . "0"))

	;; Blocks, etc
	(?# . ("#{" . "}"))
	(?b . ("(" . ")"))
	(?B . ("{" . "}"))
	(?> . ("<" . ">"))
	(?t . evil-surround-read-tag)
	(?< . evil-surround-read-tag)
	(?f . evil-surround-function)))

;; **** Keys
(with-eval-after-load 'evil-surround
  (evil-define-key 'normal evil-surround-mode-map (kbd ",") 'evil-surround-edit)
  (evil-define-key 'normal evil-surround-mode-map (kbd "C-,") 'evil-Surround-edit)
  (evil-define-key 'visual evil-surround-mode-map (kbd ",") 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map (kbd "C-,") 'evil-Surround-region))

;; *** Evil-args
(straight-use-package 'evil-args)

;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; ** Jump
;; Make evil-jump buffer-local
(setq evil-jumps-cross-buffers nil)

;; ** Evil-lion
(straight-use-package 'evil-lion)
(evil-lion-mode)

;; ** Match paren
;; The normal evil-jump-item gives up easily. This tries to get to a paren more
(defun my/match-paren ()
  (interactive)
  (when (not (ignore-errors (call-interactively #'evil-jump-item)))
    (backward-up-list)))

(my/evil-normal-define-key "%" 'my/match-paren)

;; ** Goto end of line
;; By default evil goes to the last line, first char. This goes to the very last char in the buffer
(evil-define-motion evil-goto-line (count)
  "Go to the first non-blank character of line COUNT.
   By default the last line."
  :jump t
  :type line
  (if (null count)
      (with-no-warnings (end-of-buffer))
    (goto-char (point-min))
    (forward-line (1- count)))
  (end-of-line))

;; ** Fix evil open line
(setq evil-auto-indent nil)

;; ** Visuals
(setq evil-emacs-state-cursor '("purple" box))
(setq evil-normal-state-cursor '("red" box))
(setq evil-visual-state-cursor '("yellow" box))
(setq evil-insert-state-cursor '("orange" box))
(setq evil-replace-state-cursor '("green" box))
(setq evil-operator-state-cursor '("white" hollow))

;; *** Get current evil cursor color
(defun my/get-current-evil-cursor-color ()
  (pcase evil-state
    ('normal (car evil-normal-state-cursor))
    ('insert (car evil-insert-state-cursor))
    ('replace (car evil-replace-state-cursor))
    ('visual (car evil-visual-state-cursor))
    ('emacs (car evil-emacs-state-cursor))
    ('operator (car evil-operator-state-cursor))))

;; ** Keys
;; Prevent emacs state from being exited with esc, fixes exwm since it uses emacs state and to exit hydra you have to do esc
(define-key evil-emacs-state-map (kbd "<escape>") 'keyboard-quit)

;; Couldn't bother to create custom evil-join
;; P is normally bound to manual, make this key useful
(my/evil-normal-define-key "P" 'delete-indentation)

(my/evil-normal-define-key "DEL" 'backward-delete-char-untabify)

;; *** Replace return
;; This moves the cursor to the beginning of the next line instead of creating a newline
(my/evil-replace-define-key "RET" (lambda ()
				    (interactive)
				    (evil-next-line)
				    (evil-beginning-of-line)))

;; *** Rebind evil case change
(my/evil-normal-define-key "g u" 'evil-downcase)
(my/evil-normal-define-key "g U" 'evil-upcase)

;; *** RET in normal mode should insert enter
(my/evil-normal-define-key "RET" #'newline)

;; *** Add perspective movement to g
(my/evil-normal-define-key "gb" 'evil-scroll-line-to-bottom)
(my/evil-normal-define-key "gf" 'evil-scroll-line-to-top)
(my/evil-normal-define-key "ge" 'evil-scroll-line-to-center)
;; (my/evil-normal-define-key "/" 'evil-scroll-line-to-center)

;; *** Don't complete from all buffers
(setq evil-complete-all-buffers nil)

;; *** Don't add pasted over thing to killring
(setq evil-kill-on-visual-paste nil)

;; *** Go down visual line with M-p, M-n
(my/evil-universal-define-key "M-n" #'evil-next-visual-line)
(my/evil-universal-define-key "M-p" #'evil-previous-visual-line)

;; *** Move by paragraph
(defun my/move-paragraph (forward)
  (let ((regex-forward "[[:graph:]].*\n[[:blank:]]*\n")
	(regex-backward "^[[:blank:]]*\n.*[[:graph:]]"))
    (if forward
	(if (ignore-errors (re-search-forward regex-forward))
	    (previous-line)
	  (end-of-buffer)
	  ;; Since last line of buffers is blank, go back one line
	  (previous-line))
      (unless (ignore-errors (forward-line 1) (re-search-backward regex-backward))
	(beginning-of-buffer)))
    (beginning-of-line)))

(evil-define-motion my/backward-paragraph (count)
  "Move to the end of the COUNT-th next paragraph."
  :jump t
  :type exclusive
  (if count
      (dotimes (i count)
	(my/move-paragraph nil))
    (my/move-paragraph nil)))

(evil-define-motion my/forward-paragraph (count)
  "Move to the end of the COUNT-th next paragraph."
  :jump t
  :type exclusive
  (if count
      (dotimes (i count)
	(my/move-paragraph t))
    (my/move-paragraph t)))

(my/evil-normal-define-key "r" 'my/forward-paragraph)
(my/evil-visual-define-key "r" 'my/forward-paragraph)
(my/evil-normal-define-key "R" 'my/backward-paragraph)
(my/evil-visual-define-key "R" 'my/backward-paragraph)

(my/evil-normal-define-key "j" 'evil-replace)
(my/evil-visual-define-key "j" 'evil-replace)
(my/evil-normal-define-key "J" 'evil-replace-state)
(my/evil-visual-define-key "J" 'evil-replace-state)

;; *** Don't save chars deleted with x to clipboard
(my/evil-normal-define-key "x" 'delete-char)
(my/evil-normal-define-key "X" (lambda () (interactive)
				 (backward-char)
				 (call-interactively #'delete-char)))

(my/evil-visual-define-key "x" (lambda () (interactive)
				  (let ((ring kill-ring))
				    (call-interactively 'evil-delete)
				    (setq kill-ring ring))))

;; * Leader
;; When changing leader, change =my/leader-map-key=
(define-prefix-command 'my/leader-map)

(defvar my/leader-map-key "SPC")
(defvar my/mod-leader-map-key "C-SPC")

(defvar my/window-leader-key "C-=")
(defvar my/mod-window-leader-key "M-C-=")

(my/evil-normal-define-key "U" 'undo-tree-visualize)

(my/evil-normal-define-key my/leader-map-key my/leader-map)
(my/evil-visual-define-key my/leader-map-key my/leader-map)

(my/evil-universal-define-key my/mod-leader-map-key my/leader-map)

;; * exwm

;; ** Keys before exwm init
(setq exwm-input-prefix-keys nil)
;; Exwm don't send back these keys
(dolist (k '(
	     XF86AudioLowerVolume
	     XF86AudioRaiseVolume
	     XF86PowerOff
	     XF86AudioMute
	     XF86AudioPlay
	     XF86AudioStop
	     XF86AudioPrev
	     XF86AudioNext
	     XF86ScreenSaver
	     XF86Back
	     XF86Forward

	     Scroll_Lock
	     print

	     ;; (read-event)
	     6 ;; C-f
	     12 ;; C-l
	     20 ;; C-t
	     11 ;; C-k
	     1 ;; C-a
	     10 ;; C-j
	     19 ;; C-s

	     14 ;; C-n
	     16 ;; C-p

	     ;; Doesn't seem to work, I'm getting errors when running the insert functions
	     ;; 134217835 ;; å
	     ;; 134217772 ;; ä
	     ;; 134217774 ;; ö
	     ;; 134217808 ;; Å
	     ;; 134217788 ;; Ä
	     ;; 134217790 ;; Ö

	     tab ;; Actual tab

	     134217848 ;; M-x

	     21 ;; C-u
	     23 ;; C-w

	     5 ;; C-e
	     7 ;; C-g

	     67108896 ;; C-space

	     8388653 ;; Decrease volume: Win--
	     8388669 ;; Increase volume: Win-=

	     ;; M-top letter row
	     134217841
	     134217831
	     134217837
	     134217836
	     134217847
	     134217849
	     134217830
	     134217845
	     134217826
	     134217787

	     ;; M-middle letter row
	     134217828
	     134217843
	     134217844
	     134217838
	     134217842
	     134217833
	     134217825
	     134217829
	     134217839
	     134217832

	     ;; C-d for exwm-edit compose
	     4
	     ))
  (cl-pushnew k exwm-input-prefix-keys))

;; Enable M-x menu in full-screen programs
(setq exwm-input-global-keys
      `(
	;; Bind "s-r" to exit char-mode and fullscreen mode.
	([?\M-x] . counsel-M-x)
	([tab] . my/window-hydra/body)
	))

;; ** Load package
(eval-and-compile
  (straight-use-package 'exwm)
  (require 'exwm-core))

;; ** keys
;; *** Define mode
(defvar my/exwm-mode-map (make-sparse-keymap))

(define-minor-mode my/exwm-mode nil nil nil my/exwm-mode-map)
(add-hook 'exwm-manage-finish-hook 'my/exwm-mode)

;; *** Define functions
(defun my/exwm-backspace () (interactive) (exwm-input--fake-key 'backspace))
(defun my/exwm-delete () (interactive) (exwm-input--fake-key 'delete))
(defun my/exwm-tab () (interactive) (exwm-input--fake-key 'tab))

(defun my/exwm-å () (interactive) (exwm-input--fake-key "å"))
(defun my/exwm-ä () (interactive) (exwm-input--fake-key "ä"))
(defun my/exwm-ö () (interactive) (exwm-input--fake-key "ö"))

(defun my/exwm-Å () (interactive) (exwm-input--fake-key "Å"))
(defun my/exwm-Ä () (interactive) (exwm-input--fake-key "Ä"))
(defun my/exwm-Ö () (interactive) (exwm-input--fake-key "Ö"))

(defun my/exwm-paste () (interactive) (exwm-input--fake-key ?\C-v))

(defun my/exwm-page-up () (interactive) (exwm-input--fake-key 'next))
(defun my/exwm-page-down () (interactive) (exwm-input--fake-key 'prior))

(defun my/exwm-return () (interactive) (exwm-input--fake-key 'return))
(defun my/exwm-escape () (interactive) (exwm-input--fake-key 'escape))
(defun my/exwm-find () (interactive) (exwm-input--fake-key ?\C-f))

(defun my/exwm-up () (interactive) (exwm-input--fake-key 'up))
(defun my/exwm-down () (interactive) (exwm-input--fake-key 'down))

(defun my/exwm-m-q () (interactive) (exwm-input--fake-key ?\!))
(defun my/exwm-m-g () (interactive) (exwm-input--fake-key ?\@))
(defun my/exwm-m-m () (interactive) (exwm-input--fake-key ?\#))
(defun my/exwm-m-l () (interactive) (exwm-input--fake-key ?\$))
(defun my/exwm-m-w () (interactive) (exwm-input--fake-key ?\%))
(defun my/exwm-m-y () (interactive) (exwm-input--fake-key ?\*))
(defun my/exwm-m-f () (interactive) (exwm-input--fake-key ?\())
(defun my/exwm-m-u () (interactive) (exwm-input--fake-key ?\)))
(defun my/exwm-m-b () (interactive) (exwm-input--fake-key ?\&))
(defun my/exwm-m-comma () (interactive) (exwm-input--fake-key ?^))

(defun my/exwm-1 () (interactive) (exwm-input--fake-key ?1))
(defun my/exwm-2 () (interactive) (exwm-input--fake-key ?2))
(defun my/exwm-3 () (interactive) (exwm-input--fake-key ?3))
(defun my/exwm-4 () (interactive) (exwm-input--fake-key ?4))
(defun my/exwm-5 () (interactive) (exwm-input--fake-key ?5))
(defun my/exwm-6 () (interactive) (exwm-input--fake-key ?6))
(defun my/exwm-7 () (interactive) (exwm-input--fake-key ?7))
(defun my/exwm-8 () (interactive) (exwm-input--fake-key ?8))
(defun my/exwm-9 () (interactive) (exwm-input--fake-key ?9))
(defun my/exwm-0 () (interactive) (exwm-input--fake-key ?0))

;; *** Keys
(setq exwm-mode-map (make-sparse-keymap))

(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "DEL") 'my/exwm-backspace)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "<deletechar>") 'my/exwm-delete)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "TAB") 'my/exwm-tab)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "M-x") 'counsel-M-x)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "C-k") 'my/exwm-paste)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "C-u") 'my/exwm-page-up)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "C-w") 'my/exwm-page-down)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "RET") 'my/exwm-return)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "C-g") 'my/exwm-escape)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "å") 'my/exwm-å)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "ä") 'my/exwm-ä)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "ö") 'my/exwm-ö)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "Å") 'my/exwm-Å)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "Ä") 'my/exwm-Ä)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "Ö") 'my/exwm-Ö)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd my/mod-leader-map-key) 'my/leader-map)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "C-=") 'my/window-hydra/body)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "M-x") 'counsel-M-x)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "C-d") #'exwm-edit--compose)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "C-j") 'my/toggle-switch-to-minibuffer)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "C-s") 'my/exwm-find)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "C-n") 'my/exwm-down)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "C-p") 'my/exwm-up)

(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "!") 'my/exwm-m-q)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "@") 'my/exwm-m-g)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "#") 'my/exwm-m-m)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "$") 'my/exwm-m-l)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "%") 'my/exwm-m-w)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "*") 'my/exwm-m-y)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "(") 'my/exwm-m-f)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd ")") 'my/exwm-m-u)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "&") 'my/exwm-m-b)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "^") 'my/exwm-m-comma)

(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "1") 'my/exwm-1)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "2") 'my/exwm-2)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "3") 'my/exwm-3)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "4") 'my/exwm-4)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "5") 'my/exwm-5)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "6") 'my/exwm-6)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "7") 'my/exwm-7)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "8") 'my/exwm-8)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "9") 'my/exwm-9)
(evil-define-key '(normal insert visual emacs replace) my/exwm-mode-map (kbd "0") 'my/exwm-0)

;; Needs to be kept same as above
(define-key my/exwm-mode-map (kbd "DEL") 'my/exwm-backspace)
(define-key my/exwm-mode-map (kbd "<deletechar>") 'my/exwm-delete)
(define-key my/exwm-mode-map (kbd "TAB") 'my/exwm-tab)
(define-key my/exwm-mode-map (kbd "M-x") 'counsel-M-x)
(define-key my/exwm-mode-map (kbd "C-k") 'my/exwm-paste)
(define-key my/exwm-mode-map (kbd "C-u") 'my/exwm-page-up)
(define-key my/exwm-mode-map (kbd "C-w") 'my/exwm-page-down)
(define-key my/exwm-mode-map (kbd "RET") 'my/exwm-return)
(define-key my/exwm-mode-map (kbd "C-g") 'my/exwm-escape)
(define-key my/exwm-mode-map (kbd "å") 'my/exwm-å)
(define-key my/exwm-mode-map (kbd "ä") 'my/exwm-ä)
(define-key my/exwm-mode-map (kbd "ö") 'my/exwm-ö)
(define-key my/exwm-mode-map (kbd "Å") 'my/exwm-Å)
(define-key my/exwm-mode-map (kbd "Ä") 'my/exwm-Ä)
(define-key my/exwm-mode-map (kbd "Ö") 'my/exwm-Ö)
(define-key my/exwm-mode-map (kbd my/mod-leader-map-key) 'my/leader-map)
(define-key my/exwm-mode-map (kbd "C-=") 'my/window-hydra/body)
(define-key my/exwm-mode-map (kbd "M-x") 'counsel-M-x)
(define-key my/exwm-mode-map (kbd "C-d") #'exwm-edit--compose)
(define-key my/exwm-mode-map (kbd "C-j") 'my/toggle-switch-to-minibuffer)
(define-key my/exwm-mode-map (kbd "C-s") 'my/exwm-find)
(define-key my/exwm-mode-map (kbd "C-n") 'my/exwm-down)
(define-key my/exwm-mode-map (kbd "C-p") 'my/exwm-up)

(define-key my/exwm-mode-map (kbd "!") 'my/exwm-m-q)
(define-key my/exwm-mode-map (kbd "@") 'my/exwm-m-g)
(define-key my/exwm-mode-map (kbd "#") 'my/exwm-m-m)
(define-key my/exwm-mode-map (kbd "$") 'my/exwm-m-l)
(define-key my/exwm-mode-map (kbd "%") 'my/exwm-m-w)
(define-key my/exwm-mode-map (kbd "*") 'my/exwm-m-y)
(define-key my/exwm-mode-map (kbd "(") 'my/exwm-m-f)
(define-key my/exwm-mode-map (kbd ")") 'my/exwm-m-u)
(define-key my/exwm-mode-map (kbd "&") 'my/exwm-m-b)
(define-key my/exwm-mode-map (kbd "^") 'my/exwm-m-comma)

(define-key my/exwm-mode-map (kbd "1") 'my/exwm-1)
(define-key my/exwm-mode-map (kbd "2") 'my/exwm-2)
(define-key my/exwm-mode-map (kbd "3") 'my/exwm-3)
(define-key my/exwm-mode-map (kbd "4") 'my/exwm-4)
(define-key my/exwm-mode-map (kbd "5") 'my/exwm-5)
(define-key my/exwm-mode-map (kbd "6") 'my/exwm-6)
(define-key my/exwm-mode-map (kbd "7") 'my/exwm-7)
(define-key my/exwm-mode-map (kbd "8") 'my/exwm-8)
(define-key my/exwm-mode-map (kbd "9") 'my/exwm-9)
(define-key my/exwm-mode-map (kbd "0") 'my/exwm-0)

;; ** Run exwm
(exwm-enable)
;; (add-hook 'after-init-hook 'exwm-enable)

;; ** Don't remove header in exwm-buffers
(add-hook 'exwm-manage-finish-hook (lambda () (kill-local-variable 'header-line-format)))

;; ** Exwm-edit
(setq exwm-edit-bind-default-keys nil)
;; (straight-use-package '(exwm-edit :type git :host github :repo "walseb/exwm-edit"))
;; (straight-use-package 'exwm-edit)
(straight-use-package '(exwm-edit :type git :host github :repo "agzam/exwm-edit" :branch "remove-global-mode"))

(with-eval-after-load 'exwm
  (require 'exwm-edit))

;; *** Remove header
(add-hook 'exwm-edit-mode-hook (lambda () (kill-local-variable 'header-line-format)))

;; ** Set exwm buffer name
;; *** Manually set buffer name
;; (defun my/exwm-set-window-name ()
;;   (interactive)
;;   (exwm-workspace-rename-buffer (completing-read "set title " nil)))

;; (define-key my/file-options-map (kbd "r") 'my/exwm-set-window-name)

;; *** Auto set buffer name
;; We use class names for all windows expect for Java applications and GIMP (because of problems with those).
(defun my/exwm-should-use-title-for-buffer-name ()
  (or
   (not exwm-instance-name)
   (string-prefix-p "sun-awt-X11-" exwm-instance-name)
   (string= "gimp" exwm-instance-name)
   ;; Firefox calls itself navigator?
   (string-prefix-p "Navigator" exwm-instance-name t)
   ;;(string-prefix-p "Firefox" exwm-instance-name t)
   ))

(add-hook 'exwm-update-class-hook 'my/exwm-buffer-give-name-class)

(defun my/exwm-buffer-give-name-class ()
  (when (not (my/exwm-should-use-title-for-buffer-name))
    (exwm-workspace-rename-buffer exwm-class-name)))

(add-hook 'exwm-update-title-hook 'my/exwm-buffer-give-name-title)

(defun my/exwm-buffer-give-name-title ()
  (when (my/exwm-should-use-title-for-buffer-name)
    (exwm-workspace-rename-buffer exwm-title)))

;; ** Fix modeline in exwm buffers
(add-hook 'exwm-floating-exit-hook (lambda ()
				     (kill-local-variable 'header-line-format)))

;; ** Disable floating windows
(setq exwm-manage-force-tiling nil)

;; ** Disable full screen
;; (cl-defun exwm-layout-set-fullscreen (&optional id)
;;   )

;; ** Multi-screen
(require 'exwm-randr)

;; *** Get monitor setup
(defun my/exwm-randr-auto-get-monitor ()
  (let* ((result)
	 (monitors (nth 1 (exwm-randr--get-monitors))))
    (dotimes (i (/ (length monitors) 2))
      (push (nth (* i 2) monitors) result)
      (push i result))
    result))

;; Get monitor setup
(if my/device/monitor-setup
    (progn
      (setq exwm-workspace-number (/ (length my/device/monitor-setup) 2))
      (setq exwm-randr-workspace-monitor-plist my/device/monitor-setup)))
;; (let ((monitor-setup (my/exwm-randr-auto-get-monitor)))
;; (setq exwm-workspace-number (/ (length monitor-setup) 2))
;; (setq exwm-randr-workspace-monitor-plist monitor-setup)))

;; *** Enable
(if (> exwm-workspace-number 1)
    (progn
      (exwm-randr-enable)))

;; *** Switch monitor (workspace) functions
;; `exwm-workspace-number' is equal to monitor count
;; (add-hook 'focus-out-hook 'exwm-layout--refresh)
(defun my/switch-monitor-left ()
  (interactive)
  (if (>= exwm-workspace-current-index (- exwm-workspace-number 1))
      (exwm-workspace-switch-create 0)
    (exwm-workspace-switch-create (+ exwm-workspace-current-index 1))))

(defun my/switch-monitor-right ()
  (interactive)
  (if (= exwm-workspace-current-index 0)
      (exwm-workspace-switch-create (- exwm-workspace-number 1))
    (exwm-workspace-switch-create (- exwm-workspace-current-index 1))))

;; (define-key my/keys-mode-map (kbd "M-l") 'my/switch-monitor-right)
;; (define-key my/keys-mode-map (kbd "M-h") 'my/switch-monitor-left)

;; ** Setting
;; Garbage collect when entering x window (because x is not in sync with emacs)
;; (add-hook 'exwm-mode-hook 'garbage-collect)

(setq exwm-workspace-show-all-buffers t)

(setq exwm-workspace-minibuffer-position 'top)

(add-hook 'exwm-init-hook (lambda () (interactive) (exwm-workspace-attach-minibuffer)))

;; * Hydra
(eval-and-compile
  (straight-use-package 'hydra))
(setq hydra-hint-display-type 'message)

;; ** Fix message
(with-eval-after-load 'hydra
  (setq hydra-hint-display-alist
	(list (list 'lv #'lv-message #'lv-delete-window)
	      (list 'message (lambda (str) (message "%s" str)) (lambda () (message "")))
	      (list 'posframe #'hydra-posframe-show #'hydra-posframe-hide))))

;; * Compatibility
;; ** Windows host clipboard crash
;; Emacs crashes from time to time when it's run in linux but the clipboard contents are from windows.
;; (setq x-select-request-type 'STRING)

;; (when my/windows-host
;;   (with-eval-after-load 'select
;;     (defun gui--selection-value-internal (type)
;;       (let ((request-type (if (eq window-system 'x)
;;			      (or x-select-request-type
;;				  '(UTF8_STRING COMPOUND_TEXT STRING))
;;			    'STRING))
;;	    text)
;;	(with-demoted-errors "gui-get-selection: %S"
;;	  (if (consp request-type)
;;	      (while (and request-type (not text))
;;		(setq text (gui-get-selection type (car request-type)))
;;		(setq request-type (cdr request-type)))
;;	    (setq text (gui-get-selection type request-type))))
;;	;; This seems to be the problem
;;	;; (if text
;;	;; (remove-text-properties 0 (length text) '(foreign-selection nil) text))
;;	text))))

;; * Alert
(defvar my/past-alerts (list))

(defun my/alert (&optional str severity flash)
  (let ((severity-face
	 (pcase severity
	   ('low 'my/alert-prio-low-face)
	   ('med 'my/alert-prio-med-face)
	   ('high 'my/alert-prio-high-face)
	   (_ 'my/alert-prio-none-face))))

    (when flash
      (if (eq flash 1)
	  (my/alert-blink-fringe-once severity-face)
	(my/alert-blink-fringe severity-face)))
    (if str
	(progn
	  (push (concat " " (propertize str 'face severity-face)) my/past-alerts)
	  (message str)))))

(defvar my/alert-blink-fringe-color)

(defun my/alert-blink-fringe-once (color)
  (setq my/alert-blink-fringe-color (face-attribute color :background))
  (my/alert-fringe-set-color)
  (run-with-timer 0.25 nil 'my/alert-fringe-restore))

(defun my/alert-blink-fringe (color)
  (setq my/alert-blink-fringe-color (face-attribute color :background))
  (my/alert-fringe-set-color)
  (run-with-timer 0.25 nil 'my/alert-fringe-restore)
  (run-with-timer 0.5 nil 'my/alert-fringe-set-color)
  (run-with-timer 0.75 nil 'my/alert-fringe-restore)
  (run-with-timer 1.0 nil 'my/alert-fringe-set-color)
  (run-with-timer 1.25 nil 'my/alert-fringe-restore))

(defun my/alert-fringe-set-color ()
  (set-face-attribute 'fringe nil :foreground my/alert-blink-fringe-color :background my/alert-blink-fringe-color))

(defun my/alert-fringe-restore ()
  (set-face-attribute 'fringe nil :foreground nil :background nil))

(defvar my/alert-updated-hook nil)

(defun my/alert-reset ()
  (interactive)
  (setq my/past-alerts (list))
  (run-hooks 'my/alert-updated-hook))

(defun my/alert-remove (&optional name)
  (interactive)
  (let ((new-name (when name (concat " " name))))
    (setq my/past-alerts (remove (or new-name (completing-read "Remove entry" my/past-alerts)) my/past-alerts))
    (run-hooks 'my/alert-updated-hook)))

;; ** Faces
(defface my/alert-prio-high-face
  '((t :inherit default))
  "")

(defface my/alert-prio-med-face
  '((t :inherit default))
  "")

(defface my/alert-prio-low-face
  '((t :inherit default))
  "")

(defface my/alert-prio-none-face
  '((t :inherit default))
  "")

;; ** Process completed alert
(defun my/alert-process-completed ()
  (let* ((buf-name (buffer-name))
	 (alert-str (concat "Completed: " buf-name)))
    (my/alert-statusline-message-temporary alert-str)))

(defun my/alert-statusline-message-temporary (message &optional severity duration)
  (my/alert message (or severity 'low))
  (run-with-timer 5 nil (lambda () (my/alert-remove message))))

;; * Package management
;; ** Guix
(straight-use-package 'guix)

;; *** Keys
(define-prefix-command 'my/guix-map)
(define-key my/leader-map (kbd "G") 'my/guix-map)

(define-key my/guix-map (kbd "v") 'guix)
(define-key my/guix-map (kbd "P") 'guix-profiles)
(define-key my/guix-map (kbd "g") 'guix-generation)
(define-key my/guix-map (kbd "G") 'guix-system-generations)
(define-key my/guix-map (kbd "i") 'guix-installed-user-packages)
(define-key my/guix-map (kbd "I") 'guix-installed-system-packages)

(define-prefix-command 'my/guix-services-map)
(define-key my/guix-map (kbd "s") 'my/guix-services-map)

(define-key my/guix-services-map (kbd "a") 'guix-all-services)
(define-key my/guix-services-map (kbd "n") 'guix-services-by-name)
(define-key my/guix-services-map (kbd "l") 'guix-services-by-location)
(define-key my/guix-services-map (kbd "d") 'guix-find-service-definition)

(define-prefix-command 'my/guix-package-map)
(define-key my/guix-map (kbd "p") 'my/guix-package-map)

(define-key my/guix-package-map (kbd "a") 'guix-all-packages)
(define-key my/guix-package-map (kbd "n") 'guix-packages-by-name)
(define-key my/guix-package-map (kbd "l") 'guix-packages-by-location)
(define-key my/guix-package-map (kbd "c") 'guix-packages-from-system-config-file)
(define-key my/guix-package-map (kbd "d") 'guix-find-package-definition)

(define-prefix-command 'my/guix-store-map)
(define-key my/guix-map (kbd "S") 'my/guix-store-map)

(define-key my/guix-store-map (kbd "l") 'guix-store-live-items)
(define-key my/guix-store-map (kbd "d") 'guix-store-dead-items)
(define-key my/guix-store-map (kbd "D") 'guix-store-item-derivers)
(define-key my/guix-store-map (kbd "f") 'guix-store-failures)
(define-key my/guix-store-map (kbd "r") 'guix-store-item-references)
(define-key my/guix-store-map (kbd "R") 'guix-store-item-referrers)
(define-key my/guix-store-map (kbd "C-r") 'guix-store-item-requisites)

;; ** Local packages
;; (add-to-list 'load-path (expand-file-name (concat user-emacs-directory "local-packages")))

;; * Minor
;; ** Startup
;; Disable startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; ** Scratch buffer
;; *** Disable initial scratch buffer message
(setq initial-scratch-message nil)

;; *** Set default mode
(setq initial-major-mode 'org-mode)

;; ** Encoding
;; (setq locale-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)

;; ** Line wrapping
;; *** Enable truncate lines mode
(setq-default truncate-lines nil)
(setq truncate-lines nil)

;; Always truncate lines
(setq truncate-partial-width-windows nil)
(setq-default truncate-partial-width-windows nil)

;; **** Toggle truncate lines
(define-key my/leader-map (kbd "C-v")
  (lambda () (interactive)
    (setq truncate-lines (not truncate-lines))))

;; ** Visual line mode
(global-visual-line-mode 1)

;; *** Fringe indicators of wrapped line
(setq visual-line-fringe-indicators '(left-bracket nil))
;; (setq visual-line-fringe-indicators '(top-left-angle nil))
;; (setq visual-line-fringe-indicators '(empty-line nil))

;; ** Disable tooltips
(tooltip-mode -1)
;; (setq show-help-function nil)
(setq show-help-function 'message)

;; ** 1 letter prompts
;; Convert yes or no prompt to y or n prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; ** Smooth scroll
;; Scroll 1 line at a time when cursor goes outside screen
(setq scroll-conservatively 200)

;; ** Bell
;; Disable bell
(setq ring-bell-function 'ignore)

;; ** Subword (camel case movement)
(global-subword-mode 1)

;; ** Change max killring size
(setq kill-ring-max 500)

;; ** Don't save duplicates to kill ring
(setq kill-do-not-save-duplicates t)

;; ** Pixel scroll mode
;; In org mode when displaying images pixel scroll mode can be useful maybe
;; (add-hook 'org-mode-hook 'pixel-scroll-mode)

;; ** Increase and decrease brightness
(defun my/increase-brightness ()
  (interactive)
  (my/message-at-point (my/sudo-shell-command-to-string "brightnessctl s +1%")))

(defun my/decrease-brightness ()
  (interactive)
  (my/message-at-point (my/sudo-shell-command-to-string "brightnessctl s 1%-")))

(when my/enable-brightness-binds
  (global-set-key (kbd "<XF86MonBrightnessUp>") 'my/increase-brightness)
  (global-set-key (kbd "<XF86MonBrightnessDown>") 'my/decrease-brightness))

;; ** Sudo edit
(straight-use-package 'sudo-edit)

(define-key my/leader-map (kbd "C-S-s") 'sudo-edit)

;; *** Dired fix
(defun my/sudo-edit-is-on ()
  (string-equal
   (file-remote-p (or buffer-file-name default-directory) 'user)
   "root"))

(defun my/dired-sudo-edit-setup ()
  ;; If file is edited with sudo (in this case only works on dired due to hook)
  (if (my/sudo-edit-is-on)
      (dired-sort-other "-alh")))

(add-hook 'dired-mode-hook 'my/dired-sudo-edit-setup)

;; ** Enable disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; ** Async
(straight-use-package 'async)

(dired-async-mode 1)

;; ** Async-shell-command
(setq async-shell-command-buffer 'new-buffer)

;; ** Zoom
(defvar my/current-default-face-height my/default-face-height)
(defvar my/resize-timer-not-running t)

(defun my/set-default-face-modeline-resize ()
  (if my/resize-timer-not-running
      (progn
	(setq my/resize-timer-not-running nil)
	(run-with-idle-timer 1 nil (lambda ()
				     (my/exwm-resize-minibuffer)
				     (setq my/resize-timer-not-running t))))))

(defun my/exwm-resize-minibuffer ()
  (exwm-workspace-detach-minibuffer)
  (exwm-workspace-attach-minibuffer))

(defun my/set-default-face-height (height)
  (setq my/current-default-face-height height)
  (set-face-attribute 'default nil :height height)
  (my/set-default-face-modeline-resize)

  ;; Update hydra text
  (setq my/text-size-hydra/hint (concat "Text height: " (number-to-string height))))

(defun my/increment-default-face-height ()
  (interactive)
  (my/set-default-face-height (+ my/current-default-face-height 5)))

(defun my/decrement-default-face-height ()
  (interactive)
  (my/set-default-face-height (- my/current-default-face-height 5)))

(defun my/reset-default-face-height ()
  (interactive)
  (my/set-default-face-height my/default-face-height))

(define-key my/leader-map (kbd "-") 'my/text-size-hydra/body)
(define-key my/leader-map (kbd "=") 'my/text-size-hydra/body)

(define-key my/leader-map (kbd "+") 'my/reset-default-face-height)
(define-key my/leader-map (kbd "_") 'my/reset-default-face-height)

;; *** Hydra
(with-eval-after-load 'hydra
  (defhydra my/text-size-hydra (:hint nil
				      :color red)
    ("p" my/increment-default-face-height nil)
    ("n" my/decrement-default-face-height nil)
    ("N" my/reset-default-face-height nil)
    ("P" my/reset-default-face-height nil)))

;; ** Exit emacs
(define-key my/leader-map (kbd "C-z") 'save-buffers-kill-emacs)

;; ** Help mode
(setq help-mode-map (make-sparse-keymap))

;; ** Ibuffer
(setq ibuffer-mode-map (make-sparse-keymap))

;; ** Compilation mode
(setq compilation-mode-map (make-sparse-keymap))
(setq compilation-minor-mode-map (make-sparse-keymap))
(setq compilation-shell-minor-mode-map (make-sparse-keymap))
(setq compilation-mode-tool-bar-map (make-sparse-keymap))

(define-key compilation-mode-map (kbd "j") 'my/compilation-change-command)
(define-key compilation-mode-map (kbd "g") 'recompile)
(define-key compilation-mode-map (kbd "C-c") 'kill-compilation)
;; (evil-define-key '(normal insert visual replace) compilation-mode-map (kbd "C-c") 'kill-compilation)

;; *** Show alert after compilation completed
(add-to-list 'compilation-finish-functions (lambda (_a _b)
					     (my/alert-process-completed)))

;; ** Prefer loading newest lisp source file
(setq load-prefer-newer t)

;; ** Revert buffer bind
(define-key my/leader-map (kbd "r") 'revert-buffer)

;; ** Hotkey to hide cursor
(define-key my/leader-map (kbd "M-h") (lambda () (interactive) (setq cursor-type nil)))

;; ** Tetris
(with-eval-after-load 'tetris
  (evil-define-key 'insert tetris-mode-map (kbd "p") #'tetris-rotate-next)
  (evil-define-key 'insert tetris-mode-map (kbd "P") #'tetris-rotate-prev)
  (evil-define-key 'insert tetris-mode-map (kbd "n") #'tetris-move-down)
  (evil-define-key 'insert tetris-mode-map (kbd "N") #'tetris-move-bottom)
  (evil-define-key 'insert tetris-mode-map (kbd "h") #'tetris-move-left)
  (evil-define-key 'insert tetris-mode-map (kbd "l") #'tetris-move-right)

  (evil-define-key 'insert tetris-mode-map (kbd "SPC") #'tetris-move-bottom))

;; ** Redefine keyboard-escape-quit
(defun keyboard-escape-quit ()
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
	((region-active-p)
	 (deactivate-mark))
	((> (minibuffer-depth) 0)
	 (abort-recursive-edit))
	(current-prefix-arg
	 nil)
	((> (recursion-depth) 0)
	 (exit-recursive-edit))
	(buffer-quit-function
	 (funcall buffer-quit-function))))

;; ** lisp-ls
;; ls for systems without ls installed (like windows). Gets used automatically on those systems
(setq-default ls-lisp-format-time-list  '("%d-%m-%Y %H:%M" "%d-%m-%Y %H:%M")
	      ls-lisp-use-localized-time-format t)

;; ** Fix backward-sexp
(defun my/backward-sexp (&optional arg)
  "Fixed backward sexp so you don't have to place cursor 1 char in front of paren you want to go backward on"
  (interactive "^p")
  (or arg (setq arg 1))
  (my/forward-sexp (- arg) 1))

(defun my/forward-sexp (&optional arg extra-move)
  (interactive "^p")
  (or arg (setq arg 1))
  (if forward-sexp-function
      (funcall forward-sexp-function arg)
    (goto-char (or (scan-sexps (+ (point) extra-move) arg) (buffer-end arg)))
    (if (< arg 0) (backward-prefix-chars))))

;; ** Switch to last buffer
(defun my/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (car (cdr (ivy--buffer-list "")))))

;; ** Echo keypresses instantly
;; (setq echo-keystrokes 0.001)
(setq echo-keystrokes 0)

;; ** Configure mouse
(define-key minibuffer-inactive-mode-map [mouse-1] #'ignore)

;; *** Disable middleclick and right click
(define-key global-map [mouse-2] #'ignore)
(my/evil-universal-define-key "<mouse-2>" #'ignore)

(define-key global-map [mouse-3] #'ignore)

;; *** Disable mouse wheel acceleration
(setq mouse-wheel-progressive-speed nil)

;; ** Mouse-avoidance
;; (mouse-avoidance-mode 'none)

;; (setq mouse-avoidance-banish-position '((frame-or-window . frame)
;;					(side . right)
;;					(side-pos . 0)
;;					(top-or-bottom . bottom)
;;					(top-or-bottom-pos . 0)))

;; ** Minibuffer-depth
;; Enable and show minibuffer recursive depth
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; ** Clone indirect buffer name
;; *** Clone indirect buffer this window
(defun my/clone-indirect-buffer ()
  (interactive)
  (when (not (string= major-mode 'exwm-mode))
    (clone-indirect-buffer
     (concat
      "I: "
      (buffer-name)
      )
     t)))

;; *** Clone indirect buffer other window
(defun my/clone-indirect-buffer-other-window ()
  (interactive)
  (when (not (string= major-mode 'exwm-mode))
    (clone-indirect-buffer-other-window
     (concat
      "I: "
      (buffer-name)
      )
     t
     )))

;; ** Build config
(defun my/build-config-docs ()
  (interactive)
  (my/config-visit)
  (my/outorg-export-to-org-file "~/.emacs.d/readme.org"))

;; ** Man mode
;; *** Disable keys
(setq Man-mode-map (make-sparse-keymap))

;; ** Timer
;; Set timer to only run expired repeating hooks once after sleep
(setq timer-max-repeats 1)

;; ** Copy minibuffer contents
(define-key my/leader-map (kbd "Y") (lambda ()
				      (interactive)
				      ;; (kill-buffer " *Minibuf-0*")
				      (let ((curr-buf (buffer-name)))
					(switch-to-buffer " *Minibuf-0*")
					(copy-region-as-kill (point-min) (point-max))
					(switch-to-buffer curr-buf))))

;; ** Set safe local variables
(add-to-list 'safe-local-variable-values '(eval setq-local dante-project-root
						(projectile-project-root)))

(add-to-list 'safe-local-variable-values '(dante-repl-command-line "ob" "repl"))

;; ** Calendar
(setq calendar-mode-map (make-sparse-keymap))

(with-eval-after-load 'calendar
  (define-key calendar-mode-map (kbd "l") 'calendar-forward-month)
  (define-key calendar-mode-map (kbd "h") 'calendar-backward-month))

;; ** Explain pause mode
(straight-use-package '(explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode"))
;; (explain-pause-mode)

;; ** Compilation-mode
(setq compilation-ask-about-save nil)

;; ** Store point/position in buffers
(save-place-mode 1)

(setq save-place-file (concat user-emacs-directory ".cache/save-place"))

;; ** Disable XF86Back and XF86Forward
(global-unset-key (kbd "<XF86Back>"))
(global-unset-key (kbd "<XF86Forward>"))

;; ** Unbind C-z
;; By default this runs suspend-frame
(global-unset-key (kbd "C-z"))

;; * Productivity
;; ** Break timer
;; In seconds
(defvar my/break-time (* 21 60))
(defvar my/not-clocked-in-break-time (* 1 60))

(defun my/break-screen ()
  ;; Restart timer
  (let* ((is-clocked-in (org-clocking-p))
	 (time (if is-clocked-in
		   my/break-time
		 my/not-clocked-in-break-time)))

    (my/break-timer-run time)

    (when my/interruptions
      (my/message-at-point "Clock in! Also check agenda!")

      (when (not is-clocked-in)
	(my/alert-statusline-message-temporary "Clock in!" 'med))
      ;; Clock
      ;; (run-with-timer 0.2 nil (lambda () (sleep-for 5)))
      ;; (org-mru-clock-in)

      (my/fire-notification (my/get-random-txt) (my/get-random-wallpaper)))))

(defun my/break-timer-run (time)
  (interactive)
  (run-with-timer time nil #'my/break-screen))

(my/break-timer-run my/not-clocked-in-break-time)

;; *** Random
(defun my/get-random-txt ()
  (seq-random-elt
   (org-map-entries (lambda ()
		      (when (> (org-current-level) 1)
			(let ((old-kill-ring kill-ring))
			  (org-copy-subtree)
			  (message "")
			  (let ((return (pop kill-ring)))
			    (setq kill-ring old-kill-ring)
			    return))))
		    "Tech"
		    (list (concat my/notes-folder "Organize/Agenda.org")))))

;; * File options
;; (define-prefix-command 'my/file-options-map)
;; (define-key my/leader-map (kbd "`") 'my/file-options-map)

;; ;; ** Revert
;; (define-key my/file-options-map (kbd "r") 'revert-buffer)

;; ;; ** Statistics
;; (define-prefix-command 'my/statistics-map)
;; (define-key my/file-options-map (kbd "s") 'my/statistics-map)

;; (define-key my/statistics-map (kbd "w") 'count-words)
;; (define-key my/statistics-map (kbd "r") 'count-words-region)

;; ;; ** Indentation
;; (define-prefix-command 'my/indentation-map)
;; (define-key my/file-options-map (kbd "i") 'my/indentation-map)

;; (defun my/change-tab-width ()
;;   (interactive)
;;   (setq-default tab-width (string-to-number (completing-read "Enter tab width" nil))))

;; ;; Applies only to region
;; (define-key my/indentation-map (kbd "t") 'tabify)
;; (define-key my/indentation-map (kbd "SPC") 'untabify)

;; (define-key my/indentation-map (kbd "w") 'my/change-tab-width)

;; * Open
(define-prefix-command 'my/open-map)
(define-key my/leader-map (kbd "o") 'my/open-map)

;; ** Scratch
;; Kill the initial scratch buffer
(ignore-errors
  (kill-buffer "*scratch*"))

(defun my/switch-to-scratch()
  (interactive)
  (find-file (concat user-emacs-directory "scratch.org")))

(define-key my/open-map (kbd "s") 'my/switch-to-scratch)

;; ** Org-brain notes
(define-key my/open-map (kbd "n") (lambda () (interactive)
				    (require 'org-brain)
				    (or
				     (and (get-buffer "*org-brain*")
					  (switch-to-buffer "*org-brain*"))
				     (when (and buffer-file-name
						(file-in-directory-p
						 buffer-file-name
						 org-brain-path))
				       (org-brain-visualize (org-brain-path-entry-name buffer-file-name) nil nil nil))
				     (org-brain-visualize "Origo" nil nil nil))))

(define-key my/open-map (kbd "N") (lambda () (interactive)
				    (require 'org-brain)
				    (org-brain-visualize
				     (completing-read "Entry: "
						      (org-brain--all-targets)))))

(define-key my/open-map (kbd "C-n") (lambda () (interactive) (find-file org-brain-path)))

;; ** Visit nixos config
(defun my/nixos-config-visit ()
  (interactive)
  (find-file "/etc/nixos/configuration.nix"))

(define-key my/open-map (kbd "C") 'my/nixos-config-visit)

;; ** Visit nix home config
(defun my/nix-home-config-visit ()
  (interactive)
  (find-file "~/.config/nixpkgs/home.nix"))

(define-key my/open-map (kbd "C-c") 'my/nix-home-config-visit)

;; ** Visit config
(defun my/config-visit ()
  (interactive)
  (find-file (expand-file-name (concat user-emacs-directory "config.el")))
  ;; Emacs lags if flycheck runs on config
  (flycheck-mode -1))

(define-key my/open-map (kbd "c") 'my/config-visit)

;; ** Open trash
(defun my/trash-visit ()
  (interactive)
  (find-file "~/.local/share/Trash/files/"))

(define-key my/open-map (kbd "t") 'my/trash-visit)

;; ** Open agenda
(defun my/org-agenda-show-agenda-and-todo (&optional arg)
  (interactive "P")
  (let ((buf (get-buffer org-agenda-buffer-name)))
    (if buf
	(switch-to-buffer buf)
      (org-agenda arg "n"))))

(define-key my/open-map (kbd "a") 'my/org-agenda-show-agenda-and-todo)

;; ** Open messages
(defun my/open-messages ()
  (interactive)
  (switch-to-buffer "*Messages*"))

(define-key my/open-map (kbd "m") 'my/open-messages)

;; ** Open downloads
(defun my/open-downloads ()
  (interactive)
  (find-file "~/Downloads"))

(define-key my/open-map (kbd "d") 'my/open-downloads)

;; ** Open home
(defun my/open-home ()
  (interactive)
  (find-file "~/"))

(define-key my/open-map (kbd "r") 'my/open-home)

;; ** Open password file
(defun my/open-passwords ()
  (interactive)
  (find-file espy-password-file))

(define-key my/open-map (kbd "p") 'my/open-passwords)

;; ** Visit agenda file
(defun my/agenda-file-visit ()
  (interactive)
  (find-file (car org-agenda-files)))

(define-key my/open-map (kbd "A") 'my/agenda-file-visit)

;; ** Open backup
(defun my/backups-visit ()
  (interactive)
  (find-file (concat user-emacs-directory ".git-backup")))

(define-key my/open-map (kbd "b") 'my/backups-visit)

;; ** Open wallpaper
(defun my/get-random-wallpaper ()
  (interactive)
  (when my/interruptions
    (my/get-random-element (directory-files-recursively (concat my/wallpaper-folder "Great/") "."))))

(defun my/show-random-wallpaper ()
  (interactive)
  (when my/interruptions
    (find-file
     (my/get-random-wallpaper))))

(define-key my/open-map (kbd "w") 'my/show-random-wallpaper)
(define-key my/open-map (kbd "W") (lambda () (interactive) (find-file my/wallpaper-folder)))

;; ** Open firefox
(defvar my/gui-browser
  (if (my/is-system-package-installed 'icecat)
      "icecat"
    (if (my/is-system-package-installed 'firefox-nightly)
	"firefox-nightly"
      (if (my/is-system-package-installed 'iceweasel)
	  "iceweasel"
	"firefox"))))

(defun my/open-in-browser (&optional url)
  (unless url
    (setq url ""))
  (start-process my/gui-browser nil my/gui-browser "--new-window" url))

(defun my/launch-gui-browser ()
  (interactive)
  (my/open-in-browser))

;; (defvar my/browser-bookmarks '(
;;			       "youtube.com"
;;			       "discordapp.com/channels/@me"
;;			       "github.com"
;;			       "steamcommunity.com/chat"
;;			       ))

;; (defun my/launch-firefox ()
;;   (interactive)
;;   (let* (
;;          (search (completing-read "url " my/browser-bookmarks))
;;          (adress
;;           (if (cl-member search my/browser-bookmarks :test #'string=)
;;               search
;;             (concat "https://www.google.com/search?q=" search))))
;;     (start-process (concat my/gui-browser my/temp-firefox-title-name) nil my/gui-browser "--new-window" adress)))

(define-key my/leader-map (kbd "C-b") 'my/launch-gui-browser)

;; ** Suggest
;; (define-key my/open-map (kbd "s") 'suggest)

;; * Polymode
;; Used by org-brain
;; (straight-use-package 'polymode)

;; * Org
(eval-and-compile
  (straight-use-package 'org)
  (require 'org))

;; Set org src indent to be 0
(setq org-edit-src-content-indentation 0)

(define-prefix-command 'my/org-mode-map)
(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map (kbd (concat my/leader-map-key " a")) #'my/org-mode-map))

;; ** Inline images
;; Set max image width to be third of display
(setq org-image-actual-width (/ (display-pixel-width) 3))

;; ** Babel
;; *** Supported runnable languages
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (gnuplot . t))))

;; *** Disable warnings in org mode before evaluating source block
(setq org-confirm-babel-evaluate nil)

;; ** Export
;; *** Twitter bootstrap
(straight-use-package 'ox-twbs)

;; ** org-superstar
(straight-use-package 'org-superstar)

(add-hook 'org-mode-hook 'org-superstar-mode)

;; ** Visuals
;; *** Highlight whole heading line
(setq org-fontify-whole-heading-line t)

;; *** Hide emphasis markers
;; The equal signs =here= to make it bold should not be visible
(setq org-hide-emphasis-markers nil)

;; *** Disable edit-src help header
(setq org-edit-src-persistent-message nil)

;; *** Disable code block indent
;; Should I change this??
;; (setq org-edit-src-content-indentation 0)

;; *** Ellipsis face
(setq org-ellipsis my/fold-ellipsis)

;; *** Always truncate lines
(setq org-startup-truncated nil)

;; ** Indent mode
(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'org-indent-mode))

;; ** Org SRC
;; *** Make c-' open in current window
(setq org-src-window-setup 'current-window)

;; *** Don't save window layout
(with-eval-after-load 'org
  (add-hook 'org-src-mode-hook (lambda () (interactive) (setq org-src--saved-temp-window-config nil))))

;; *** Rebind key
(define-key my/leader-map (kbd "'") 'my/toggle-org-src)

(defun my/toggle-org-src ()
  (interactive)
  (if (string= major-mode 'org-mode)
      (org-edit-special)
    (org-edit-src-exit)))

;; ** Capture
(setq org-default-notes-file (concat my/notes-folder "Organize/Agenda.org"))

;; *** Doct
(straight-use-package 'doct)

(setq org-capture-templates
      (doct '(
	      ;; Default todo
	      ;; '(("t" "Task" entry (file+headline "" "Tasks")
	      ;;    "* TODO %?\n  %u\n  %a"))
	      ("Task"
	       :keys "t"
	       :file org-default-notes-file
	       :headline "Tasks"
	       :prepend t
	       :template ("* TODO %?\n  %u\n  %a"))

	      ("Learning"
	       :keys "l"
	       :file org-default-notes-file
	       :headline "Learning resources"
	       :prepend t
	       :template ("* %?\n  %u\n  %a"))
	      )))

;; *** Auto capture
(defun my/auto-org-capture ()
  (interactive)
  (pcase major-mode
    ;; ('mu4e-headers-mode (mu4e-org-store-and-capture))
    ;; ('mu4e-view-mode (mu4e-org-store-and-capture))
    (_ (call-interactively 'org-capture))))

(define-key my/leader-map (kbd "C") 'my/auto-org-capture)

;; ** Agenda
;; Give agenda file to use
(setq org-agenda-files `(,org-default-notes-file))
;; (directory-files-recursively "~/Notes/Agenda/" ".org$")

(setq org-agenda-window-setup 'current-window)

(setq org-agenda-start-day "-1d")
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-span 50)

(setq org-agenda-todo-ignore-scheduled 'all)

;; Put todos on top
(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs" ((alltodo "")
				     (agenda "")

				     ;; Add org-agenda time budgets integration
				     ;; (agenda "" ((org-agenda-sorting-strategy '(habit-down time-up priority-down category-keep user-defined-up))))
				     ;; (org-time-budgets-in-agenda)
				     ))))

;; *** log-mode
;; Shows clocked time in timeline view
(setq org-agenda-start-with-log-mode 'clock)

;; **** Colorize and resize blocks
;; https://orgmode.org/worg/org-hacks.html
(defun my/org-agenda-log-mode-colorize-block ()
  "Set different line spacing based on clock time duration."
  (save-excursion
    (let* ((colors (cl-case (alist-get 'background-mode (frame-parameters))
		     ('light
		      (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7"))
		     ('dark
		      (list "#aa557f" "DarkGreen" "DarkSlateGray" "DarkSlateBlue"))))
	   pos
	   duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
	(goto-char pos)
	(when (and (not (equal pos (point-at-eol)))
		   (setq duration (org-get-at-bol 'duration)))
	  ;; larger duration bar height
	  (let ((line-height (if (< duration 15) 1.0 (+ 0.5 (/ duration 30))))
		(ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
	    (overlay-put ov 'face `(:background ,(car colors) :foreground "black"))
	    (setq colors (cdr colors))
	    (overlay-put ov 'line-height line-height)
	    (overlay-put ov 'line-spacing (1- line-height))))))))

(add-hook 'org-agenda-finalize-hook 'my/org-agenda-log-mode-colorize-block)

;; *** Super agenda
;; (straight-use-package 'org-super-agenda)
;; (with-eval-after-load 'org-agenda
;;   (org-super-agenda-mode 1))

;; *** Idle-agenda
(setq my/idle-agenda-delay (* 60 5))

;; (run-with-idle-timer my/idle-agenda-delay t 'my/startup-view)

;; *** org-timeline
;; (straight-use-package 'org-timeline)

;; (with-eval-after-load 'org-agenda
;;   (require 'org-timeline))

;; (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append)

;; *** Keys
(setq org-agenda-mode-map (make-sparse-keymap))
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "n") 'org-agenda-next-line)

  (define-key org-agenda-mode-map (kbd "p") 'org-agenda-previous-line)

  (define-key org-agenda-mode-map (kbd "l") 'org-agenda-later)
  (define-key org-agenda-mode-map (kbd "h") 'org-agenda-earlier)

  (define-key org-agenda-mode-map (kbd "g") 'org-agenda-redo-all)

  (define-key org-agenda-mode-map (kbd "t") 'org-agenda-todo)

  (define-key org-agenda-mode-map (kbd "l") 'org-agenda-todo)


  (define-key org-agenda-mode-map [remap newline] 'org-agenda-goto))

;; ** Clock
(eval-and-compile
  ;; This is required by the status line
  (require 'org-clock))

(setq org-clock-into-drawer "CLOCK-LOGBOOK")
(setq org-clock-mode-line-total 'today)
(setq org-extend-today-until 6)
(setq my/org-clocks-file org-default-notes-file)

(add-hook 'after-init-hook (lambda ()
			     (require 'org)
			     (org-clock-auto-clockout-insinuate)))

;; *** org-mru-clock
(straight-use-package 'org-mru-clock)

(setq org-mru-clock-keep-formatting t)

(setq org-mru-clock-files (lambda () `(,my/org-clocks-file)))
(setq org-mru-clock-how-many 999)

(define-key my/leader-map (kbd "k") 'org-mru-clock-in)
(define-key my/leader-map (kbd "C-k") 'org-clock-out)
(define-key my/leader-map (kbd "M-k") (lambda () (interactive) (find-file my/org-clocks-file)))

;; **** Custom prompt
(with-eval-after-load 'org-mru-clock
  (defun org-mru-clock-format-entry ()
    "Return the parent heading string appended to the heading at point."
    (let* ((heading (org-get-heading 'no-tags 'no-todo))
	   (time (org-clock-sum-current-item
		  (org-clock-get-sum-start)))
	   (limit (org-entry-get nil "MinTime"))
	   (is-done (if (< time (string-to-number limit))
			"todo"
		      ""))
	   (parent
	    (save-excursion
	      (org-up-heading-safe)
	      (org-get-heading 'no-tags 'no-todo)))
	   (parent-post (if parent
			    (format "%\ 4s %\ 3s / %\ 3s min | " is-done time limit)
			  ""))
	   (with-parent (concat parent-post heading)))
      (if org-mru-clock-keep-formatting
	  with-parent
	(substring-no-properties with-parent)))))

;; *** org-time-budgets
;; (straight-use-package 'org-time-budgets)

;; *** Quicly clock offline time
;; https://emacs.stackexchange.com/questions/34905/how-to-clock-offline-hours-quickly
(defun my/org-insert-clock-range (&optional n)
  (interactive "NTime Offset (in min): ")
  (let* ((ctime (cdr (decode-time (current-time))))
	 (min (car ctime))
	 (start (apply 'encode-time 0 (- min n) (cdr ctime))))
    (org-insert-time-stamp start t t "CLOCK: ")
    (insert "--")
    (org-insert-time-stamp (current-time) t t))
  (org-clock-update-time-maybe))

;; *** Keys
(define-key my/org-mode-map (kbd "c") 'my/org-insert-clock-range)

;; (define-prefix-command 'my/clock-map)
;; (define-key my/leader-map (kbd "c") 'my/clock-map)

;; (define-key my/clock-map (kbd "s") 'org-clock-in)
;; (define-key my/clock-map (kbd "S") 'org-clock-out)
;; (define-key my/clock-map (kbd "C-s") 'org-clock-in-last)

;; (define-key my/clock-map (kbd "e") 'org-clock-modify-effort-estimate)

;; ** Ivy-todo
;; (straight-use-package 'ivy-todo)
;; (setq ivy-todo-file my/org-clocks-file)

;; ** Custom links
;; ***  Find wallpaper
;; Functions used in the find wallpaper snippet
(defun my/open-wall (file-name)
  (let* ((default-directory my/wallpaper-folder)
	 (files (my/rec-find-files)))
    (unless (ignore-errors (progn (find-file (seq-find (lambda (a) (string-match-p (rx-to-string file-name) a)) files)) t))
      (error "Error: file not found"))))

(defun my/find-wall-name (initial-input)
  (my/counsel-file-jump-get-name initial-input my/wallpaper-folder))

;; ** Habit
;; https://cpbotha.net/2019/11/02/forming-and-maintaining-habits-using-orgmode/
;; (add-to-list 'org-modules 'org-habit t)
(with-eval-after-load 'org
  (require 'org-habit))

(setq org-habit-show-all-today t)

(setq org-habit-graph-column 100)

;; log into LOGBOOK drawer
(setq org-log-into-drawer t)

;; ** Present
(defun my/org-present-next ()
  (interactive)
  (widen)
  (if (string= (string (char-after)) "*")
      (forward-line))
  (narrow-to-region
   (- (re-search-forward "^*") 1)
   (- (re-search-forward "^*") 1))
  (evil-open-fold)
  (goto-char (point-min)))

(defun my/org-present-prev ()
  (interactive)
  (widen)
  (if (string= (string (char-after)) "*")
      (forward-line))
  (narrow-to-region
   (re-search-backward "^*")
   (+ (re-search-backward "^*") 1))
  (evil-open-fold)
  (goto-char (point-min)))

;; ** Eldoc
(straight-use-package 'org-plus-contrib)

(with-eval-after-load 'org
  (require 'org-refile))

(add-hook 'org-mode-hook #'org-eldoc-load)

;; *** Fix error
;; The function =org-src-get-lang-mode= doesn't exist, but the function =org-src--get-lang-mode= does
;;  (defun org-src-get-lang-mode (LANG)
;;    (org-src--get-lang-mode LANG))

;; ** Brain
(straight-use-package 'org-brain)
(setq org-brain-path my/notes-folder)
(setq org-brain-show-history nil)
(setq org-brain-show-resources nil)
(setq org-brain-open-same-window t)

(add-hook 'org-brain-visualize-text-hook 'org-toggle-inline-images)

(setq org-brain-completion-system 'ivy)

;; Speedups
(setq org-brain-file-entries-use-title nil)
(setq org-brain-scan-for-header-entries nil)

;; org-insert-link doesn't pick up org-brain unless you require org-brain
(with-eval-after-load 'org
  (require 'org-brain))

;; (with-eval-after-load 'org-brain
;;   (add-hook 'org-brain-visualize-mode-hook (lambda () (interactive) (org-brain-visualize-follow 1))))

;; *** My add child
(defun my/org-brain-add-child (entry children)
  (interactive (list (org-brain-entry-at-pt)
		     (org-brain-choose-entries "Add child: " 'all nil nil
					       (let ((path (file-relative-name default-directory org-brain-path)))
						 (if (string= "./" path)
						     nil
						   path)))))
  (dolist (child-entry children)
    (org-brain-add-relationship entry child-entry))
  (org-brain--revert-if-visualizing))

;; *** Polymode support
;; (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode)

;; *** Keys
(setq org-brain-visualize-mode-map (make-sparse-keymap))

(with-eval-after-load 'org-brain
  ;; Movement
  (define-key org-brain-visualize-mode-map "n" 'forward-button)
  (define-key org-brain-visualize-mode-map "p" 'backward-button)
  (define-key org-brain-visualize-mode-map [remap backward-delete-char-untabify] 'org-brain-visualize-back)
  (evil-define-key 'normal org-brain-visualize-mode-map (kbd "u") 'org-brain-visualize-back)

  ;; Child
  (define-key org-brain-visualize-mode-map "c" 'my/org-brain-add-child)
  (define-key org-brain-visualize-mode-map "C" 'org-brain-remove-child)

  ;; Parent
  (define-key org-brain-visualize-mode-map "v" 'org-brain-add-parent)
  (define-key org-brain-visualize-mode-map "V" 'org-brain-remove-parent)

  ;; Delete
  (define-key org-brain-visualize-mode-map "d" 'org-brain-delete-entry)
  (define-key org-brain-visualize-mode-map "D" 'org-brain-delete-selected-entries)

  ;; File
  (define-key org-brain-visualize-mode-map "o" 'org-brain-goto-current)
  (define-key org-brain-visualize-mode-map "O" 'org-brain-goto)

  (define-key org-brain-visualize-mode-map "j" 'org-brain-set-title)
  (define-key org-brain-visualize-mode-map "J" 'org-brain-rename-file)

  (define-key org-brain-visualize-mode-map "t" 'org-brain-open-resource)

  ;; Select
  (define-key org-brain-visualize-mode-map "m" 'org-brain-select)

  ;; (define-key org-brain-visualize-mode-map "m" 'org-brain-visualize-mind-map)

  (define-key org-brain-visualize-mode-map "*" 'org-brain-add-child-headline)
  (define-key org-brain-visualize-mode-map "h" 'org-brain-add-child-headline)
  (define-key org-brain-visualize-mode-map "u" 'org-brain-visualize-parent)
  (define-key org-brain-visualize-mode-map [?\t] 'forward-button)
  (define-key org-brain-visualize-mode-map [backtab] 'backward-button)
  (define-key org-brain-visualize-mode-map "f" 'org-brain-add-friendship)
  (define-key org-brain-visualize-mode-map "F" 'org-brain-remove-friendship)
  (define-key org-brain-visualize-mode-map "l" 'org-brain-add-resource)
  (define-key org-brain-visualize-mode-map "a" 'org-brain-visualize-attach)
  (define-key org-brain-visualize-mode-map "A" 'org-brain-archive)
  (define-key org-brain-visualize-mode-map "b" 'org-brain-visualize-back)
  (define-key org-brain-visualize-mode-map "\C-y" 'org-brain-visualize-paste-resource)
  (define-key org-brain-visualize-mode-map "T" 'org-brain-set-tags)
  (define-key org-brain-visualize-mode-map "q" 'org-brain-visualize-quit)
  (define-key org-brain-visualize-mode-map "w" 'org-brain-visualize-random)
  (define-key org-brain-visualize-mode-map "W" 'org-brain-visualize-wander)
  (define-key org-brain-visualize-mode-map "+" 'org-brain-show-descendant-level)
  (define-key org-brain-visualize-mode-map "-" 'org-brain-hide-descendant-level)
  (define-key org-brain-visualize-mode-map "z" 'org-brain-show-ancestor-level)
  (define-key org-brain-visualize-mode-map "Z" 'org-brain-hide-ancestor-level)
  (define-key org-brain-visualize-mode-map "e" 'org-brain-annotate-edge)
  (define-key org-brain-visualize-mode-map "\C-c\C-w" 'org-brain-refile)
  (define-key org-brain-visualize-mode-map "\C-c\C-x\C-v" 'org-toggle-inline-images))

;; ** org-ql
;; (straight-use-package 'org-ql)

;; ** Disable syntax highlighting in source code blocks
(setq org-src-fontify-natively nil)

;; ** Key
(define-key my/org-mode-map (kbd "s") 'org-schedule)

(define-key my/org-mode-map (kbd "i") 'org-toggle-inline-images)
(define-key my/org-mode-map (kbd "I") 'org-toggle-link-display)

(define-key my/org-mode-map (kbd "e") 'org-insert-link)

(define-key my/org-mode-map (kbd "p") 'org-shiftup)
(define-key my/org-mode-map (kbd "n") 'org-shiftdown)
(define-key my/org-mode-map (kbd "l") 'org-shiftright)
(define-key my/org-mode-map (kbd "h") 'org-shiftleft)

(define-key my/org-mode-map (kbd "P") 'org-shiftmetaup)
(define-key my/org-mode-map (kbd "N") 'org-shiftmetadown)
(define-key my/org-mode-map (kbd "L") 'org-shiftmetaright)
(define-key my/org-mode-map (kbd "H") 'org-shiftmetaleft)

(define-key my/org-mode-map (kbd "|") 'org-table-create-or-convert-from-region)

(define-key my/org-mode-map (kbd "z") 'org-shifttab)

(define-key my/org-mode-map (kbd "f") 'my/org-present-next)
(define-key my/org-mode-map (kbd "b") 'my/org-present-prev)

(define-key my/org-mode-map (kbd "d") 'org-deadline)

(define-key my/org-mode-map (kbd "r") 'org-refile)

(define-key my/org-mode-map (kbd "E") (lambda () (interactive) (counsel-M-x "^org export-")))

;; * Time management - chronometrist
;; (straight-use-package 'chronometrist)
;;(straight-use-package '(chronometrist :type git :host github :repo "contrapunctus-1/chronometrist"))
;;(require 'chronometrist)

;; * Outline
;; Must be set before outline is loaded
;; Required by outorg
(defvar outline-minor-mode-prefix "\M-#")

(straight-use-package 'outline)
;; (require 'outorg)

(add-hook 'prog-mode-hook 'outline-minor-mode)

;; ** Outline evil text object
(evil-define-text-object evil-around-heading (count &optional beg end type)
  "Select heading"
  (let ((top nil)
	(bot nil))
    (save-restriction
      (save-excursion
	(my/auto-narrow-to-subtree)
	(setq top (point-min))
	(setq bot (point-max))))
    (evil-range top bot)))

(evil-define-text-object evil-inside-heading (count &optional beg end type)
  "Select heading"
  (let ((top nil)
	(bot nil))
    (save-restriction
      (save-excursion
	(my/auto-narrow-to-subtree)
	(beginning-of-buffer)
	(ignore-errors
	  (next-line)
	  (beginning-of-line))
	(setq top (point))
	(setq bot (point-max))))
    (evil-range top bot)))

(define-key evil-outer-text-objects-map "h" 'evil-around-heading)
(define-key evil-inner-text-objects-map "h" 'evil-inside-heading)

;; ** Imenu
(define-key my/leader-map (kbd "i") 'counsel-imenu)

;; ** Counsel-outline
(define-key my/leader-map (kbd "TAB") 'counsel-outline)

;; ***  Fix so that counsel-outline can unfold to the line it needs to go to
(defun counsel-outline-action (x)
  "Go to outline X."
  (goto-char (cdr x))
  (outline-show-entry))

;; *** Fix counsel-outline in elisp mode
;; Elisp mode uses the classic lisp outline syntax
(setq counsel-outline-settings
      '((emacs-lisp-mode
	 :outline-regexp ";; [*]\\{1,8\\} "
	 :outline-level counsel-outline-level-emacs-lisp)
	(org-mode
	 :outline-title counsel-outline-title-org
	 :action counsel-org-goto-action
	 :history counsel-org-goto-history
	 :caller counsel-org-goto)
	;; markdown-mode package
	(markdown-mode
	 :outline-title counsel-outline-title-markdown)
	;; Built-in mode or AUCTeX package
	(latex-mode
	 :outline-title counsel-outline-title-latex)))

;; ** Outshine
(straight-use-package 'outshine)
;; (straight-use-package '(outshine :type git :host github :repo "alphapapa/outshine"))

;; Clean outshine-mode-map
(setq outshine-mode-map (make-sparse-keymap))

(add-hook 'outline-minor-mode-hook 'outshine-mode)

(setq outshine-startup-folded-p nil)

;; ** Outorg
(setq outorg-edit-buffer-persistent-message nil)
(setq outorg-unindent-active-source-blocks-p nil)

;; *** Toggle current heading
(define-key my/leader-map (kbd "f") 'my/outorg-toggle-heading)

(defun my/outorg-toggle-heading ()
  (interactive)
  (if (string= major-mode 'org-mode)
      (outorg-copy-edits-and-exit)
    (outorg-edit-as-org)))

;; *** Toggle entire buffer
(define-key my/leader-map (kbd "F") 'my/outorg-toggle)

(defun my/outorg-toggle ()
  (interactive)
  (if (string= major-mode 'org-mode)
      (outorg-copy-edits-and-exit)
    (outorg-edit-as-org '(4))))

;; *** Export
(defun my/outorg-export-to-org-file (&optional name)
  (interactive)
  (let ((buffer (generate-new-buffer "outorg-org-output"))
	(mode major-mode))
    (copy-to-buffer buffer (point-min) (point-max))
    (switch-to-buffer buffer)
    (funcall mode)
    (outorg-convert-to-org)
    (if name
	(write-file name)
      (save-buffer))
    (kill-buffer)))

;; ** Visuals
(setq counsel-outline-face-style nil)

;; *** Fontify whole line
;; This makes it so the whole line the heading is on has the heading background color
(setq outshine-fontify-whole-heading-line t)

;; ** Narrowing
;; (define-prefix-command 'my/narrow-map)
;; (define-key my/leader-map (kbd "n") 'my/narrow-map)

;; *** Narrow indirect
(defun my/narrow-indirect (beg end)
  (my/clone-indirect-buffer)
  (narrow-to-region beg end))

;; **** Evil operator
(evil-define-operator my/evil-narrow-indirect (beg end type)
  "Indirectly narrow the region from BEG to END."
  (interactive "<R>")
  (evil-normal-state)
  (my/narrow-indirect beg end))

(define-key evil-normal-state-map "M" 'my/evil-narrow-indirect)
(define-key evil-visual-state-map "M" 'my/evil-narrow-indirect)

;; **** Evil-goggle support
(with-eval-after-load 'evil-goggles
  (add-to-list 'evil-goggles--commands '(my/evil-narrow-indirect :face evil-goggles-yank-face :switch evil-goggles-enable-yank :advice evil-goggles--generic-async-advice)))

;; *** Narrow
;; **** Evil operator
(evil-define-operator my/evil-narrow (beg end type)
  "Indirectly narrow the region from BEG to END."
  (interactive "<R>")
  (evil-normal-state)
  (narrow-to-region beg end))

(define-key evil-normal-state-map "m" 'my/evil-narrow)
(define-key evil-visual-state-map "m" 'my/evil-narrow)

;; *** Universal narrow function
(defun my/narrow-widen ()
  (interactive)
  (if (and (boundp 'loccur-mode) loccur-mode)
      (loccur-mode -1)
    (widen)))

;; (define-key my/narrow-map (kbd "w") 'my/narrow-widen)
(define-key my/leader-map (kbd "n") 'my/narrow-widen)

;; *** Narrow to subtree
(defun my/auto-narrow-to-subtree ()
  (interactive)
  (pcase major-mode
    ('org-mode (org-narrow-to-subtree))
    (_
     ;; Fixes a bug where if cursor is at heading, the one above gets narrowed
     (next-line)

     (outline-previous-visible-heading 1)
     (outshine-narrow-to-subtree))))

;; (define-key my/narrow-map (kbd "i") 'my/auto-narrow-to-subtree)

;; ** Outline ellipsis
(defvar outline-display-table (make-display-table))
(set-display-table-slot outline-display-table 'selective-display
			(vector (make-glyph-code my/fold-ellipsis-char 'escape-glyph)))
(defun set-outline-display-table ()
  (setf buffer-display-table outline-display-table))

(add-hook 'outline-mode-hook 'set-outline-display-table)
(add-hook 'outline-minor-mode-hook 'set-outline-display-table)

;; ** Keys
;; *** Outline fold
(my/evil-normal-define-key "g i" 'outline-previous-visible-heading)

(my/evil-normal-define-key "g o" 'outline-toggle-children)
(my/evil-normal-define-key "g O" 'outline-show-subtree)
(my/evil-normal-define-key "g h" 'my/outline-hide-all-headings)
(my/evil-normal-define-key "g H" 'my/outline-hide-all)

(my/evil-normal-define-key "g a" 'outline-show-all)

(defun my/outline-hide-all ()
  (interactive)
  (outline-hide-sublevels 1))

(defun my/outline-hide-all-headings ()
  (interactive)
  (outline-show-all)
  (outline-hide-body))

;; *** Code fold
(my/evil-normal-define-key "g C-o" 'my/code-fold-show)
(my/evil-normal-define-key "g RET" 'my/code-fold-show-all)
(my/evil-normal-define-key "g C-h" 'my/code-fold-hide-level)

(defun my/code-fold-show ()
  (interactive)
  (if hs-minor-mode
      (hs-toggle-hiding)
    (yafolding-toggle-element)))

(defun my/code-fold-show-all ()
  (interactive)
  (if hs-minor-mode
      (hs-show-all)
    (yafolding-show-all)))

(defun my/code-fold-hide-level ()
  (interactive)
  (if hs-minor-mode
      (call-interactively 'hs-hide-level)
    (yafolding-hide-all)))

;; ** Folding
;; *** Hideshow
(defvar my/hs-ignore-modes '(fsharp-mode))

(add-hook 'prog-mode-hook (lambda () (interactive)
			    (if (not (member major-mode my/hs-ignore-modes))
				(hs-minor-mode 1))))

;; *** Yafolding
;; Used for universal folding
(straight-use-package 'yafolding)

(define-globalized-minor-mode global-yafolding-mode
  yafolding-mode yafolding-mode)
(yafolding-mode)
(global-yafolding-mode 1)

(setq yafolding-ellipsis-content my/fold-ellipsis)
(setq yafolding-show-fringe-marks nil)

;; * Completion
;; ** Posframe
;; Disable mouse banish
(setq posframe-mouse-banish nil)

;; ** Ivy
(eval-and-compile
  (straight-use-package 'ivy))
(ivy-mode 1)

(setq ivy-use-virtual-buffers nil)

;; Make user intput selectable
(setq ivy-use-selectable-prompt t)

;; *** Fix ivy dispatching
;; So when using exwm with minibuffer at the top and trying to use the ivy menu you get an error like
;; window-resize: Cannot resize the root window of a frame
;; This fixes that, it might cause bugs though
(defun ivy-shrink-after-dispatching ()
  "Shrink the window after dispatching when action list is too large."
  nil)

;; *** Visuals
(defun my/ivy-set-height ()
  (remove-hook 'post-command-hook 'my/ivy-set-height)
  (setq ivy-height (+ (frame-height) 1)))

(add-hook 'after-init-hook
	  (lambda ()
	    (add-hook 'post-command-hook 'my/ivy-set-height)))

(with-eval-after-load 'counsel
  (setq ivy-height-alist nil)
  (setq-default ivy-height-alist nil)
  (add-to-list 'ivy-height-alist '(swiper . 10))
  (add-to-list 'ivy-height-alist '(swiper-isearch . 10))
  (add-to-list 'ivy-height-alist '(counsel-switch-buffer . 10))
  (add-to-list 'ivy-height-alist '(git-backup-ivy . 3)))

;; **** Highlight whole row in minibuffer
;; Change the default emacs formatter to highlight whole row in minibuffer
(delete '(t . ivy-format-function-default) ivy-format-functions-alist)
(add-to-list 'ivy-format-functions-alist '(t . ivy-format-function-line))

;; *** Wgrep
;; Needed by ivy-occur to edit buffers
(straight-use-package 'wgrep)

;; *** Fix history
(defun ivy-next-line-or-history (&optional arg)
  "Move cursor vertically down ARG candidates.
If the input is empty, select the previous history element instead."
  (interactive "p")
  (let ((orig-index ivy--index))
    (ivy-next-line arg)
    (when (string= ivy-text "")
      (ivy-previous-history-element 1))))

;; *** Switch buffer
;; **** Ignore buffers
(defun my/ivy-switch-buffer-ignore (str)
  (let ((buf (get-buffer str)))
    (and buf (or
	      ;; (my/ignore-dired-buffers buf)
	      ;; (my/ignore-org-brain-buffers buf)
	      ))))

(with-eval-after-load 'ivy
  (add-to-list 'ivy-ignore-buffers #'my/ivy-switch-buffer-ignore))

;; ***** Ignore dired buffers
(defun my/ignore-dired-buffers (buf)
  (eq (buffer-local-value 'major-mode buf) 'dired-mode))

;; ***** Ignore
(defun my/ignore-org-brain-buffers (buf)
  (and (buffer-local-value 'buffer-file-name buf)
       (file-in-directory-p
	(buffer-local-value 'buffer-file-name buf)
	(expand-file-name org-brain-path))))

;; *** ivy-posframe
;; (straight-use-package 'ivy-posframe)

;; (ivy-posframe-mode -1)

;; *** Keys
(defun my/ivy-top ()
  (interactive)
  (ivy-previous-line ivy--length))

(defun my/ivy-bot ()
  (interactive)
  (ivy-next-line ivy--length))

(setq ivy-minibuffer-map (make-sparse-keymap))

(define-prefix-command 'my/ivy-mode-map)
(evil-define-key 'normal ivy-minibuffer-map (kbd (concat my/leader-map-key " a")) 'my/ivy-mode-map)

(define-key my/ivy-mode-map (kbd "k") 'counsel-minibuffer-history)

;; Enable avy movements in ivy buffer
(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "M-n") 'ivy-avy)
(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "M-p") 'ivy-avy)

(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "G") 'my/ivy-bot)
(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "g g") 'my/ivy-top)

(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "n") 'ivy-next-line)
(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "p") 'ivy-previous-line)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-n") 'ivy-next-line)
(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-p") 'ivy-previous-line)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-,") 'ivy-call)

(define-key ivy-minibuffer-map [remap evil-ret] 'ivy-done)
(define-key ivy-minibuffer-map [remap newline] 'ivy-done)

(define-key ivy-minibuffer-map (kbd "C-c") 'ivy-dispatching-done)

(define-key ivy-minibuffer-map [remap keyboard-quit] 'minibuffer-keyboard-quit)
(define-key minibuffer-inactive-mode-map [remap keyboard-quit] 'minibuffer-keyboard-quit)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-u") 'ivy-scroll-down-command)
(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-w") 'ivy-scroll-up-command)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-o") 'ivy-occur)

;; Doesn't work with dedicated minibuffer window?
;; evil-define-key '(motion normal) ivy-minibuffer-map  (kbd "C-y") 'ivy-dispatching-done)
;; define-key 'insert ivy-minibuffer-map  (kbd "C-y") 'ivy-dispatching-done)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-s") 'ivy-next-line-or-history)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "TAB") 'ivy-partial-or-done)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "RET") 'ivy-done)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-y") 'ivy-insert-current)

;; Clear ivy input
(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "D") (lambda () (interactive) (beginning-of-line-text)
								 (evil-delete-char (+ 1 (point)) (point-max))
								 (delete-char 1)))

;; ** Counsel
(straight-use-package 'counsel)

(counsel-mode 1)

;; (setq-default counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
(setq counsel-grep-base-command "grep -i -E -n -e %s %s")

;; *** Counsel-rg
(setq counsel-rg-base-command "rg --with-filename --no-heading --line-number --color never %s")

;; *** Counsel-yank-pop
;; Delete text under selection when pasing just like with normal evil paste
(advice-add #'counsel-yank-pop :before (lambda (&optional arg)
					 (if (string= evil-state 'visual)
					     (delete-region (point) (mark)))))

;; *** Always run counsel ag in default directory
(defvar my/rg-available (executable-find "rg"))

(defun my/counsel-grep ()
  (interactive)
  (if my/rg-available
      (counsel-rg nil default-directory)
    (counsel-ag nil default-directory)))

;; *** Remove dups
;; **** Remove duplicate entries in kill ring
(advice-add #'counsel-yank-pop :before (lambda (&optional arg) (delete-dups kill-ring)))

;; **** Remove duplicate entries in yank ring
(advice-add #'counsel-mark-ring :before (lambda (&optional arg) (delete-dups mark-ring)))

;; *** Find file
;; **** Filter out .. and .
(setq counsel-find-file-ignore-regexp "^\\./$\\|^\\.\\./$")
;; In ivy you can't press ~/~ on the first entry to enter that directory because there is supposed to be ~.~ there
(setq ivy-extra-directories '("./"))

;; *** Keys
;; (define-key my/leader-map (kbd "g") 'counsel-M-x)
(global-set-key (kbd "M-c") 'counsel-M-x)

(global-set-key (kbd "M-k") 'counsel-yank-pop)

(define-key ivy-minibuffer-map (kbd "DEL") 'ivy-backward-delete-char)

;; ** Counsel flycheck
;;   https://github.com/nathankot/dotemacs/blob/master/init.el
(defvar my/counsel-flycheck-history nil
  "History for `counsel-flycheck'")

(defun my/counsel-flycheck ()
  (interactive)
  (if (not (bound-and-true-p flycheck-mode))
      (message "Flycheck mode is not available or enabled")
    (ivy-read "Error: "
	      (let ((source-buffer (current-buffer)))
		(with-current-buffer (or (get-buffer flycheck-error-list-buffer)
					 (progn
					   (with-current-buffer
					       (get-buffer-create flycheck-error-list-buffer)
					     (flycheck-error-list-mode)
					     (current-buffer))))
		  (flycheck-error-list-set-source source-buffer)
		  (flycheck-error-list-reset-filter)

		  ;; Finally, refresh the error list to show the most recent errors
		  (flycheck-error-list-refresh)

		  (revert-buffer t t t)
		  (split-string (buffer-string) "\n" t)))
	      :action (lambda (s &rest _)
			(let* ((error (get-text-property 0 'tabulated-list-id s))
			       (pos (flycheck-error-pos error)) )
			  (goto-char (flycheck-error-pos error))))
	      :history 'my/counsel-flycheck-history)))

(define-key my/leader-map (kbd "J") 'my/counsel-flycheck)

;; ** Swiper
(straight-use-package 'swiper)

;; (defun my/use-swiper-or-grep (&optional input case-sensative)
;;   (interactive)
;;   (swiper input))
;; (if (and buffer-file-name (not (bound-and-true-p org-src-mode)) (not (string= "gz" (file-name-extension buffer-file-name))))
;; (counsel-grep input)
;; (swiper input)))

;; Checks for if case sensative search
;; (if case-sensative
;; (setq counsel-grep-base-command "grep -E -n -e %s %s")
;; (setq-default counsel-grep-base-command "grep -i -E -n -e %s %s"))

(global-set-key (kbd "C-s") 'swiper)
(define-key swiper-map (kbd "M-j") 'swiper-query-replace)

;;  (setq swiper-use-visual-line t)

;; *** Disable visual line search
;; When using visual line mode swiper also searches every visual line, not just every line. This is really slow
(setq swiper-use-visual-line nil)
(setq swiper-use-visual-line-p (lambda (a) nil))

;; *** Search for thing-at-point
(defun my/swiper-thing-at-point ()
  "jump to word under cursor"
  (interactive)
  (my/use-swiper-or-grep (thing-at-point 'symbol)))

(my/evil-normal-define-key "#" 'my/swiper-thing-at-point)
(my/evil-normal-define-key "*" 'my/swiper-thing-at-point)

;; ** Ivy rich
(straight-use-package 'ivy-rich)

(defvar my/ivy-rich-docstring-spacing 40)

;; *** Faces
(defface my/ivy-rich-doc-face
  '((t :inherit font-lock-doc-face))
  "Face used for the doc face in ivy rich buffers")

(defface my/ivy-rich-switch-buffer-size-face
  '((t :inherit default))
  "Face used by ivy rich")

(defface my/ivy-rich-switch-buffer-path-face
  '((t :inherit default))
  "Face used by ivy rich")

(defface my/ivy-rich-switch-buffer-project-face
  '((t :inherit default))
  "Face used by ivy rich")

(defface my/ivy-rich-switch-buffer-indicator-face
  '((t :inherit default))
  "Face used by ivy rich")

(defface my/ivy-rich-switch-buffer-major-mode-face
  '((t :inherit default))
  "Face used by ivy rich")

(defface my/ivy-rich-find-file-symlink-face
  '((t :inherit default))
  "Face used by ivy rich")

;; *** Transformers
;; **** Don't shorten path
(defun my/ivy-rich-path (file)
  (buffer-file-name
   (get-buffer file)))

;; *** Set transformers list
;; (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-switch-buffer)
;; (setq ivy-rich-path-style 'abbrev)

(setq ivy-rich-display-transformers-list
      ;; Switch buffer
      `(
	;; ivy-switch-buffer
	;; (:columns
	;;  ((ivy-rich-candidate (:width 80))
	;;   ;;(ivy-rich-switch-buffer-size (:width 7 :face my/ivy-rich-switch-buffer-size-face))
	;;   (ivy-rich-switch-buffer-indicators (:width 4 :face my/ivy-rich-switch-buffer-indicator-face :align right))
	;;   (ivy-rich-switch-buffer-major-mode (:width 20 :face my/ivy-rich-switch-buffer-major-mode-face))
	;;   ;; (ivy-rich-switch-buffer-project (:width 20 :face my/ivy-rich-switch-buffer-project-face))
	;;   ;; (my/ivy-rich-path)

	;;   ;; These two takes a lot of memory and cpu
	;;   ;;(ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))) :face my/ivy-rich-switch-buffer-path-face))
	;;   )
	;;  :predicate
	;;  (lambda (cand) (get-buffer cand)))
	;; Find file
	counsel-find-file
	(:columns
	 ((ivy-read-file-transformer)
	  (ivy-rich-counsel-find-file-truename (:face my/ivy-rich-find-file-symlink-face))))
	;; M-x
	counsel-M-x
	(:columns
	 ((counsel-M-x-transformer (:width 40))
	  (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
	;; Describe function
	counsel-describe-function
	(:columns
	 ((counsel-describe-function-transformer (:width ,my/ivy-rich-docstring-spacing))
	  (ivy-rich-counsel-function-docstring (:face my/ivy-rich-doc-face))))
	;; Describe variable
	counsel-describe-variable
	(:columns
	 ((counsel-describe-variable-transformer (:width ,my/ivy-rich-docstring-spacing))
	  (ivy-rich-counsel-variable-docstring (:face my/ivy-rich-doc-face))))
	;; Recentf
	counsel-recentf
	(:columns
	 ((ivy-rich-candidate (:width 0.8))
	  (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))))

(ivy-rich-mode 1)

;; ** Company
(straight-use-package 'company)

(setq company-idle-delay 0)
;; Sets how long before company echoes tooltips in the minibuffer. Normally company and eldocs fights eachother if this is 0. This is fixed using hooks in "Fix company and eldoc"
(setq company-echo-delay 0)

;; Don't downcase result
(setq company-dabbbrev-downcase nil)

;; Make tooltim margin minimal
(setq company-tooltip-margin 2)

;; Start searching for candidates when 2 letters has been written
(setq company-minimum-prefix-length 2)

(with-eval-after-load 'company
  (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix))

(setq company-show-numbers t)

;; Only show 10 candidates at a time
(setq company-tooltip-limit 10)

;; Align annotations to right side
(setq company-tooltip-align-annotations t)

;; Makes it possible to exit company without a candidate selected
(setq company-require-match nil)

;; Enable scrollbar
(setq company-tooltip-offset-display 'scrollbar) ;;'line

(global-company-mode t)

;; Remove dabbrev because it's slow in long files
(setq company-backends (delete 'company-dabbrev company-backends))

;; *** Fix company and eldoc
;; Where the company menu is up, eldoc shouldn't write to the minibuffer because company is already writing documentation there
(add-hook 'company-completion-started-hook (lambda (a) (setq-local eldoc-idle-delay 100)))
(add-hook 'company-after-completion-hook (lambda (a) (setq-local eldoc-idle-delay my/eldoc-idle-delay)))

;; *** Company posframe
;; (straight-use-package 'company-posframe)
;; (company-posframe-mode)

;; *** Company doc buffer
;; Company doc mode disables visual line mode for whatever reason, enable it inside this redefinition of company-show-doc-buffer
(defun my/company-show-doc-buffer ()
  "Temporarily show the documentation buffer for the selection."
  (interactive)
  (let (other-window-scroll-buffer)
    (company--electric-do
      (let* ((selected (nth company-selection company-candidates))
	     (doc-buffer (or (company-call-backend 'doc-buffer selected)
			     (user-error "No documentation available")))
	     start)
	(setq-local truncate-lines nil)
	(visual-line-mode 1)
	(when (consp doc-buffer)
	  (setq start (cdr doc-buffer)
		doc-buffer (car doc-buffer)))
	(setq other-window-scroll-buffer (get-buffer doc-buffer))
	(let ((win (display-buffer doc-buffer t)))
	  (set-window-start win (if start start (point-min))))))))

(define-key company-active-map (kbd "C-o") 'my/company-show-doc-buffer)

(defun my/company-show-doc-buffer-keep-open ()
  "Temporarily show the documentation buffer for the selection."
  (interactive)
  (let (other-window-scroll-buffer)
    (let* ((selected (nth company-selection company-candidates))
	   (doc-buffer (or (company-call-backend 'doc-buffer selected)
			   (user-error "No documentation available")))
	   start)
      (setq-local truncate-lines nil)
      (visual-line-mode 1)
      (when (consp doc-buffer)
	(setq start (cdr doc-buffer)
	      doc-buffer (car doc-buffer)))
      (setq other-window-scroll-buffer (get-buffer doc-buffer))
      (let ((win (display-buffer doc-buffer t)))
	(set-window-start win (if start start (point-min)))))))

(define-key company-active-map (kbd "C-S-h") 'my/company-show-doc-buffer-keep-open)

;; *** Company-show-numbers but with letters
;; Need to implement
;; Letters, etc for autocomplete
;; line 2769, might also need to change more lines due to "company show numbers" being at a few more places
;;  (setq right (concat (format " %s" (nth numbered '(a s d f g h j k l i r))) right)))


;; **** Find function that gets hotkey
;; name "company-complete-number"

;; *** Disable quickhelp for good
;; fsharp mode auto-enables quickhelp by default, disable it
(setq company-quickhelp-delay nil)

;; *** Visuals
;; **** Lighter
(defun my/company--group-lighter (candidate base)
  (let ((backend (or (get-text-property 0 'company-backend candidate)
		     (cl-some (lambda (x) (and (not (keywordp x)) x))
			      company-backend))))
    (when (and backend (symbolp backend))
      (let ((name (replace-regexp-in-string "company-\\|-company" ""
					    (symbol-name backend))))
	(format "%s-%s | " base name)))))

;; ***** Set base
(setq company-lighter-base "company")

;; *** Keys
(with-eval-after-load 'company
  (setq company-active-map (make-sparse-keymap))
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "C-g") 'company-abort)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map (kbd "C-u") 'company-previous-page)
  (define-key company-active-map (kbd "C-w") 'company-next-page)

  ;; Complete on tab
  ;; (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "<f1>") 'nil))

;; ** Company-box
;; Company with icons
;; Doesn't work with my setup right now
;; (when window-system
;;  (straight-use-package 'company-box)

;;  (add-hook 'company-mode-hook 'company-box-mode)

;;  ;;(setq company-box-minimum-width 100)
;;  ;;(setq company-box--height 500)
;;  ;;(setq company-tooltip-minimum 10)

;;  (remove-hook 'company-box-selection-hook 'company-box-doc)
;;  (remove-hook 'company-box-hide-hook 'company-box-doc--hide))

;; ** Flycheck
(setq flycheck-mode-map (make-sparse-keymap))
(straight-use-package 'flycheck)

;; Decrease delay
(setq flycheck-display-errors-delay 0.1)

(global-flycheck-mode)

;; *** Disable switch buffer delay
(setq flycheck-idle-buffer-switch-delay nil)

;; *** Disable flycheck fringe
(setq flycheck-indication-mode nil)

;; *** Flycheck display methods
;; **** Custom display in window
(defun my/flycheck-display-message (errors)
  (let* ((buf (get-buffer-create "*flycheck-msg*"))
	 (win (get-buffer-window buf)))
    ;; Create window if none
    (unless win
      (let ((new-win (split-window-below)))
	(with-selected-window new-win
	  (switch-to-buffer buf)
	  (shrink-window (/ (frame-height) 4)))))
    ;; Insert error
    (with-current-buffer buf
      (erase-buffer)
      (mapc (lambda (a) (insert (flycheck-error-format a))) errors))))

;; **** Flycheck-posframe
;; (when window-system
;;   (straight-use-package 'flycheck-posframe)

;;   (with-eval-after-load 'flycheck
;;     (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)))

;; (setq my/flycheck-posframe-symbol nil)
;; ;; (setq my/flycheck-posframe-symbol "→ ")

;; (setq flycheck-posframe-error-prefix my/flycheck-posframe-symbol)
;; (setq flycheck-posframe-info-prefix my/flycheck-posframe-symbol)
;; (setq flycheck-posframe-prefix my/flycheck-posframe-symbol)
;; (setq flycheck-posframe-warning-prefix my/flycheck-posframe-symbol)

;; ;; (setq flycheck-posframe-position 'frame-bottom-right-corner)

;; **** Flycheck pos-tip
;; (straight-use-package 'flycheck-pos-tip)

;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode 1))

;; **** Flycheck inline
(straight-use-package 'flycheck-inline)

;; Put flycheck-inline at BOL to make it always readable
(setq flycheck-inline-display-function (lambda (msg _) (flycheck-inline-display-phantom msg (line-beginning-position))))

(with-eval-after-load 'flycheck
  (require 'flycheck-inline)
  (add-hook 'flycheck-mode-hook #'turn-on-flycheck-inline))

;; *** Flycheck-package
;; Flycheck with extra correction for elisp packages
(straight-use-package 'flycheck-package)

(with-eval-after-load 'flycheck
  (flycheck-package-setup))

;; *** Copy flycheck errors contents
(define-key my/leader-map (kbd "y") 'flycheck-copy-errors-as-kill)

;; ** Which-key
;; (straight-use-package 'which-key)

;; (which-key-mode)

;; (setq which-key-idle-delay 1)

;; ** Yasnippet
(setq yas-minor-mode-map (make-sparse-keymap))

(straight-use-package 'yasnippet)

(yas-global-mode 1)

;; *** Company integration
;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
	    '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; *** Ivy integration
(straight-use-package 'ivy-yasnippet)
;; Needed because its font isn't loaded on install, but is needed in theme
(define-key my/leader-map (kbd "I") 'ivy-yasnippet)

;; *** Keys
(my/evil-normal-define-key "TAB" #'my/auto-tab)
(my/evil-insert-define-key "TAB" #'my/auto-tab)

;; * Movement
;; ** Isearch
(define-key isearch-mode-map (kbd "C-n") 'my/isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-p") 'my/isearch-repeat-backward)

(define-key isearch-mode-map (kbd "C-w") (lambda () (interactive) () (my/isearch-repeat-times t 5)))
(define-key isearch-mode-map (kbd "C-u") (lambda () (interactive) () (my/isearch-repeat-times nil 5)))

(setq isearch-lazy-highlight t)
(setq lazy-highlight-initial-delay 0)

;; *** Stop from having to press C-n two times after pressing C-p
(defvar my/last-isearch-dir nil)

(defun my/isearch-repeat-times (forward &optional count)
  (dotimes (i count)
    (my/isearch-repeat forward)))

(defun my/isearch-repeat (forward)
  (interactive)
  (if forward
      (my/isearch-repeat-forward)
    (my/isearch-repeat-backward)))

(defun my/isearch-repeat-forward ()
  (interactive)
  (call-interactively 'isearch-repeat-forward)

  (when (string= my/last-isearch-dir 'backward)
    (call-interactively 'isearch-repeat-forward))
  (setq my/last-isearch-dir 'forward))

(defun my/isearch-repeat-backward ()
  (interactive)
  (call-interactively 'isearch-repeat-backward)

  (when (string= my/last-isearch-dir 'forward)
    (call-interactively 'isearch-repeat-backward))
  (setq my/last-isearch-dir 'backward))

(add-hook 'isearch-mode-end-hook (lambda () (setq my/last-isearch-dir nil)))

;; *** Make isearch end when only one match
;; (defun my/ret-if-one-isearch-match ()
;;   (interactive)
;;   (set-buffer my/isearch-current-buffer)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (if (= (count-matches isearch-string) 1)
;;         (setq unread-command-events (listify-key-sequence "\C-m")))))

;; (add-hook 'isearch-update-post-hook 'my/ret-if-one-isearch-match)


;; *** Replace isearch regexp search with ivy-regex-plus
(defun my/isearch-forward-regexp (&optional not-regexp no-recursive-edit)
  (interactive "P\np")
  (isearch-mode t (null not-regexp) nil (not no-recursive-edit) 'my/ivy--regex-plus))

(defun my/isearch-backward-regexp (&optional not-regexp no-recursive-edit)
  (interactive "P\np")
  (isearch-mode nil (null not-regexp) nil (not no-recursive-edit) 'my/ivy--regex-plus))

(defun my/ivy--regex-plus (string &optional lax)
  (ivy--regex-plus string))

;; ** Goto middle of line
(defun my/go-to-middle-of-line ()
  (interactive)
  (goto-char (/  (+ (line-end-position) (line-beginning-position)) 2)))

(my/evil-normal-define-key "^" 'my/go-to-middle-of-line)

;; ** Navigate indent
;; https://emacs.stackexchange.com/questions/20900/navigate-by-indentation
(defun indentation-get-next-good-line (direction skip good)
  "Moving in direction `direction', and skipping over blank lines and lines that
    satisfy relation `skip' between their indentation and the original indentation,
    finds the first line whose indentation satisfies predicate `good'."
  (let ((starting-indentation (current-indentation))
	(lines-moved direction))
    (save-excursion
      (while (and (zerop (forward-line direction))
		  (or (eolp)  ; Skip past blank lines and other skip lines
		      (funcall skip (current-indentation) starting-indentation)))
	(setq lines-moved (+ lines-moved direction)))
      ;; Now we can't go further. Which case is it?
      (if (and
	   (not (eobp))
	   (not (bobp))
	   (funcall good (current-indentation) starting-indentation))
	  lines-moved
	nil))))

(defun indentation-get-next-sibling-line ()
  "The line number of the next sibling, if any."
  (indentation-get-next-good-line 1 '> '=))

(defun indentation-get-previous-sibling-line ()
  "The line number of the previous sibling, if any"
  (indentation-get-next-good-line -1 '> '=))

(defun indentation-get-parent-line ()
  "The line number of the parent, if any."
  (indentation-get-next-good-line -1 '>= '<))

(defun indentation-get-child-line ()
  "The line number of the first child, if any."
  (indentation-get-next-good-line +1 'ignore '>))

(defun indentation-move-to-line (func preserve-column name)
  "Move the number of lines given by func. If not possible, use `name' to say so."
  (let ((saved-column (current-column))
	(lines-to-move-by (funcall func)))
    (if lines-to-move-by
	(progn
	  (forward-line lines-to-move-by)
	  (move-to-column (if preserve-column
			      saved-column
			    (current-indentation))))
      (message "No %s to move to." name))))

(defun indentation-forward-to-next-sibling ()
  "Move to the next sibling if any, retaining column position."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-next-sibling-line t "next sibling"))

(defun indentation-backward-to-previous-sibling ()
  "Move to the previous sibling if any, retaining column position."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-previous-sibling-line t "previous sibling"))

(defun indentation-up-to-parent ()
  "Move to the parent line if any."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-parent-line nil "parent"))

(defun indentation-down-to-child ()
  "Move to the first child line if any."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-child-line nil "child"))

;; *** Keys
(my/evil-normal-define-key "C-n" 'indentation-forward-to-next-sibling)
(my/evil-visual-define-key "C-n" 'indentation-forward-to-next-sibling)

(my/evil-normal-define-key "C-p" 'indentation-backward-to-previous-sibling)
(my/evil-visual-define-key "C-p" 'indentation-backward-to-previous-sibling)

(my/evil-normal-define-key "C-h" 'indentation-up-to-parent)
(my/evil-visual-define-key "C-h" 'indentation-up-to-parent)

(my/evil-normal-define-key "<deletechar>" 'indentation-down-to-child)
(my/evil-visual-define-key "<deletechar>" 'indentation-down-to-child)

;; ** Marks
(setq mark-ring-max 100)

;; *** Bind counsel-mark-ring
(my/evil-universal-define-key "C-;" 'counsel-mark-ring)

;; *** History
;; This is buggy
;; (straight-use-package 'history)

;; (setq history-window-local-history t)
;; (setq history-history-max 100)
;; (setq history-delete-duplicates t)

;; ;; These needs to be run before `history-mode'
;; (setq history-advised-before-functions '(imenu isearch-mode beginning-of-buffer end-of-buffer swiper undo-tree-undo))
;; (setq history-advised-after-functions '(swiper undo-tree-undo))

;; (history-mode 1)

;; **** Keys
;; (my/evil-universal-define-key "C-b" 'history-prev-history)
;; (my/evil-universal-define-key "C-o" 'history-next-history)

;; *** back-button
(straight-use-package 'back-button)

(setq back-button-no-wrap nil)

;; **** Keys
(my/evil-universal-define-key "C-b" 'back-button-local-backward)
(my/evil-universal-define-key "C-o" 'back-button-local-forward)

;; ** Avy
(straight-use-package 'avy)

(setq avy-all-windows nil)

(setq avy-keys '(
		 ;; Easy
		 ?a ?n ?e ?t ?o ?s ?h ?d ?i ?r
		 ;; Med
		 ?g ?m ?l ?w ?y ?f ?u ?b ?x ?c ?v ?k ?p ?, ?.
		 ;; Hard
		 ?q ?\; ?j ?\/ ?z

		 ;; Shifted

		 ;; Easy
		 ?A ?N ?E ?T ?O ?S ?H ?D
		 ;; Med
		 ?R ?I ?G ?M ?L ?W ?Y ?F ?U ?B ?X ?C ?V ?K ?P
		 ;; Hard
		 ?Q ?\: ?J ?\? ?Z

		 ;; Digits
		 ?7 ?4 ?8 ?3 ?9 ?2 ?0 ?1
		 ))

;; Disable highlighting when avy is used. Doesn't work on 16 color terminals
(if window-system (setq avy-background t))

;; *** Avy-goto-line
;; **** Above
(defun my/avy-goto-line-above-keep-horizontal-position (&optional arg)
  (interactive "p")
  (setq cursor-horizontal-pos (current-column))

  ;; Fixes problem with goto-line and visual line mode
  (if (eq evil-state 'visual)
      (if (eq (evil-visual-type) 'line)
	  (progn
	    (setq was-visual-line t)
	    (evil-visual-char)))
    (progn
      (setq was-visual-line nil)
      (my/evil-normal-state arg)))

  (avy-goto-line-above 2 t)

  (if (eq was-visual-line t)
      (evil-visual-line))

  (move-to-column cursor-horizontal-pos))

;; **** Below
(defun my/avy-goto-line-below-keep-horizontal-position (&optional arg)
  (interactive "p")
  (setq cursor-horizontal-pos (current-column))

  ;; Fixes problem with goto-line and visual line mode
  (if (eq evil-state 'visual)
      (if (eq (evil-visual-type) 'line)
	  (progn
	    (setq was-visual-line t)
	    (evil-visual-char)))
    (progn
      (setq was-visual-line nil)
      (my/evil-normal-state arg)))

  (avy-goto-line-below 2)

  (if (eq was-visual-line t)
      (evil-visual-line))

  (move-to-column cursor-horizontal-pos))

;; *** Avy-goto-word
(defun my/avy-goto-word-0-in-line(&optional arg)
  (interactive "p")
  (if (not (eq evil-state 'visual))
      (my/evil-normal-state arg))

  (avy-goto-subword-0 t nil (line-beginning-position) (line-end-position)))

;; *** Avy-goto-subword-0
;; **** Below
(defun my/avy-goto-subword-0-below()
  (interactive)
  (avy-goto-subword-0 t nil (point) (window-end (selected-window) t)))

;; **** Above
(defun my/avy-goto-subword-0-above()
  (interactive)
  (avy-goto-subword-0 t nil (window-start (selected-window)) (point)))

;; ** Avy flycheck
(straight-use-package 'avy-flycheck)

(define-key my/leader-map (kbd "j") 'avy-flycheck-goto-error)

;; ** Link-hint
(straight-use-package 'link-hint)

;; ** Scroll
(my/evil-universal-define-key "C-u" 'evil-scroll-up)
(my/evil-universal-define-key "C-w" 'evil-scroll-down)

;; *** On-screen
;; (straight-use-package 'on-screen)
;; (on-screen-global-mode)
;; (setq on-screen-highlight-method 'fringe)

;; ** Jammer
(straight-use-package 'jammer)
(setq jammer-repeat-delay 0.5)
(setq jammer-repeat-window 1)
(setq jammer-type 'repeat)
(setq jammer-block-type 'blacklist)
(setq jammer-block-list '(
			  backward-delete-char
			  delete-char
			  ;;ivy-backward-delete-char
			  ;;			  ;; Backward/forward
			  ;;			  evil-backward-char evil-forward-char evil-previous-line evil-next-line previous-line next-line
			  ;;			  ;; Dired
			  ;;			  dired-next-line dired-previous-line
			  ;;			  word movements
			  ;;			  evil-forward-word evil-forward-word-begin evil-forward-word-end evil-backward-word-begin
			  ;;			  ;; WORD movements
			  ;;			  evil-forward-WORD evil-forward-WORD-begin evil-forward-WORD-end evil-backward-WORD-begin
			  ;;			  evil-backward-word-begin evil-backward-word-end
			  ))
;; (jammer-mode)

;; ** goto change
;; g-; and g-,
(straight-use-package 'goto-chg)

;; ** Change default directory
(defun my/change-default-directory ()
  (interactive)
  (let ((dir (read-file-name "Change default dir: ")))
    (if (f-dir-p dir)
	(setq default-directory dir))))

;; ** Keys
;; (my/evil-normal-define-key "M-f" 'avy-goto-char-in-line)
;; (define-key my/leader-map (kbd "f") 'avy-goto-char-in-line)

;; (my/evil-normal-define-key "M-w" 'my/avy-goto-word-0-in-line)
;; (define-key my/leader-map (kbd "w") 'my/avy-goto-word-0-in-line)

;; (my/evil-normal-define-key "M-g" 'avy-goto-char-2)
;; (define-key my/leader-map (kbd "g") 'avy-goto-char-2)

;; (my/evil-normal-define-key "M-g" 'avy-goto-char-2)
;; (define-key my/leader-map (kbd "g") 'avy-goto-char-2)

;; (my/evil-normal-define-key "M-n" 'avy-goto-word-0-below)
;;   (define-key evil-normal-state-map (kbd "M-p") 'avy-goto-word-0-above)

;; (define-key my/leader-map (kbd "n") 'my/avy-goto-line-below-keep-horizontal-position)
;; (define-key my/leader-map (kbd "p") 'my/avy-goto-line-above-keep-horizontal-position)

;; (define-key my/keys-mode-map (kbd "M-l") 'link-hint-open-link)
(define-key my/leader-map (kbd "l") 'link-hint-open-link)
;; (define-key my/keys-mode-map (kbd "M-???") 'link-hint-copy-link)

;; * Bookmark management
(defun my/select-bookmark (message)
  (ivy-read message (bookmark-all-names)))

(defun my/add-bookmark ()
  (interactive)
  (bookmark-set (my/select-bookmark "Add bookmark ")))

(defun my/delete-bookmark ()
  (interactive)
  (bookmark-delete (my/select-bookmark "Delete bookmark ")))

;; doesn't work
;; (defun my/load-bookmark-file ()
;; (interactive)
;; (bookmark-load (ivy-read "load bookmark file " nil)))

;; * Buffer management
;; ** Bufler
;; (straight-use-package 'bufler)

;; (cl-defun bufler-buffer-alist-at (path)
;;   (interactive "P")
;;   (let* ((level-start (pcase path
;;			('nil 0)
;;			(_ (1+ (length (-take-while #'null path))))))
;;	 (grouped-buffers (bufler-buffers :path path))
;;	 (paths (bufler-group-tree-paths grouped-buffers)))
;;     (cl-labels ((format-heading
;;		 (heading level) (propertize heading
;;					     'face (bufler-level-face level)))
;;		;; All the changes are here!!
;;		;; Nothing is changed beyond adding: (my/bufler-format
;;		(format-path
;;		 (path) (my/bufler-format (string-join (cl-loop for level from level-start
;;								for element in path
;;								collect (cl-typecase element
;;									  (string (format-heading element level))
;;									  (buffer (buffer-name element))))
;;						       bufler-group-path-separator)))
;;		(path-cons
;;		 (path) (cons (format-path (-non-nil path)) (-last-item path))))
;;       (mapcar #'path-cons paths))))

;; ;; This is where all my edits are
;; (defun my/bufler-format (str-prop)
;;   (let* ((str (substring-no-properties str-prop))
;;	 (split-str (split-string str bufler-group-path-separator))
;;	 (str-new
;;	  (pcase (length split-str)
;;	    (1 str)
;;	    (2 (format "%-70s %-60s" (nth 1 split-str) (nth 0 split-str)))
;;	    (3 (format "%-70s %-60s %s" (nth 2 split-str) (nth 1 split-str) (nth 0 split-str))))))
;;     str-new))

;; * Window management
;; ** Window split functions
(defun my/window-split-up ()
  (interactive)
  (split-window nil nil 'above))

(defun my/window-split-down ()
  (interactive)
  (split-window nil nil 'below))

(defun my/window-split-left ()
  (interactive)
  (split-window nil nil 'left))

(defun my/window-split-right ()
  (interactive)
  (split-window nil nil 'right))

;; ** Window config manager
(defvar my/window-config-name-changed-hook nil
  "Hook called after user has loaded a window configuration")

(defvar my/window-configurations nil)
(defvar my/current-window-configuration "None")

(defun my/get-window-config-names ()
  (mapcar #'car my/window-configurations))

(defun my/select-window-config (message)
  (ivy-read message (my/get-window-config-names)))

(defun my/get-selected-window-config-position (selected-config)
  (cl-position selected-config (my/get-window-config-names) :test 'string=))

(defun my/update-current-window-config ()
  (setq my/current-window-configuration my/selected-window-config)
  (run-hooks 'my/window-config-name-changed-hook))

(defun my/add-window-config ()
  (interactive)
  (setq my/selected-window-config (my/select-window-config "Add window config: "))

  (setq my/selected-window-config-position (my/get-selected-window-config-position my/selected-window-config))

  (if (eq my/selected-window-config-position nil)
      (push (list my/selected-window-config (current-window-configuration)) my/window-configurations)
    (setf (nth my/selected-window-config-position my/window-configurations) (list my/selected-window-config (current-window-configuration))))

  (my/update-current-window-config))

(defun my/load-window-config ()
  (interactive)
  (setq my/selected-window-config (my/select-window-config "Load window config "))
  (set-window-configuration (nth 1 (nth (my/get-selected-window-config-position my/selected-window-config) my/window-configurations)))

  (my/update-current-window-config))

(defun my/delete-window-config ()
  (interactive)
  (setq my/window-configurations (delete (nth (my/get-selected-window-config-position (my/select-window-config "Delete window config ")) my/window-configurations) my/window-configurations)))

;; ** Winner-mode
;; Remove default keys
(setq winner-mode-map (make-sparse-keymap))

(winner-mode)

;; ** No more window resetting
;; (defmacro save-window-excursion (&rest body)
;;  )

;; ** Switch to minibuffer
(defun my/toggle-switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (window-minibuffer-p)
      (if (minibuffer-selected-window)
	  (select-window (minibuffer-selected-window))
	(minibuffer-keyboard-quit))
    (select-window (active-minibuffer-window))))

(define-key my/keys-mode-map (kbd "C-j") 'my/toggle-switch-to-minibuffer)
(my/evil-universal-define-key "C-j" 'my/toggle-switch-to-minibuffer)

;; ** Window and buffer management hydra
(defhydra my/window-hydra (:hint nil
				 :color red
				 :pre
				 (setq my/window-hydra/hint
				       (concat "next: "
					       (let ((list (ivy--buffer-list "")))
						 (if (and (string= (car list) (buffer-name))
							  ;; If there is only 1 buffer in emacs
							  (> (length list) 1))
						     (substring-no-properties
						      (nth 1 list))
						   (substring-no-properties
						    (car list))))))
				 :post
				 (if exwm-firefox-evil-mode
				     (call-interactively 'exwm-firefox-evil-normal)
				   (call-interactively 'evil-force-normal-state)))
  "movement"

  ;; Move focus
  ("p" evil-window-up nil)
  ("n" evil-window-down nil)
  ("l" evil-window-right nil)
  ("h" evil-window-left nil)

  ;; Move focus to edge window
  ;; Frame border window
  ("|" evil-window-mru nil)

  ;; Move window
  ;; Move up
  ("P" evil-move-very-top nil)
  ;; Move down
  ("N" evil-move-very-bottom nil)
  ;; Move right
  ("L" evil-move-far-right nil)
  ;; Move left
  ("H" evil-move-far-left nil)

  ;; Switch monitor right
  ("$" my/switch-monitor-right nil)
  ;; Switch monitor left
  ("0" my/switch-monitor-left nil)

  ;; Resize window
  ;; Resize up
  ("C-p" (evil-window-increase-height 10) nil)
  ;; Resize down
  ("C-n" (evil-window-decrease-height 10) nil)
  ;; Resize right
  ;;("<delete>" (evil-window-decrease-width 10) nil)
  ;; ("<deletechar>" (evil-window-decrease-width 10) nil)

  ("<deletechar>" (evil-window-decrease-width 10) nil)
  ("DEL" (evil-window-decrease-width 10) nil)
  ;; Resize left
  ;;("\b" (evil-window-increase-width 10) nil)
  ("C-h" (evil-window-increase-width 10) nil)

  ;; Resize up
  ("C-S-p" (evil-window-increase-height 40) nil)
  ;; Resize down
  ("C-S-n" (evil-window-decrease-height 40) nil)
  ;; Resize right
  ("C-S-l" (evil-window-decrease-width 40) nil)
  ;; Resize left
  ("C-S-h" (evil-window-increase-width 40) nil)


  ;; Split
  ("o" my/window-split-right nil)
  ("O" my/window-split-left nil)
  ("v" my/window-split-down nil)
  ("V" my/window-split-up nil)

  ;; ("i" my/clone-indirect-buffer-other-window nil)
  ("Y" my/clone-indirect-buffer nil)

  ;; Search
  ("C-s" swiper-all nil)

  ;; Close window
  ("s" delete-window nil)
  ;; Focus on window
  ("d" delete-other-windows nil)

  ;; minimize window
  ("S" (lambda () (interactive) (evil-window-increase-height 1000) (evil-window-increase-width 1000)) nil)
  ;; maximize window
  ("D" (lambda () (interactive) (evil-window-decrease-height 1000) (evil-window-decrease-width 1000)) nil)

  ;; Buffer management
  ;; Find file
  ("e" counsel-find-file nil)
  ("E" my/dired-curr-dir nil)
  ("M-e" my/change-default-directory nil)

  ;; Find
  ("f" my/auto-find nil)
  ("F" my/auto-grep nil)

  ;; Browser
  ("b" my/switch-w3m-buffer nil)
  ("B" my/browser-activate-tab nil)

  ;; Switch buffer
  ;;
  ("a" ivy-switch-buffer nil)
  ;; ("a" bufler-switch-buffer nil)
  ;; ("RET" projectile-switch-to-buffer nil)
  ("A" my/switch-to-last-buffer nil)

  ;; Same as M-e
  ("8" my/counsel-switch-buffer-ediff nil)

  ;; Kill buffer
  ("k" my/auto-kill-buffer nil)

  ;; Move around in buffer
  ("C-u" evil-scroll-up nil)
  ("C-w" evil-scroll-down nil)

  ("," counsel-linux-app nil)

  ;; Switch window configuration
  ("t" my/load-window-config nil)
  ("T" my/add-window-config nil)
  ("C-t" my/delete-window-config nil)

  (";" counsel-bookmark nil)
  (":" my/add-bookmark nil)
  ("C-;" my/delete-bookmark nil)

  ;; Projectile
  ("i" counsel-projectile-switch-to-buffer nil)
  ("I" (lambda () (interactive) (require 'projectile) (projectile-switch-open-project)) nil)
  ("6" my/open-project nil)
  ("C-i" projectile-kill-buffers nil)

  ("u" winner-undo nil)
  ("C-r" winner-redo nil)

  ("R" rename-buffer nil)

  ("-" (lambda () (interactive) (text-scale-decrease 1)))
  ("=" (lambda () (interactive) (text-scale-increase 1)))

  ("+" (lambda () (interactive) (text-scale-mode 0)))
  ("_" (lambda () (interactive) (text-scale-mode 0)))

  ;; Add this to not auto exit insert mode after closing the hydra
  ("<escape>" nil)
  ("C-e" nil))

;; *** Keys
(my/evil-universal-define-key my/mod-window-leader-key 'my/window-hydra/body)
(my/evil-universal-define-key my/window-leader-key 'my/window-hydra/body)

;; * Window and buffer settings
;; ** Window settings
;; *** Make cursor auto move to new split window
(defun my/split-and-follow-horozontally ()
  (interactive)
  (split-window-below)
  ;;(balance-windows)
  (other-window 1))

(defun my/split-and-follow-vertically()
  (interactive)
  (split-window-right)
  ;;(balance-windows)
  (other-window 1))

;; *** Don't ask for confirmation when killing window
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))


;; *** Kill all buffers
(defun my/kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-M-s-k") 'my/kill-all-buffers)

;; *** Unique names for identical buffer names
(setq uniquify-buffer-name-style 'forward)
;; (setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; *** Custom buffer names - Auto rename buffers
;; Checkout the `relative-buffers' package
(defun my/custom-buffer-name ()
  ;; Make sure it runs after projectile is initialized
  (let ((path (or (buffer-file-name) dired-directory)))
    (when path
      (let ((name (my/custom-buffer-name-file path)))
	(when name
	  (rename-buffer
	   name
	   t))))))

(defun my/custom-buffer-name-file (path)
  (let ((project-name (projectile-project-name)))
    (if (string= project-name "-")
	(my/file-top-path path)
      (concat project-name " => " (my/file-top-path path)))))

(define-minor-mode my/custom-buffer-name-mode "")

(define-globalized-minor-mode global-my/custom-buffer-name-mode my/custom-buffer-name-mode my/custom-buffer-name)

(with-eval-after-load 'projectile
  (global-my/custom-buffer-name-mode))

;; * Dired
;; ** Disable cluttered major mode
;; Dired normally puts the sorting string in the major mode name, this disables that
(defun dired-sort-set-mode-line ())

;; ** Open current dir
(defun my/dired-curr-dir ()
  (interactive)
  (dired
   ;; If this file isn't temporary
   (if (buffer-file-name)
       ;; Default directory isn't updated when entering symlinks, this fixes that
       (file-name-directory (file-truename (buffer-file-name)))
     default-directory)))

;; ** Narrow
;; (straight-use-package 'dired-narrow)

;; ** Move file between windows
(setq dired-dwim-target t)

;; ** Change permissions in wdired
(setq wdired-allow-to-change-permissions 'advanced)

;; ** Put deleted files into trash folder
(setq delete-by-moving-to-trash t)

;; ** Dired async
;; *** Better async confirmation messages
(setq dired-async-message-function
      (lambda (text face &rest args)
	(let ((formatted-text (concat "Dired-async completed at: " (current-time-string) " with message: " (apply #'format text args) "\n")))
	  (message formatted-text)
	  (save-excursion
	    (switch-to-buffer "dired-async-messages")
	    (insert formatted-text)))))

;; ** rsync
(straight-use-package 'dired-rsync)

;; ** Dired atool
(straight-use-package 'dired-atool)

(dired-atool-setup)

;; ** Wdired
(define-prefix-command 'my/wdired-mode-map)
(evil-define-key 'normal wdired-mode-map (kbd (concat my/leader-map-key " a")) 'my/wdired-mode-map)

(define-key my/wdired-mode-map (kbd "s") 'wdired-finish-edit)
(define-key my/wdired-mode-map (kbd "k") 'wdired-abort-changes)

;; ** Dired collapse
;; (straight-use-package 'dired-collapse)

;; (add-hook 'dired-mode-hook 'dired-collapse-mode)

;; ** Subtree
;; (straight-use-package 'dired-subtree)

;; ** Date format
(setq my/dired-base-ls-command "-alh --time-style \"+%d-%m-%Y %H:%M\"")
(setq dired-listing-switches my/dired-base-ls-command)

;; ** Sorting
(defun my/dired-sort-menu ()
  (interactive)
  (dired-sort-other
   (pcase (completing-read "Sort by: " '( "date" "size" "name" "dir" "du" "~compatibility"))
     ("name" (progn (my/dired-du-disable-quietly) my/dired-base-ls-command))
     ("date" (progn (my/dired-du-disable-quietly) (concat my/dired-base-ls-command "-t")))
     ("size" (progn (my/dired-du-disable-quietly) (concat my/dired-base-ls-command "--sort=size")))
     ("dir" (progn (my/dired-du-disable-quietly) (concat my/dired-base-ls-command "--group-directories-first")))
     ("~compatibility" (progn (my/dired-du-disable-quietly) "-alh"))
     ("du" (progn (dired-sort-other "-alh") (dired-du-mode 1) "-alh")))))

(defun my/dired-du-disable-quietly ()
  (if dired-du-mode
      (dired-du-mode -1)))

(define-key dired-mode-map (kbd "s") 'my/dired-sort-menu)

;; *** Tramp compatibility
;; When dired is used over tramp sorting doesn't work
(defun my/dired-tramp-compatibility ()
  (when (file-remote-p default-directory)
    (dired-sort-other
     (progn (my/dired-du-disable-quietly) "-alh"))))

(add-hook 'dired-mode-hook 'my/dired-tramp-compatibility)

;; ** Recursive folder size
(straight-use-package 'dired-du)

(with-eval-after-load 'dired
  (require 'dired-du))

(setq dired-du-size-format t)

;; *** Disable on new buffer
(add-hook 'dired-mode-hook 'my/dired-du-disable-quietly)

;; ** Dired-single
;; (straight-use-package 'dired-single)

;; ** Dired omit-mode
;; This hides the this directory and previous directory folders in dired (. and ..)
(setq dired-omit-extensions nil)
(setq dired-omit-files "^\\.$\\|^\\.\\.$")

(with-eval-after-load 'dired
  (require 'dired-x))

(add-hook 'dired-mode-hook 'dired-omit-mode)

;; ** Keys
;; Reset dired mode map
(setq dired-mode-map (make-sparse-keymap))

(defun my/toggle-delete-to-trash ()
  (interactive)
  (if (eq delete-by-moving-to-trash nil)
      (progn
	(setq delete-by-moving-to-trash t)
	(message "Delete to trash enabled"))
    (progn
      (setq delete-by-moving-to-trash nil)
      (message "Delete to trash disabled"))))

(define-prefix-command 'my/dired-mode-map)
(evil-define-key 'normal dired-mode-map (kbd (concat my/leader-map-key " a")) 'my/dired-mode-map)

(define-key my/dired-mode-map (kbd "t") 'my/toggle-delete-to-trash)
(define-key my/dired-mode-map (kbd "w") 'dired-toggle-read-only)

(defun my/image-dired ()
  (interactive)
  (image-dired default-directory))

(define-key my/dired-mode-map (kbd "i") 'my/image-dired)
(define-key my/dired-mode-map (kbd "h") 'dired-hide-details-mode)

;; *** Dired close buffer and go down level
(defun my/dired-kill-and-go-down ()
  (interactive)
  (let ((dired-buffer (buffer-name)))
    (dired-up-directory)
    (kill-buffer dired-buffer)))

;; *** Dired keys
;; Bind =Backspace= to go up one directory
;; (define-key dired-mode-map [?\d] 'dired-up-directory)
(evil-define-key '(normal) dired-mode-map (kbd "k") 'dired-up-directory)

(evil-define-key 'insert dired-mode-map (kbd "A") 'dired-do-find-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "B") 'dired-do-byte-compile)

(evil-define-key '(normal insert) dired-mode-map (kbd "K") 'my/dired-kill-and-go-down)
(evil-define-key '(normal insert) dired-mode-map (kbd "RET") 'dired-find-file)
(evil-define-key '(normal insert) dired-mode-map(kbd "C") 'dired-do-copy)
(evil-define-key '(normal insert) dired-mode-map (kbd "D") 'dired-do-delete)
(evil-define-key '(normal insert) dired-mode-map  (kbd "j") 'dired-do-rename)
(evil-define-key '(normal insert) dired-mode-map  (kbd "J") 'dired-do-rename)
(evil-define-key '(normal insert) dired-mode-map  (kbd "!") 'dired-do-shell-command)
(evil-define-key '(normal insert) dired-mode-map  (kbd "&") 'dired-do-async-shell-command)
(evil-define-key '(normal insert) dired-mode-map  (kbd "Z") 'dired-atool-do-unpack-with-subdirectory)
(evil-define-key '(normal insert) dired-mode-map  (kbd "c") 'dired-atool-do-pack)

(evil-define-key 'insert dired-mode-map  (kbd "G") 'dired-do-chgrp)
(evil-define-key 'insert dired-mode-map  (kbd "H") 'dired-do-hardlink)
(evil-define-key 'insert dired-mode-map  (kbd "L") 'dired-do-load)
(evil-define-key 'insert dired-mode-map  (kbd "P") 'dired-do-print)
(evil-define-key 'insert dired-mode-map  (kbd "Q") 'dired-do-find-regexp-and-replace)
(evil-define-key 'insert dired-mode-map  (kbd "S") 'dired-do-symlink)
(evil-define-key 'insert dired-mode-map  (kbd "T") 'dired-do-touch)
;; Comparison commands
(evil-define-key '(normal insert) dired-mode-map (kbd "=") 'my/ediff-dired)

;; Make all regexp commands share a `%' prefix:
;; We used to get to the submap via a symbol dired-regexp-prefix,
;; but that seems to serve little purpose, and copy-keymap
;; does a better job without it.
(evil-define-key 'insert dired-mode-map (kbd "%") nil)
(evil-define-key 'insert dired-mode-map  (kbd "%u") 'dired-upcase)
(evil-define-key 'insert dired-mode-map  (kbd "%l") 'dired-downcase)
(evil-define-key 'insert dired-mode-map  (kbd "%d") 'dired-flag-files-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "%g") 'dired-mark-files-containing-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "%m") 'dired-mark-files-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "%r") 'dired-do-rename-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "%C") 'dired-do-copy-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "%H") 'dired-do-hardlink-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "%R") 'dired-do-rename-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "%S") 'dired-do-symlink-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "%&") 'dired-flag-garbage-files)
;; Commands for marking and unmarking.
(evil-define-key 'normal dired-mode-map (kbd "*") nil)
(evil-define-key 'insert dired-mode-map (kbd "*") nil)
(evil-define-key 'normal dired-mode-map (kbd "**") 'dired-mark-executables)
(evil-define-key 'normal dired-mode-map (kbd "*/") 'dired-mark-directories)
(evil-define-key 'normal dired-mode-map (kbd "*@") 'dired-mark-symlinks)
(evil-define-key 'normal dired-mode-map (kbd "*%") 'dired-mark-files-regexp)

(evil-define-key '(normal insert) dired-mode-map (kbd "*c") 'dired-change-marks)
(evil-define-key '(normal insert) dired-mode-map (kbd "*s") 'dired-mark-subdir-files)
(evil-define-key '(normal insert) dired-mode-map (kbd "*m") 'dired-mark)
(evil-define-key '(normal insert) dired-mode-map (kbd "*u") 'dired-unmark)
(evil-define-key '(normal insert) dired-mode-map (kbd "*?") 'dired-unmark-all-files)
(evil-define-key '(normal insert) dired-mode-map (kbd "*!") 'dired-unmark-all-marks)
(evil-define-key '(normal insert) dired-mode-map (kbd "U") 'dired-unmark-all-marks)
(evil-define-key '(normal insert) dired-mode-map (kbd "*\177") 'dired-unmark-backward)
(evil-define-key '(normal insert) dired-mode-map (kbd "*\C-n") 'dired-next-marked-file)
(evil-define-key '(normal insert) dired-mode-map (kbd "*\C-p") 'dired-prev-marked-file)
(evil-define-key '(normal insert) dired-mode-map (kbd "*t") 'dired-toggle-marks)
;; Lower keys for commands not operating on all the marked files
(evil-define-key '(normal insert) dired-mode-map (kbd "d") 'dired-flag-file-deletion)
(evil-define-key 'insert dired-mode-map (kbd "e") 'counsel-find-file)
;; (put 'dired-find-file :advertised-binding (kbd "\C-m"))
(evil-define-key 'insert dired-mode-map (kbd "g") 'revert-buffer)
;; (evil-define-key 'insert dired-mode-map (kbd "RET") 'dired-subtree-insert)
;; (evil-define-key 'insert dired-mode-map (kbd "i") 'dired-subtree-insert)
;; (evil-define-key 'insert dired-mode-map (kbd "k") 'dired-subtree-remove)
(evil-define-key 'insert dired-mode-map (kbd "RET") 'dired-maybe-insert-subdir)
(evil-define-key 'insert dired-mode-map (kbd "i") 'dired-maybe-insert-subdir)
(evil-define-key 'insert dired-mode-map (kbd "k") 'dired-kill-subdir)

(evil-define-key 'insert dired-mode-map (kbd "l") 'dired-do-redisplay)
(evil-define-key 'normal dired-mode-map (kbd "M-m") 'dired-mark-subdir-files)
(evil-define-key '(normal insert) dired-mode-map (kbd "m") 'dired-mark)
(evil-define-key '(normal insert) dired-mode-map (kbd "M") 'dired-toggle-marks)
(evil-define-key 'insert dired-mode-map (kbd "n") 'dired-next-line)
(evil-define-key '(normal insert) dired-mode-map (kbd "o") 'dired-find-file-other-window)
(evil-define-key '(normal insert) dired-mode-map (kbd "O") 'dired-insert-subdir)
(evil-define-key 'insert dired-mode-map (kbd "p") 'dired-previous-line)
(evil-define-key 'insert dired-mode-map (kbd "s") 'my/dired-sort-menu)
(evil-define-key 'insert dired-mode-map (kbd "t") 'dired-toggle-marks)
(evil-define-key '(normal insert) dired-mode-map (kbd "u") 'dired-unmark)
(evil-define-key 'insert dired-mode-map (kbd "v") 'dired-view-file)

(evil-define-key 'insert dired-mode-map (kbd "y") 'dired-copy-filename-as-kill)
(evil-define-key 'insert dired-mode-map (kbd "w") 'browse-url-of-dired-file)
(evil-define-key '(normal insert) dired-mode-map (kbd "x") 'dired-do-flagged-delete)

(evil-define-key '(normal insert) dired-mode-map (kbd "?") 'dired-show-file-type)
(evil-define-key '(normal insert) dired-mode-map (kbd "+") 'dired-create-directory)
;; moving
(evil-define-key '(normal insert) dired-mode-map (kbd "<") 'dired-prev-dirline)
(evil-define-key '(normal insert) dired-mode-map (kbd ">") 'dired-next-dirline)
(evil-define-key '(normal insert) dired-mode-map (kbd "^") 'dired-up-directory)
;; folding
(evil-define-key 'normal dired-mode-map (kbd "g") nil)
(define-key dired-mode-map  [remap evil-close-fold] 'dired-hide-subdir)
(define-key dired-mode-map  [remap evil-open-fold] 'dired-unhide-subdir)
(define-key dired-mode-map  [remap my/outline-hide-all-body] 'dired-hide-all)
(define-key dired-mode-map  [remap outline-show-all] 'dired-hide-all)
;; misc
;; (evil-define-key 'insert dired-mode-map [remap read-only-mode] 'dired-toggle-read-only)
;; ;; `toggle-read-only' is an obsolete alias for `read-only-mode'
;; (evil-define-key 'insert dired-mode-map [remap toggle-read-only] 'dired-toggle-read-only)
;; (evil-define-key 'insert dired-mode-map  (kbd "\177") 'dired-unmark-backward)

;; thumbnail manipulation (image-dired)
(evil-define-key 'insert dired-mode-map (kbd "\C-td") 'image-dired-display-thumbs)
(evil-define-key 'insert dired-mode-map (kbd "\C-tt") 'image-dired-tag-files)
(evil-define-key 'insert dired-mode-map (kbd "\C-tr") 'image-dired-delete-tag)
(evil-define-key 'insert dired-mode-map (kbd "\C-tj") 'image-dired-jump-thumbnail-buffer)
(evil-define-key 'insert dired-mode-map (kbd "\C-ti") 'image-dired-dired-display-image)
(evil-define-key 'insert dired-mode-map (kbd "\C-tx") 'image-dired-dired-display-external)
(evil-define-key 'insert dired-mode-map (kbd "\C-ta") 'image-dired-display-thumbs-append)
(evil-define-key 'insert dired-mode-map (kbd "\C-t.") 'image-dired-display-thumb)
(evil-define-key 'insert dired-mode-map (kbd "\C-tc") 'image-dired-dired-comment-files)
(evil-define-key 'insert dired-mode-map (kbd "\C-tf") 'image-dired-mark-tagged-files)
(evil-define-key 'insert dired-mode-map (kbd "\C-t\C-t") 'image-dired-dired-toggle-marked-thumbs)
(evil-define-key 'insert dired-mode-map (kbd "\C-te") 'image-dired-dired-edit-comment-and-tags)
;; encryption and decryption (epa-dired)
;; (evil-define-key 'insert (kbd ":d") 'epa-dired-do-decrypt)
;; (evil-define-key 'insert  (kbd ":v") 'epa-dired-do-verify)
;; (evil-define-key 'insert  (kbd ":s") 'epa-dired-do-sign)
;; (evil-define-key 'insert  (kbd ":e") 'epa-dired-do-encrypt)

;; * Gud
(straight-use-package 'realgud)
(setq gdb-many-windows 'nil)

;; * Eldoc
;; Shows information in echo area
;; Needed??
;; (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(setq eldoc-echo-area-use-multiline-p t)
(setq-default eldoc-echo-area-use-multiline-p t)
(setq my/eldoc-idle-delay 0)
(setq eldoc-idle-delay my/eldoc-idle-delay)

(setq tooltip-resize-echo-area t)

;; Update function to only show documentation when not in insert mode
;; (el-patch-feature eldoc)
;; (el-patch-defun eldoc-print-current-symbol-info ()
;; "Print the text produced by `eldoc-documentation-function'."
;;  This is run from post-command-hook or some idle timer thing,
;;  so we need to be careful that errors aren't ignored.
;; (if (not (eq evil-state 'insert))
;; (with-demoted-errors "eldoc error: %s"
;; (and (or (eldoc-display-message-p)
;;  Erase the last message if we won't display a new one.
;; (when eldoc-last-message
;; (eldoc-message nil)
;; nil))
;; (eldoc-message (funcall eldoc-documentation-function))))))

;; ** Box
;; (straight-use-package 'eldoc-box)

;; (add-hook 'eldoc-mode-hook 'eldoc-box-hover-mode)

;; eldoc-box-hover-at-point-mode needs to be enabled after eldoc-box-hover-mode is done, otherwise problems can appear
;; (defadvice eldoc-box-hover-mode (after eldoc-box-hover-mode activate) (eldoc-box-hover-at-point-mode 1))

;; *** Force remove header
;; (defun eldoc-box--display (str)
;;   "Display STR in childframe."
;;   (unless (equal str "") ; WORKAROUND lsp returns empty string from time to time
;;     (let ((doc-buffer (get-buffer-create eldoc-box--buffer)))
;;       (with-current-buffer doc-buffer
;;	(setq mode-line-format nil)
;;	(setq header-line-format nil)
;;	;; without this, clicking childframe will make doc buffer the current buffer
;;	;; and `eldoc-box--maybe-cleanup' in `eldoc-box--cleanup-timer' will clear the childframe
;;	(setq eldoc-box-hover-mode t)
;;	(erase-buffer)
;;	(insert str)
;;	(goto-char (point-min)))
;;       (eldoc-box--get-frame doc-buffer))))

;; ** Inline
;; (straight-use-package 'eldoc-overlay)

;; (global-eldoc-overlay-mode)

;; * Code
;; ** Generic
;; *** Xref
;; By default xref always prompts with certain commands
(setq xref-prompt-for-identifier nil)

;; **** Ivy-xref
(straight-use-package 'ivy-xref)

(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)

;; *** Smartparens
;; (straight-use-package 'smartparens)

;; (smartparens-global-mode)

;; *** Quick-peek
(straight-use-package 'quick-peek)

(setq quick-peek-spacer nil)

;; *** Aggressive indent
(straight-use-package '(aggressive-indent :type git :host github :repo "walseb/aggressive-indent-mode"))

(global-aggressive-indent-mode)
(add-to-list 'aggressive-indent-excluded-modes 'plantuml-mode)
(add-to-list 'aggressive-indent-excluded-modes 'java-mode)
(add-to-list 'aggressive-indent-excluded-modes 'c-mode)
(add-to-list 'aggressive-indent-excluded-modes 'fsharp-mode)
(add-to-list 'aggressive-indent-excluded-modes 'haskell-cabal-mode)
(add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)

;; *** Whitespace cleanup
(straight-use-package 'whitespace-cleanup-mode)

(global-whitespace-cleanup-mode)

(add-to-list 'whitespace-cleanup-mode-ignore-modes 'image-mode)
(add-to-list 'whitespace-cleanup-mode-ignore-modes 'dired-mode)

;; TODO: Keep here while image-mode bug exists
(add-to-list 'whitespace-cleanup-mode-ignore-modes 'fundamental-mode)

;; *** indent guide
;; (straight-use-package 'highlight-indent-guides)

;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; (add-hook 'prog-mode-hook (lambda () (highlight-indent-guides-mode (my/highlight-indent-guide-should-enable))))

;; ;;(defun my/highlight-indent-guide-should-enable ()
;; ;;  (pcase major-mode
;; ;;    ('emacs-lisp-mode -1)
;; ;;    ('lisp-interaction-mode -1)
;; ;;    ('clojure-mode -1)
;; ;;    ('scheme-mode -1)
;; ;;    ('common-lisp-mode -1)
;; ;;    (_ 1)))

;; (setq highlight-indent-guides-method 'column)
;; ;; (setq highlight-indent-guides-method 'fill)

;; (setq highlight-indent-guides-responsive 'top)
;; (setq highlight-indent-guides-delay 0)

;; *** Auto fix suggested
(defun my/auto-fix-suggested ()
  (interactive)
  (pcase major-mode
    ('haskell-mode
     (if my/haskell-hie-enable
	 (call-interactively 'lsp-execute-code-action)
       (call-interactively 'attrap-attrap)))
    (_
     (if lsp-mode
	 (call-interactively 'lsp-execute-code-action)
       (call-interactively 'attrap-attrap)))))

(define-key my/leader-map (kbd "RET") 'my/auto-fix-suggested)

;; *** Auto jump to definition
(straight-use-package 'dumb-jump)

(defun my/auto-jump-to-definition ()
  (interactive)
  (pcase major-mode
    ('fsharp-mode (fsharp-ac/gotodefn-at-point))
    ('clojure-mode (clojure-find-def))
    ('emacs-lisp-mode (call-interactively 'xref-find-definitions))
    ('c-mode (call-interactively 'xref-find-definitions))
    ('c++-mode (call-interactively 'xref-find-definitions))
    ('objc-mode (call-interactively 'xref-find-definitions))
    ('csharp-mode (omnisharp-go-to-definition))
    ('haskell-mode
     (if my/haskell-hie-enable
	 (lsp-find-definition)
       (call-interactively 'xref-find-definitions)))
    (_
     (if lsp-mode
	 (lsp-find-definition)
       (dumb-jump-go)))))

(define-key my/leader-map (kbd "t") 'my/auto-jump-to-definition)

(setq dumb-jump-selector 'ivy)

;; *** Auto find usages
(defun my/auto-find-usages ()
  (interactive)
  (pcase major-mode
    ('csharp-mode (omnisharp-find-usages-with-ido))
    (_ (call-interactively 'xref-find-references))))

(define-key my/leader-map (kbd "u") 'my/auto-find-usages)

;; *** Auto eval
(defun my/auto-eval ()
  (interactive)
  (if (string= evil-state 'visual)
      (my/auto-eval-region)
    (pcase major-mode
      ;; Silent result
      ;; ('org-mode (org-babel-execute-src-block nil nil '((:result-params . ("none")))))
      ;; ('org-mode (org-babel-execute-src-block nil nil '((:result-params . ("silent")))))
      ;; ('org-mode (org-babel-execute-src-block))
      ;; Without pretty print it formats results of "[[[ ... ]]]" as "| ... |"
      ('org-mode (org-babel-execute-src-block nil nil '((:result-params . ("pp")))))
      ('scheme-mode (geiser-eval-definition nil))
      ('clojure-mode (cider-eval-last-sexp))
      ;; ('racket-mode (geiser-eval-definition nil))
      ;; ('racket-mode (racket-eval-last-sexp))
      ('racket-mode (my/racket-send-last-sexp))
      ('plantuml-mode (plantuml-preview-region 0 (line-beginning-position) (line-end-position)))
      ('fsharp-mode (fsharp-eval-phrase))
      ('c-mode (cling-send-region (line-beginning-position) (line-end-position)))
      ('c++-mode (cling-send-region (line-beginning-position) (line-end-position)))
      ('csharp-mode (my/csharp-run-repl))
      ('haskell-mode (save-excursion (my/auto-eval-region (progn (beginning-of-line) (point)) (progn (end-of-line) (point)))))
      (_ (call-interactively 'eros-eval-last-sexp)))))

(defun my/auto-eval-region (_beg _end)
  (interactive)
  (let* ((beg (min _beg _end))
	 (end (max _beg _end)))
    (pcase major-mode
      ('clojure-mode (cider-eval-region beg end))
      ('plantuml-mode (plantuml-preview-region 0 beg end))
      ('fsharp-mode (fsharp-eval-region beg end))
      ('c-mode (cling-send-region beg end))
      ('c++-mode (cling-send-region beg end))
      ('csharp-mode (my/csharp-run-repl))
      ('racket-mode (racket--send-region-to-repl beg end))
      ;; ('haskell-mode (save-selected-window (my/haskell-interactive-copy-string-to-prompt (buffer-substring-no-properties beg end)) (haskell-interactive-bring) (goto-char (point-max)) (re-search-backward (haskell-interactive-prompt-regex)) (end-of-line) (recenter) ))
      ('haskell-mode (save-excursion (my/haskell-interactive-mode-run-expr
				      ;; Since indentation is syntax in haskell, make sure the selection begins at the beginning of the line, and doesn't exclude some white space
				      (progn
					(let ((beg-fixed nil))
					  (when (/= (line-number-at-pos beg) (line-number-at-pos end))
					    (save-excursion
					      (goto-char beg)

					      ;; If the search failed
					      (unless (re-search-backward "[[:graph:]]" (line-beginning-position) t)
						(setq beg-fixed (line-beginning-position)))))
					  (buffer-substring-no-properties (or beg-fixed beg) end))))))

      ;; (haskell-interactive-mode-prompt-previous)
      (_
       ;; eval-region doesn't return anything, just prints to the minibuffer so eros can't be used here
       (eros--eval-overlay
	(eval-region beg end t)
	end)))))

(defun my/auto-eval-buffer ()
  (interactive)
  (pcase major-mode
    ('scheme-mode (geiser-eval-buffer nil))
    ('clojure-mode (cider-eval-buffer))
    ('plantuml-mode (plantuml-preview-buffer 0))
    ('fsharp-mode (fsharp-eval-region (point-min) (point-max)))
    ('c-mode (cling-send-buffer))
    ('c++-mode (cling-send-buffer))
    ('csharp-mode (my/csharp-run-repl))
    ('racket-mode (racket--send-region-to-repl (point-min) (point-max)))
    ('haskell-mode (progn (haskell-process-load-file) (haskell-interactive-bring) (end-of-buffer) (recenter)))
    (_ (when (not (string= (file-name-nondirectory buffer-file-name) "config.el"))
	 (eval-buffer nil)))))

(defun my/auto-eval-buffer-run ()
  (interactive)
  (pcase major-mode
    ('haskell-mode (progn
		     (haskell-process-load-specific-file (my/projectile-find-file "Main.hs"))
		     (my/haskell-interactive-mode-run-expr "main")))
    (_ (message "Mode not supported"))))

(defun my/auto-eval-print ()
  (interactive)
  (pcase major-mode
    ('org-mode (call-interactively #'org-babel-execute-src-block))
    ('scheme-mode (geiser-eval-last-sexp t))
    ('clojure-mode (cider-eval-print-last-sexp))
    ('csharp-mode (haskell-interactive-copy-to-prompt))
    (_ (eval-print-last-sexp nil))))

;; (define-key my/leader-map (kbd "e") 'my/auto-eval)
(define-key my/leader-map (kbd "e") 'my/auto-eval-buffer)
(define-key my/leader-map (kbd "E") 'my/auto-eval-buffer-run)
;; (define-key my/leader-map (kbd "M-e") 'my/auto-eval-print)

;; *** Auto debug
(defun my/auto-debug ()
  (interactive)
  ;;(load-library "realgud")
  (if (eq evil-state 'visual)
      (my/auto-debug-region)
    (pcase major-mode
      ('emacs-lisp-mode (progn (require 'edebug) (call-interactively #'edebug-set-breakpoint)))
      ('c-mode (call-interactively #'gud-break))
      ('c++-mode (call-interactively #'gud-break))
      (_ (eval-last-sexp nil)))))

(defun my/auto-remove-debug ()
  (interactive)
  ;;(load-library "realgud")
  (if (eq evil-state 'visual)
      (my/auto-debug-region)
    (pcase major-mode
      ('emacs-lisp-mode (call-interactively #'edebug-unset-breakpoint))
      ('c-mode (call-interactively #'gud-remove))
      ('c++-mode (call-interactively #'gud-remove))
      (_ (eval-last-sexp nil)))))

;; (defun my/auto-debug-region ()
;; (interactive)
;; )

(defun my/start-gdb()
  (split-window-below)
  (call-interactively #'gdb))

(defun my/auto-start-debugger ()
  (interactive)
  ;;(load-library "realgud")
  (pcase major-mode
    ('c-mode (my/start-gdb))
    ('c++-mode (my/start-gdb))
    (_ (eval-last-sexp nil))))

(define-key my/leader-map (kbd "C-D") 'my/auto-debug)
;; (define-key my/leader-map (kbd "C-D") 'my/auto-remove-debug)
;; (define-key my/leader-map (kbd "D") 'my/auto-debug-buffer)
;; (define-key my/leader-map (kbd "M-D") 'my/auto-start-debugger)

;; *** Auto docs
(defun my/auto-docs ()
  (interactive)
  (pcase major-mode
    ('haskell-mode
     ;; (let ((browse-url-browser-function 'eww-browse-url))
     (my/ivy-hoogle))
    ('haskell-interactive-mode (my/ivy-hoogle))
    ('nix-mode
     (if (file-in-directory-p (buffer-file-name) "~/.config/nixpkgs/")
	 (man "home-configuration.nix")
       (my/nixos-options-ivy)))))

(define-key my/leader-map (kbd "h") help-map)
(define-key my/leader-map (kbd "H") 'my/auto-docs)

;; ** Documentation
;; *** Compact-docstrings
;; (straight-use-package 'compact-docstrings)

;; *** Zeal
(straight-use-package 'zeal-at-point)

(define-key my/leader-map (kbd "T") 'zeal-at-point)

;; ** Plantuml
(straight-use-package 'plantuml-mode)

;; Org src compatibility
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

(add-hook 'plantuml-mode-hook 'my/plantuml-mode)

;; ** LSP
(straight-use-package 'lsp-mode)

;; Normally lsp-mode starts up flymake mode automaticall which breaks ccls
(setq lsp-prefer-flymake nil)

(setq lsp-document-highlight-delay nil)
(setq lsp-enable-symbol-highlighting nil)

;; *** Company LSP
(straight-use-package 'company-lsp)
(push 'company-lsp company-backends)

;; Increases performance
(setq company-lsp-cache-candidates 'auto)

;; *** LSP-ui
(straight-use-package 'lsp-ui)
;; TODO I have to load the package fully here to set the fonts later

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(setq lsp-ui-doc-enable nil
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t)
;; (setq lsp-ui-sideline-ignore-duplicate t)

(setq lsp-ui-sideline-show-code-actions nil
      ;; Errors i think
      lsp-ui-sideline-show-diagnostics nil

      ;; someFunc :: IO ()
      lsp-ui-sideline-show-hover t

      ;; [someFunc]
      lsp-ui-sideline-show-symbol t)

;; (setq lsp-ui-sideline-delay 0)

;; *** Flycheck keys
(with-eval-after-load 'lsp-ui-flycheck
  (define-key lsp-ui-flycheck-list-mode-map [remap evil-ret] 'lsp-ui-flycheck-list--view)
  (define-key lsp-ui-flycheck-list-mode-map [remap newline] 'lsp-ui-flycheck-list--view))

;; ** DAP
;; I have to clear the mode map before it's defined otherwise I can't unbind it
(setq dap-mode-map (make-sparse-keymap))

(straight-use-package 'dap-mode)
(dap-mode 1)

(with-eval-after-load 'dap-mode
  (dap-ui-mode 1))

;; ** Elgot
;; (straight-use-package 'eglot)

;; ** Lisps
;; *** Common lisp
;; **** Slime
;; (straight-use-package 'slime)

;; (setq inferior-lisp-program "/usr/bin/sbcl")
;; (setq slime-contribs '(slime-fancy))

;; ***** Slime comany
;; (straight-use-package 'slime-company)

;; (slime-setup '(slime-fancy slime-company))

;; **** Keys
(define-prefix-command 'my/common-lisp-mode-map)
(evil-define-key 'normal lisp-mode-map (kbd (concat my/leader-map-key " a")) 'my/common-lisp-mode-map)

(define-key my/common-lisp-mode-map (kbd "d") 'slime-inspect-definition)

;; *** Scheme
;; (straight-use-package 'geiser)

;; (define-prefix-command 'my/scheme-mode-map)
;; (evil-define-key 'normal scheme-mode-map (kbd (concat my/leader-map-key " a")) 'my/scheme-mode-map)

;; (define-key my/scheme-mode-map (kbd "s") 'geiser-set-scheme)
;; (define-key my/scheme-mode-map (kbd "C-s") 'run-geiser)

;; *** Racket
(straight-use-package 'racket-mode)
(add-hook 'racket-mode-hook (lambda () (setq-local flycheck-check-syntax-automatically '(save))))

(defun my/racket-send-last-sexp ()
  (save-excursion
    (goto-char (+ 1 (point)))
    (racket-send-last-sexp)))

;; **** Fix eval
;; (with-eval-after-load 'racket-mode
;;   (defun racket-eval-last-sexp ()
;;     "Eval the previous sexp asynchronously and `message' the result."
;;     (interactive)
;;     (racket--cmd/async
;;      `(eval
;;        ,(buffer-substring-no-properties (racket--repl-last-sexp-start)
;;					(+ 1(point))))
;;      (lambda (v)
;;        (message "%s" v))))

;;   (defun racket--repl-last-sexp-start ()
;;     (save-excursion
;;       (condition-case ()
;;	  (progn
;;	    (my/backward-sexp)
;;	    (if (save-match-data (looking-at "#;"))
;;		(+ (point) 2)
;;	      (point)))
;;	(scan-error (user-error "There isn't a complete s-expression before point"))))))

;; **** Keys
(setq racket-repl-mode-map (make-sparse-keymap))

;; *** Emacs-lisp
;; **** Eros
(straight-use-package 'eros)

(setq eros-eval-result-duration 'command)
(eros-mode 1)

;; ***** Make it work inside run with timer and such
;; Eros doesn't work inside run-with-timers because then ~this-command~ is set. In this patch I just remove the ~this-command~ check
(cl-defun eros--make-result-overlay (value &rest props &key where duration (type 'result)
					   (format (concat " " eros-eval-result-prefix "%s "))
					   (prepend-face 'eros-result-overlay-face)
					   &allow-other-keys)
  "Place an overlay displaying VALUE at the end of line.

VALUE is used as the overlay's after-string property, meaning it
is displayed at the end of the overlay.  The overlay itself is
placed from beginning to end of current line.

Return nil if the overlay was not placed or if it might not be
visible, and return the overlay otherwise.

Return the overlay if it was placed successfully, and nil if it
failed.

This function takes some optional keyword arguments:

- If WHERE is a number or a marker, apply the overlay over the
  entire line at that place (defaulting to `point').  If it is a
  cons cell, the car and cdr determine the start and end of the
  overlay.

- DURATION takes the same possible values as the
  `eros-eval-result-duration' variable.

- TYPE is passed to `eros--make-overlay' (defaults to `result').

- FORMAT is a string passed to `format'.  It should have exactly
  one %s construct (for VALUE).

All arguments beyond these (PROPS) are properties to be used on
the overlay."
  (declare (indent 1))
  (while (keywordp (car props))
    (setq props (cddr props)))
  ;; If the marker points to a dead buffer, don't do anything.
  (let ((buffer (cond
		 ((markerp where) (marker-buffer where))
		 ((markerp (car-safe where)) (marker-buffer (car where)))
		 (t (current-buffer)))))
    (with-current-buffer buffer
      (save-excursion
	(when (number-or-marker-p where)
	  (goto-char where))
	;; Make sure the overlay is actually at the end of the sexp.
	(skip-chars-backward "\r\n[:blank:]")
	(let* ((beg (if (consp where)
			(car where)
		      (save-excursion
			(backward-sexp 1)
			(point))))
	       (end (if (consp where)
			(cdr where)
		      (line-end-position)))
	       (display-string (format format value))
	       (o nil))
	  (remove-overlays beg end 'category type)
	  (funcall (if eros-overlays-use-font-lock
		       #'font-lock-prepend-text-property
		     #'put-text-property)
		   0 (length display-string)
		   'face prepend-face
		   display-string)
	  ;; If the display spans multiple lines or is very long, display it at
	  ;; the beginning of the next line.
	  (when (or (string-match "\n." display-string)
		    (> (string-width display-string)
		       (- (window-width) (current-column))))
	    (setq display-string (concat " \n" display-string)))
	  ;; Put the cursor property only once we're done manipulating the
	  ;; string, since we want it to be at the first char.
	  (put-text-property 0 1 'cursor 0 display-string)
	  (when (> (string-width display-string) (* 3 (window-width)))
	    (setq display-string
		  (concat (substring display-string 0 (* 3 (window-width)))
			  "...\nResult truncated.")))
	  ;; Create the result overlay.
	  (setq o (apply #'eros--make-overlay
			 beg end type
			 'after-string display-string
			 props))
	  (pcase duration
	    ((pred numberp) (run-at-time duration nil #'eros--delete-overlay o))
	    (`command
	     ;; (if this-command
	     (add-hook 'pre-command-hook
		       #'eros--remove-result-overlay
		       nil 'local)
	     ;; (eros--remove-result-overlay))
	     ))
	  (let ((win (get-buffer-window buffer)))
	    ;; Left edge is visible.
	    (when (and win
		       (<= (window-start win) (point))
		       ;; In 24.3 `<=' is still a binary predicate.
		       (<= (point) (window-end win))
		       ;; Right edge is visible. This is a little conservative
		       ;; if the overlay contains line breaks.
		       (or (< (+ (current-column) (string-width value))
			      (window-width win))
			   (not truncate-lines)))
	      o)))))))

;; **** Litable
;; ;;(straight-use-package '(litable :type git :host github :repo "Fuco1/blablabla"))
;; (straight-use-package 'litable)

;; ;; Eval everything
;; (defun litable--safe-eval (form)
;;   (eval form))

;; ;; Make it only eval the parens around cursor
;; (defun litable-update-defs (&optional a b c)
;;   (litable-remove-overlays)
;;   (when a
;;     (ignore-errors
;;       (let ((form (save-excursion
;;                     (backward-up-list)
;;                     (sexp-at-point))))
;;         (litable-find-function-subs-arguments form)))))

;; ***** Don't safe check
;; (defun litable--safe-eval (form)
;; (eval form))

;; **** Enable debugging on error
(setq debug-on-error nil)

;; **** Debugging

;; **** Suggest
(straight-use-package 'suggest)

;; **** Formatting
(straight-use-package 'elisp-format)

;; **** Keys
(define-prefix-command 'my/emacs-lisp-mode-map)
(evil-define-key 'normal emacs-lisp-mode-map (kbd (concat my/leader-map-key " a")) 'my/emacs-lisp-mode-map)

(define-key my/emacs-lisp-mode-map (kbd "d") 'find-function)
(define-key my/emacs-lisp-mode-map (kbd "D") 'find-variable)

;; (define-key my/emacs-lisp-mode-map (kbd "c") 'emacs-lisp-byte-compile)

(define-key my/emacs-lisp-mode-map (kbd "s") 'suggest)

(define-prefix-command 'my/emacs-lisp-formatting-map)
(define-key my/emacs-lisp-mode-map (kbd "f") 'my/emacs-lisp-formatting-map)

(define-key my/emacs-lisp-formatting-map (kbd "b") 'elisp-format-buffer)
(define-key my/emacs-lisp-formatting-map (kbd "r") 'elisp-format-region)
(define-key my/emacs-lisp-formatting-map (kbd "f") 'elisp-format-file)
(define-key my/emacs-lisp-formatting-map (kbd "C-d") 'elisp-format-directory)
;; Format marked files in dired
;; elisp-format-dired-mark-files


;; control
(define-key my/emacs-lisp-mode-map "n" 'edebug-step-mode)
;; (define-key my/emacs-lisp-mode-map "n" 'edebug-next-mode)

(define-key my/emacs-lisp-mode-map (kbd "s") 'edebug-go-mode)
(define-key my/emacs-lisp-mode-map "S" 'edebug-Go-nonstop-mode)
(define-key my/emacs-lisp-mode-map "t" 'edebug-trace-mode)
(define-key my/emacs-lisp-mode-map "T" 'edebug-Trace-fast-mode)

(define-key my/emacs-lisp-mode-map "g" 'edebug-goto-here)

(define-key my/emacs-lisp-mode-map "i" 'edebug-step-in)
(define-key my/emacs-lisp-mode-map "o" 'edebug-step-out)

;; *** Clojure
(straight-use-package 'clojure-mode)

;; **** Cider
(straight-use-package 'cider)

;; ***** Enlighten
(add-hook 'clojure-mode-hook 'cider-enlighten-mode)

;; **** Keys
(define-prefix-command 'my/clojure-mode-map)
(evil-define-key 'normal clojure-mode-map (kbd (concat my/leader-map-key " a")) 'my/clojure-mode-map)

(define-key my/clojure-mode-map (kbd "C-s") 'cider-connect)

;; ** Java
(straight-use-package 'lsp-java)

(defun my/java-mode ()
  ;; Add configurations for java dap-mode
  (require 'dap-java)

  ;; (require 'lsp-java-boot)

  (lsp)
  (lsp-lens-mode)
  ;; (lsp-java-boot-lens-mode)

  (lsp-ui-sideline-mode 1))

;; Enable java lens
(setq lsp-java-references-code-lens-enabled t)
(setq lsp-java-implementations-code-lens-enabled t)

(add-hook 'java-mode-hook 'my/java-mode)

;; *** Keys
(define-prefix-command 'my/java-mode-map)
(evil-define-key 'normal java-mode-map (kbd (concat my/leader-map-key " a")) 'my/java-mode-map)

(define-key my/java-mode-map (kbd "r") 'lsp-rename)
(define-key my/java-mode-map (kbd "C-r") 'lsp-workspace-restart)
(define-key my/java-mode-map (kbd "f") 'lsp-format-buffer)
(define-key my/java-mode-map (kbd "i") 'lsp-java-organize-imports)
(define-key my/java-mode-map (kbd "C-b") 'lsp-java-build-project)

(define-key my/java-mode-map (kbd "f") 'xref-find-references)
(define-key my/java-mode-map (kbd "a") 'xref-find-apropos)

(define-prefix-command 'my/java-refractor-map)
(define-key my/java-mode-map (kbd "R") 'my/java-refractor-map)

(define-key my/java-refractor-map (kbd "c") 'lsp-java-extract-to-constant)
(define-key my/java-refractor-map (kbd "u") 'lsp-java-add-unimplemented-methods)
(define-key my/java-refractor-map (kbd "p") 'lsp-java-create-parameter)
(define-key my/java-refractor-map (kbd "f") 'lsp-java-create-field)
(define-key my/java-refractor-map (kbd "l") 'lsp-java-create-local)
(define-key my/java-refractor-map (kbd "m") 'lsp-java-extract-method)
(define-key my/java-refractor-map (kbd "i") 'lsp-java-add-import)

;; ** Python
;; *** Jedi
(straight-use-package 'company-jedi)

(add-to-list 'company-backends 'company-jedi)

;; ** Haskell
;; (straight-use-package '(haskell-mode :type git :host github :repo "walseb/haskell-mode"))
(straight-use-package 'haskell-mode)

(with-eval-after-load 'haskell-mode
  (define-prefix-command 'my/haskell-mode-map)
  (evil-define-key 'normal haskell-mode-map (kbd (concat my/leader-map-key " a")) 'my/haskell-mode-map)
  (evil-define-key '(insert visual replace) haskell-mode-map (kbd (concat my/mod-leader-map-key " a")) 'my/haskell-mode-map)
  ;; Also enable the mode map in ghci mode
  (evil-define-key 'normal haskell-interactive-mode-map (kbd (concat my/leader-map-key " a")) 'my/haskell-mode-map)
  (evil-define-key '(insert visual replace) haskell-interactive-mode-map (kbd (concat my/mod-leader-map-key " a")) 'my/haskell-mode-map))

(defun my/haskell-mode ()
  (interactive)
  (setq-local evil-shift-width 2))

(add-hook 'haskell-mode-hook 'my/haskell-mode)

(setq haskell-process-auto-import-loaded-modules t)
;; Disable ghci error overlay
(setq haskell-process-show-overlays nil)
(setq haskell-process-suggest-overloaded-strings nil)

;; Disable startup message
(setq haskell-process-show-debug-tips nil)

(setq haskell-interactive-mode-read-only nil)
(setq haskell-interactive-prompt-read-only nil)
(setq haskell-interactive-popup-errors nil)

;; *** Disable haskell syntax propertize
;; This function causes emacs to hang at times because of some reason
(with-eval-after-load 'haskell-mode
  (defun haskell-syntax-propertize (a b) nil))

;; *** Custom syntax highlighting
(with-eval-after-load 'haskell-mode
  (define-derived-mode haskell-mode prog-mode "my/haskell-syntax-mode"
    "Syntax highlighting for haskell"
    (setq font-lock-defaults '(my/haskell-syntax-mode-font-lock))
    (setq-local comment-start "--")
    (setq-local comment-padding 1)
    (setq-local comment-end "")

    ;; Language extensions. Since these has to work inside comments, the `t' at the end is needed. I don't know if that's possible to do inside `my/haskell-syntax-mode-font-lock' though
    (font-lock-add-keywords nil `((,(rx "LANGUAGE" space (+ graph)) 0 font-lock-function-name-face t)))

    (setq-local imenu-create-index-function 'haskell-ds-create-imenu-index)

    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 8)

    (haskell-indentation-mode)))

;; **** Keywords
(setq my/haskell-syntax-mode-keywords '("module" "import" "qualified"
					;; These are already highlighted since they are treated as functions by the haskell font-lock
					;; "type" "newtype" "data"
					"do" "proc" "rec"
					"let"  "where"
					"if" "then" "else" "in"))

;; **** Font lock
(setq my/haskell-syntax-mode-font-lock
      `(
	;; Keywords
	(,(regexp-opt my/haskell-syntax-mode-keywords 'words) . font-lock-keyword-face)

	;; Functions
	(,(rx
	   ;; Check for where statements
	   (group bol (* space) (or "let" "where" "," "{" "") (* space))
	   ;; The actual function name
	   (group (+ graph))
	   ;; The equal sign
	   (group (* (regex ".")) space "=" (or space eol))) . (2 font-lock-function-name-face))

	;; Type signatures
	(,(rx (group bol (* space) (or "let" "where" "," "{" "") (* space))
	      (group (+ graph))
	      (group (+ space) "::" space)) . (2 font-lock-function-name-face))))

;; ***** Issues
;; ****** Multiline region comment doesn't work
;; ****** Doesn't work on tuple definitions
;; In this code block:

;; let (foo, bar) = (1, 2)

;; Only (foo is highlighted

;; ****** Dosen't highlight equal without space
;; For example in enums you can do this:

;; enum	DebugDrawModes
;; {
;; DBG_NoDebug=0,
;; DBG_DrawWireframe=1,
;; DBG_DrawAabb=2,
;; DBG_DrawFeaturesText=4,

;; But this isn't highlighted

;; ****** Multiline functions
;; This doesn't work on multi-line functions:
;; collisionAABBCheck
;;   :: Foo
;;   Bar =

;; collisionAABBCheck
;;   foo
;;   bar =

;; ****** Records
;; Doesn't work properly when setting records like this:
;; foo{ _bar =
;; , _baz    =
;; , _qux   =
;; }
;; The comma and ~foo{~ gets highlighted instead of the functions

;; **** Syntax table
(with-eval-after-load 'haskell-mode
  (setq haskell-mode-syntax-table
	(let ((synTable (make-syntax-table)))
	  ;; Region style comment
	  (modify-syntax-entry ?\{ ". 1" synTable)
	  (modify-syntax-entry ?\} ". 4" synTable)

	  ;; Region and line style comment
	  (modify-syntax-entry ?- ". 123b" synTable)

	  ;; Return
	  (modify-syntax-entry ?\n "> b" synTable)
	  (modify-syntax-entry ?\^m "> b" synTable)
	  synTable)))

;; *** org-mode (ob) support
(with-eval-after-load 'haskell-mode
  (require 'ob-haskell))

;; *** Make init messages read only
(with-eval-after-load 'haskell-interactive-mode
  (defun haskell-interactive-mode-echo (session message &optional mode)
    "Echo a read only piece of text before the prompt."
    (with-current-buffer (haskell-session-interactive-buffer session)
      (save-excursion
	(haskell-interactive-mode-goto-end-point)
	(insert (if mode
		    (haskell-fontify-as-mode
		     (concat message "\n")
		     mode)
		  (propertize (concat message "\n")
			      'front-sticky t
			      'read-only nil
			      'rear-nonsticky t)))))))

;; **** Start using nix-shell
;; (setq haskell-process-wrapper-function (lambda (argv) (append (list "nix-shell" "--pure" "-I" "." "--command" )
;;							 (list (mapconcat ’identity argv " ")))))

;; *** Haskell process/prompt
(with-eval-after-load 'haskell-interactive-mode
  (evil-define-key '(normal insert visual replace) haskell-interactive-mode-map (kbd "C-c") 'haskell-process-interrupt)
  (evil-define-key '(normal insert visual replace) haskell-interactive-mode-map (kbd "C-z") 'haskell-process-interrupt)
  (evil-define-key '(normal insert) haskell-interactive-mode-map (kbd "C-n") 'haskell-interactive-mode-history-next)
  (evil-define-key '(normal insert) haskell-interactive-mode-map (kbd "C-p") 'haskell-interactive-mode-history-previous))

;; **** Load specific file
(defun haskell-process-load-specific-file (file)
  "Load a specific file."
  (interactive)
  (save-buffer)
  (haskell-interactive-mode-reset-error (haskell-session))
  (haskell-process-file-loadish (format "load \"%s\"" (replace-regexp-in-string
						       "\""
						       "\\\\\""
						       file))
				nil
				(current-buffer)))

;; **** Disable auto completion
;; Disable auto completion in haskell-interactive-mode because it's slow
(add-hook 'haskell-interactive-mode-hook (lambda () (company-mode -1)))

;; **** Max memory
;; (require 'haskell-customize)
;; (add-to-list 'haskell-process-args-ghci "+RTS")
;; (add-to-list 'haskell-process-args-ghci "-M1m")

;; (add-to-list 'haskell-process-args-cabal-new-repl "--ghc-option=+RTS")
;; (add-to-list 'haskell-process-args-cabal-new-repl "--ghc-option=-M1m")

;; This would work but the quotes can't be escaped cause then they are never formatted back to normal quotes again
;; (add-to-list 'haskell-process-args-cabal-new-repl "--ghc-options="+RTS -M100m"")
;; (add-to-list 'haskell-process-args-cabal-repl "--ghc-options="+RTS -M100m"")

;; **** Run expr
;; Just patch the :complete to also run eros overlay
;; Also removed the (insert "\n") cause it was causing everything that I evaled to add one newline
(defun my/haskell-interactive-mode-run-expr (expr)
  "Run the given expression."
  (let ((session (haskell-interactive-session))
	(process (haskell-interactive-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (list session process expr 0)
      :go (lambda (state)
	    ;; I also commented out these because they are just used when in repl-mode I think
	    ;; (goto-char (point-max))
	    ;; (insert "\n")
	    ;; (setq haskell-interactive-mode-result-end
	    ;;	  (point-max))
	    (haskell-process-send-string (cadr state)
					 (haskell-interactive-mode-multi-line (cl-caddr state)))
	    (haskell-process-set-evaluating (cadr state) t))
      :live (lambda (state buffer)
	      (unless (and (string-prefix-p ":q" (cl-caddr state))
			   (string-prefix-p (cl-caddr state) ":quit"))
		(let* ((cursor (cl-cadddr state))
		       (next (replace-regexp-in-string
			      haskell-process-prompt-regex
			      ""
			      (substring buffer cursor))))
		  (haskell-interactive-mode-eval-result (car state) next)
		  (setf (cl-cdddr state) (list (length buffer)))
		  nil)))
      :complete
      (lambda (state response)
	(haskell-process-set-evaluating (cadr state) nil)
	(unless (haskell-interactive-mode-trigger-compile-error state response)
	  (haskell-interactive-mode-expr-result state response)
	  (eros--eval-overlay response (my/next-line-pos))))))))

;; **** Patch multi-line
;; Turn 'prompt2' into prompt-cont. 'prompt2' might be deprecated
(with-eval-after-load 'haskell-interactive-mode
  (defun haskell-interactive-mode-multi-line (expr)
    "If a multi-line expression EXPR has been entered, then reformat it to be:

:{
do the
   multi-liner
   expr
:}"
    (if (not (string-match-p "\n" expr))
	expr
      (let ((pre (format "^%s" (regexp-quote haskell-interactive-prompt)))
	    (lines (split-string expr "\n")))
	(cl-loop for elt on (cdr lines) do
		 (setcar elt (replace-regexp-in-string pre "" (car elt))))
	;; Temporarily set prompt2 to be empty to avoid unwanted output
	(concat ":set prompt-cont \"\"\n"
		":{\n"
		(mapconcat #'identity lines "\n")
		"\n:}\n"
		(format ":set prompt-cont \"%s\"" haskell-interactive-prompt-cont))))))

;; **** Better copy to prompt
(defun my/haskell-interactive-copy-string-to-prompt (string)
  "Copy the current line to the prompt, overwriting the current prompt."
  (let ((l (substring-no-properties string)))
    ;; If it looks like the prompt is at the start of the line, chop
    ;; it off.
    (when (and (>= (length l) (length haskell-interactive-prompt))
	       (string= (substring l 0 (length haskell-interactive-prompt))
			haskell-interactive-prompt))
      (setq l (substring l (length haskell-interactive-prompt))))
    (haskell-interactive-mode-set-prompt l)))

;; *** nix-haskell-mode
;; It's buggy for me
;; (straight-use-package 'nix-haskell-mode)
;; (require 'nix-haskell-mode)
;; (add-hook 'haskell-mode-hook 'nix-haskell-mode)

;; *** GHC flags
;; https://downloads.haskell.org/~ghc/master/users-guide/using-warnings.html?source=post_page---------------------------
;; https://medium.com/mercury-bank/enable-all-the-warnings-a0517bc081c3
(setq my/ghc-flags
      '(
	"-Weverything"
	"-Wincomplete-uni-patterns"
	"-Wincomplete-record-updates"

	;; Don't warn if prelude is implicitly imported
	;; "-Wimplicit-prelude"
	"-Wno-implicit-prelude"

	;; Dante disables this by default
	"-Wmissing-home-modules"

	"-Widentities"
	"-Wredundant-constraints"
	"-Wpartial-fields"

	;; Normally there is a warning on every non-annotated top-level function https://gitlab.haskell.org/ghc/ghc/issues/14794?source=post_page---------------------------#ticket
	"-Wno-missing-exported-signatures"

	;; Warns that you haven't defined an export list. Without an export list all functions in the file are accessible
	;; "-Wmissing-export-lists"
	"-Wno-missing-export-lists"

	;; Don't give warning when imports arent either qualified or imported using import lists
	"-Wno-missing-import-lists"

	;; "Don’t use Safe Haskell warnings"
	"-Wno-unsafe"
	;; "Don’t use Safe Haskell warnings"
	"-Wno-safe"
	;; "Warning for polymorphic local bindings; nothing wrong with those"
	"-Wno-missing-local-signatures"
	;; "Warn if the monomorphism restriction is used"
	"-Wmonomorphism-restriction"

	;; Warns if haskell has problems inlining
	"-Wall-missed-specialisations"

	"-Wcpp-undef"

	;; By default you get a warning when you don't include type signature when writing function, disable that
	"-Wno-missing-signatures"
	))

(defun my/cabal-ghc-flags-insert ()
  (interactive)
  (insert "ghc-options: ")
  (insert (mapconcat 'identity my/ghc-flags " ")))

;; *** Hoogle
;; **** Ivy
;; (straight-use-package '(ivy-hoogle :type git :host github :repo "sjsch/ivy-hoogle"))

(defvar my/ivy-hoogle-max-entries 100)

(defun my/ivy-hoogle--do-search (str)
  (let* ((args (concat
		"search "
		"-l "
		(and my/ivy-hoogle-max-entries (concat "-n " (int-to-string my/ivy-hoogle-max-entries))))))
    (message args)
    (counsel--async-command (format "hoogle %s \"%s\""
				    args
				    str))
    '("working...")))

(defun my/ivy-hoogle ()
  "Perform a hoogle search."
  (interactive)
  (ivy-read "Hoogle: "
	    #'my/ivy-hoogle--do-search
	    :dynamic-collection t
	    :preselect (symbol-at-point)
	    ;; :preselect (counsel-symbol-at-point)
	    :re-builder #'regexp-quote
	    :action (lambda (str)
		      (browse-url
		       (substring-no-properties str
						;; + 3 is to remove the "-- " from the string
						(+ (string-match "-- .*$" str) 3)
						(length str))))
	    :caller 'my/ivy-hoogle))

;; *** Ivy top level definitions
;; This doesn't work with normal "M-x imenu", but it works fine with "M-x counsel-imenu"

(setq my/haskell-imenu-generic-expression
      `((""  ,(rx (group bol (+ graph) (+ space) "::" space
			 ;; The rest of the expression after the "::". This is so that the type info is visible in the imenu
			 (regex ".*") eol
			 )) 1)))

(add-hook 'haskell-mode-hook (lambda ()
			       ;; This is needed because haskell-mode messes with it which causes imenu to use that instead
			       (setq imenu-create-index-function 'imenu-default-create-index-function)
			       (setq imenu-generic-expression my/haskell-imenu-generic-expression)
			       ) t)

;; *** Disable haskell-doc
;; Haskell-doc is loaded in as the eldoc-documentation-function.
(add-hook 'haskell-mode-hook (lambda () (setq-local eldoc-documentation-function nil)))

;; *** Project management
;; **** Stack
;; (straight-use-package 'hasky-stack)

;; **** Cabal
;; (straight-use-package 'hasky-cabal)

;; ***** Compile
(defvar my/profiler-graph-gen-program "hp2pretty")

;; Show graph after compilation of profiling command
(add-to-list 'compilation-finish-functions (lambda (_a _b)
					     (when (string-match-p my/profiler-graph-gen-program compile-command)
					       (find-file (car (directory-files default-directory nil ".svg$"))))))

(defvar my/haskell-compile-commands
  '("cabal build"
    "cabal run"
    "profiling"
    "cabal build -O2"))

(defun my/cabal-compile ()
  (let ((default-directory (projectile-compilation-dir)))
    (compile
     (my/cabal-create-compile-command))))

(defun my/cabal-create-compile-command ()
  (let ((input (completing-read "" my/haskell-compile-commands)))
    (if (string= input "profiling")
	(my/cabal-build-profiling-command)
      input)))

(defun my/cabal-build-profiling-command ()
  (concat "cabal run --enable-profiling exes -- +RTS -p "
	  (my/cabal-get-profiling-method)
	  " -L100; " my/profiler-graph-gen-program " *.hp;"))
;; find-file FRPSimpleGame.svg

(defun my/cabal-get-profiling-method ()
  (car (split-string
	(completing-read "" my/cabal-profiling-methods)
	" =")))

(defvar my/cabal-profiling-methods
  '(
    "-hc = Breaks down the graph by the cost-centre stack which produced the data."
    "-hm = Break down the live heap by the module containing the code which produced the data."
    "-hd = Breaks down the graph by closure description. For actual data, the description is just the constructor name, for other closures it is a compiler-generated string identifying the closure."
    "-hy = Breaks down the graph by type. For closures which have function type or unknown/polymorphic type, the string will represent an approximation to the actual type."
    "-hr = Break down the graph by retainer set. Retainer profiling is described in more detail below (Section 5.4.2, “Retainer Profiling”)."
    "-hb = Break down the graph by biography. Biographical profiling is described in more detail below (Section 5.4.3, “Biographical Profiling”)."
    "-hcname,... = Restrict the profile to closures produced by cost-centre stacks with one of the specified cost centres at the top."
    "-hCname,... = Restrict the profile to closures produced by cost-centre stacks with one of the specified cost centres anywhere in the stack."
    "-hmmodule,... = Restrict the profile to closures produced by the specified modules."
    "-hddesc,... = Restrict the profile to closures with the specified description strings."
    "-hytype,... = Restrict the profile to closures with the specified types."
    "-hrcc,... = Restrict the profile to closures with retainer sets containing cost-centre stacks with one of the specified cost centres at the top."
    "-hbbio,... = Restrict the profile to closures with one of the specified biographies, where bio is one of lag, drag, void, or use."
    "-isecs: = Set the profiling (sampling) interval to secs seconds (the default is 0.1 second). Fractions are allowed: for example -i0.2 will get 5 samples per second. This only affects heap profiling; time profiles are always sampled with the frequency of the RTS clock. See Section 5.3, “Time and allocation profiling” for changing that."
    "-xt = Include the memory occupied by threads in a heap profile. Each thread takes up a small area for its thread state in addition to the space allocated for its stack (stacks normally start small and then grow as necessary). This includes the main thread, so using -xt is a good way to see how much stack space the program is using."))

;; ****** Reflex
(setq my/haskell-reflex-compile-commands
      '(
	;; Reflex
	"ob run"
	))

(defun my/haskell-reflex-compile ()
  (let ((default-directory (projectile-compilation-dir)))
    (compile
     (car my/haskell-reflex-compile-commands)
     ;; (completing-read "Compile: " my/haskell-reflex-compile-commands)
     )))

;; **** Nix cabal
;; Use nix-haskell-mode for automatic project management
;; (straight-use-package 'nix-haskell-mode)

;; *** Formatting
;; (setq haskell-stylish-on-save t)
;; (setq haskell-mode-stylish-haskell-path "brittany")
;; (setq haskell-mode-stylish-haskell-args "")

;; (setq haskell-mode-stylish-haskell-path "ormolu")
(setq haskell-mode-stylish-haskell-path "stylish-haskell")

;; (setq haskell-mode-stylish-haskell-path "hindent")

;; *** Extension management
(defun my/haskell-add-language-extension ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((extension (completing-read "Extension: " haskell-ghc-supported-extensions)))
      (insert (concat "{-# LANGUAGE "  extension " #-}\n")))))

(with-eval-after-load 'haskell-mode
  (define-key my/haskell-mode-map (kbd "e") 'my/haskell-add-language-extension))

;; *** GHC options
(defun my/haskell-add-ghc-option ()
  (interactive)
  (save-excursion
    (let ((extension (completing-read "Option: " haskell-ghc-supported-options)))
      (insert extension))))

(with-eval-after-load 'haskell-mode
  (define-key my/haskell-mode-map (kbd "o") 'my/haskell-add-ghc-option))

;; **** Hasky
(straight-use-package 'hasky-extensions)

(with-eval-after-load 'haskell-mode
  (define-key my/haskell-mode-map (kbd "E") 'hasky-extensions-browse-docs))

;; *** Haskell-cabal
(straight-use-package 'company-cabal)
(add-to-list 'company-backends 'company-cabal)

;; *** lsp-haskell
(defun my/haskell-lsp-mode ()
  (interactive)
  ;; haskll-doc-mode is buggy if eldoc is on
  ;; (setq-local lsp-eldoc-enable-hover nil)

  ;; (setq-local lsp-ui-flycheck-enable nil)

  ;; lsp-haskell doesn't work with native json
  ;; (setq-local lsp-use-native-json nil)

  (lsp)

  ;; Disable flycheck because errors are generated by haskell-interactive-mode anyways
  ;; (flycheck-disable-checker 'lsp-ui)
  ;; (setq-local flycheck-checker 'haskell-ghc)

  ;; (setq-local flycheck-checkers (remove 'lsp-ui flycheck-checkers))

  (setq-local lsp-ui-doc-enable t
	      lsp-ui-peek-enable t
	      lsp-ui-sideline-enable t
	      lsp-ui-imenu-enable t
	      lsp-ui-flycheck-enable t)
  ;; (setq lsp-ui-sideline-ignore-duplicate t)

  (setq-local lsp-ui-sideline-show-code-actions t
	      ;; Errors i think
	      lsp-ui-sideline-show-diagnostics nil

	      ;; someFunc :: IO ()
	      lsp-ui-sideline-show-hover t

	      ;; [someFunc]
	      lsp-ui-sideline-show-symbol t)

  (setq-local lsp-eldoc-enable-hover t)
  ;; (setq-local eldoc-documentation-function 'ignore)
  (setq-local lsp-eldoc-render-all t)
  (setq-local flycheck-display-errors-function 'my/flycheck-display-message)

  ;; (flycheck-mode -1)
  ;; (flycheck-posframe-mode -1)

  ;; (setq-local flycheck-display-errors-function nil)
  )

(when my/haskell-hie-enable
  ;; (straight-use-package 'lsp-haskell)
  (straight-use-package 'lsp-haskell)

  (require 'lsp)
  (require 'lsp-haskell)

  (add-hook 'haskell-mode-hook 'my/haskell-lsp-mode))

;; **** Hack in eldoc support
(setq my/use-lsp-eldoc-hack t)
(when (and my/haskell-hie-enable my/use-lsp-eldoc-hack)
  (with-eval-after-load 'lsp-ui-sideline
    (setq my/haskell-lsp-eldoc-entries '())

    ;; This function modifies what's displayed in lsp-ui-sideline. Here it is redefined so that it takes what's supposed to be displayed in the sideline, and instead sends it to an eldoc cache
    (defun lsp-ui-sideline--push-info (symbol tag bounds info bol eol)
      (when (and (= tag (lsp-ui-sideline--calculate-tag))
		 (not (lsp-ui-sideline--stop-p)))
	(let* ((info (concat (thread-first (gethash "contents" info)
			       lsp-ui-sideline--extract-info
			       lsp-ui-sideline--format-info)))
	       (current (and (>= (point) (car bounds)) (<= (point) (cdr bounds)))))
	  (when (and (> (length info) 0)
		     (lsp-ui-sideline--check-duplicate symbol info))
	    (let* ((final-string (lsp-ui-sideline--make-display-string info symbol current))
		   (pos-ov (lsp-ui-sideline--find-line (length final-string) bol eol))
		   (ov (when pos-ov (make-overlay (car pos-ov) (car pos-ov)))))

	      ;; My changes:
	      (let ((final-string-formatted (substring-no-properties final-string)))
		(add-to-list 'my/haskell-lsp-eldoc-entries final-string-formatted))

	      (when pos-ov
		;; (overlay-put ov 'info info)
		;; (overlay-put ov 'symbol symbol)
		(overlay-put ov 'bounds bounds)
		(overlay-put ov 'current current)
		;;(overlay-put ov 'after-string final-string)
		(overlay-put ov 'window (get-buffer-window))
		(overlay-put ov 'kind 'info)
		(push ov lsp-ui-sideline--ovs)))))))

    (defun my/haskell-lsp-eldoc-print ()
      (interactive)
      (when my/haskell-lsp-eldoc-entries
	(let ((at-point (thing-at-point 'symbol t)))
	  (when at-point
	    (let ((str (seq-find
			(lambda (candidate)
			  (let ((candidate-last-word (string-match (rx (not whitespace) (regexp "*") space eol) candidate)))
			    (if candidate-last-word
				(progn
				  (string=
				   (substring candidate candidate-last-word (- (length candidate) 1))
				   at-point))
			      nil)))
			my/haskell-lsp-eldoc-entries)))

	      (if str
		  (s-trim str)
		nil))))))

    (defun my/haskell-lsp-print-type ()
      (let ((msg (my/haskell-lsp-eldoc-print)))
	(when msg
	  (message "%s" msg))))

    (defun my/haskell-lsp-eldoc-init ()
      (eldoc-mode -1)
      ;; (setq-local eldoc-documentation-function 'my/haskell-lsp-eldoc-print)

      (add-hook 'post-command-hook
		'my/haskell-lsp-print-type nil t))

    ;; No idea why but eldoc doesn't run the documentation function unless i press escape, this fixes that
    (add-hook 'haskell-mode-hook (lambda ()
				   (run-with-timer 1 nil 'my/haskell-lsp-eldoc-init)
				   ;; Slow startup
				   (run-with-timer 5 nil 'my/haskell-lsp-eldoc-init)
				   (run-with-timer 10 nil 'my/haskell-lsp-eldoc-init)))))

;; *** Dante
(unless my/haskell-hie-enable
  (with-eval-after-load 'haskell-mode
    (setq dante-tap-type-time nil)
    ;; (setq dante-tap-type-time 0)

    (straight-use-package 'dante)

    ;; (require 'dante)
    (add-hook 'haskell-mode-hook 'dante-mode)

    ;; (defun my/dante-mode ()
    ;;   (my/dante-fix-flycheck))

    ;; (add-hook 'dante-mode-hook 'my/dante-mode)
    ))

;; ;; Set flycheck settings
;; (defun my/dante-fix-flycheck ()
;;   ;; Settings good for both dante and the haskell repl
;;   (setq-local flymake-no-changes-timeout nil)
;;   (setq-local flymake-start-syntax-check-on-newline nil)
;;   (setq-local flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch))) ;; idle-change

;; **** Fix eval selection bug
(unless my/haskell-hie-enable
  (with-eval-after-load 'dante
    (defun my/dante-idle-function-checks ()
      (when (or (string= evil-state "normal") (string= evil-state "insert"))
	(dante-idle-function)))

    (add-hook 'haskell-mode-hook
	      (lambda () (add-hook 'post-command-hook #'my/dante-idle-function-checks nil t)))))

;; **** Add more warnings
(unless my/haskell-hie-enable
  (setq my/ghc-warning-parameters
	;; Dante disables this by default, so remove it
	(remove "-Wmissing-home-modules" my/ghc-flags))

  (with-eval-after-load 'haskell-mode
    (require 'dante)
    (setq dante-load-flags (append dante-load-flags my/ghc-warning-parameters))))

;; Remove duplicates if any
;; (setq dante-load-flags (remove-duplicates dante-load-flags :test 'string=))

;; **** Add hlint to dante
(unless my/haskell-hie-enable
  (add-hook 'haskell-mode-hook (lambda ()
				 (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))
				 ;; Remove dante since the haskell repl is a lot faster at detecting errors anyways
				 ;; But turns out this leads to some packages being labled hidden?
				 ;; (add-to-list 'flycheck-disabled-checkers 'haskell-dante)

				 ;; Dante runs a lot faster now? Also problem with haskell-ghc is that it doesn't care about cabal files, it just runs ghc on the current file
				 (add-to-list 'flycheck-disabled-checkers 'haskell-ghc))))

;; **** Apply GHC hints
;; (straight-use-package 'attrap)

;; **** Disable nix boot
(setq dante-methods '(bare-cabal))

;; **** Fix
;; Dante wants to use the cabal dist folder instead of the dist-newstyle folder which makes it crash on newstyle projects
;; Here I just changed dist to dist-newstyle
(setq dante-methods-alist
      `((styx "styx.yaml" ("styx" "repl" dante-target))
	(snack ,(lambda (d) (directory-files d t "package\\.\\(yaml\\|nix\\)")) ("snack" "ghci" dante-target))
	(new-impure-nix dante-cabal-new-nix ("nix-shell" "--run" (concat "cabal new-repl " (or dante-target (dante-package-name) "") " --builddir=dist-newstyle/dante")))
	(new-nix dante-cabal-new-nix ("nix-shell" "--pure" "--run" (concat "cabal new-repl " (or dante-target (dante-package-name) "") " --builddir=dist-newstyle/dante")))
	(nix dante-cabal-nix ("nix-shell" "--pure" "--run" (concat "cabal repl " (or dante-target "") " --builddir=dist-newstyle/dante")))
	(impure-nix dante-cabal-nix ("nix-shell" "--run" (concat "cabal repl " (or dante-target "") " --builddir=dist-newstyle/dante")))
	(new-build "cabal.project.local" ("cabal" "new-repl" (or dante-target (dante-package-name) nil) "--builddir=dist-newstyle/dante"))
	(nix-ghci ,(lambda (d) (directory-files d t "shell.nix\\|default.nix")) ("nix-shell" "--pure" "--run" "ghci"))
	(stack "stack.yaml" ("stack" "repl" dante-target))
	(mafia "mafia" ("mafia" "repl" dante-target))
	(bare-cabal ,(lambda (d) (directory-files d t "..cabal$")) ("cabal" "repl" dante-target "--builddir=dist-newstyle/dante"))
	(bare-ghci ,(lambda (_) t) ("ghci"))))

;; *** Flycheck
;; Remove flycheck stack-ghc since it freezes emacs without stack. Don't remove the standard ghc checker though, because it works fine if I don't have HIE. If I have HIE emacs should use that instead
(add-hook 'haskell-mode-hook (lambda ()
			       (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc)))

;; *** Snippet utils
;; Used in yasnippets
(defun my/get-data-name ()
  "Finds the variable name of the data declaration above cursor"
  (save-excursion
    (search-backward-regexp "^data\ ")
    (search-forward-regexp "[:word:]")
    (search-forward-regexp "[:word:]")
    (thing-at-point 'symbol t)))

;; ** retrie
;; Currently broken on nixos
;; (straight-use-package 'retrie)

;; *** Syntax highlight functions correctly
;; (add-hook 'haskell-mode-hook
;;	  (lambda ()
;;	    (font-lock-add-keywords nil
;;				    '(((rx bol (not blank) space "::" space ) 1
;;				       font-lock-warning-face t)))))
;; (font-lock-add-keywords nil
;;			`(("\\_< :: \\_>"
;;			   (,(rx bol word)
;;			    nil   ;; Pre-match form
;;			    nil   ;; Post-match form
;;			    (1 font-lock-type-face)))))

;; ** C/CPP
;; *** LSP CCLS
(straight-use-package 'ccls)

(setq ccls-executable "/bin/ccls")

(defun my/c-mode ()
  (lsp)
  (lsp-lens-mode)
  (push 'company-lsp company-backends)

  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq-local evil-shift-width 4))

(add-hook 'c-mode-hook 'my/c-mode)
(add-hook 'c++-mode-hook 'my/c-mode)
(add-hook 'objc-mode-hook 'my/c-mode)

;; *** Cling
;; https://github.com/brianqq/inferior-cling
(defun my/cling (&optional flags)
  "Move to the buffer containing Cling, or create one if it does not exist. Defaults to C++11"
  (interactive)
  (let ((flags (or flags "-std=c++11")))
    (make-comint "inferior-cling" "cling" nil flags)
    (switch-to-buffer-other-window "*inferior-cling*")))

(defun my/cling-send-string (string &optional process)
  "Send a string terminated with a newline to the inferior-cling buffer. Has the effect of executing a command"
  (let ((process (or process (get-process "inferior-cling"))))
    (comint-send-string process string)
    (comint-send-string process "\n")))

(defun my/cling-send-region (start end)
  "Sends the region in the current buffer between `start` and `end` to the inferior-cling buffer. Sends the currently selected region when called interactively."
  (interactive "r")
  (my/cling-send-string (buffer-substring start end)))

(defun my/cling-send-buffer ()
  "Sends the current buffer to the inferior-cling buffer."
  (interactive)
  (my/cling-send-region (point-min) (point-max))) ;;do i want to wrap-raw this?

(defun my/cling-wrap-raw (string)
  "Wraps `string` in \".rawInput\", which tells Cling to accept function definitions"
  (format ".rawInput\n%s\n.rawInput" string))

(defun my/cling-wrap-region-and-send (start end)
  "Sends the region between start and end (currently selected when called interactively) to cling in raw input mode "
  (interactive "r")
  (my/cling-send-string (my/cling-wrap-raw (buffer-substring start end))))

(defun my/flatten-function-def ()
  "Flattens a function definition into a single line. This makes it easier to send to the inferior-cling buffer"
  (interactive)
  (replace-regexp "
   " "" nil (mark) (point))) ;;;Why did I do this again?

(defun my/select-defun ()
  "Selects the defun containing the point. Currently only works when point is on the line where the function's name is declared."
  (interactive)
  (move-beginning-of-line nil)
  (push-mark (point))
  (re-search-forward "{")
  (save-excursion
    (my/flatten-function-def))
  (backward-char)
  (forward-sexp))

(defun my/cling-wrap-defun-and-send ()
  "Sends the current defun to cling in raw input mode. Currently only works when point is on the first line of function definition."
  (interactive)
  (save-excursion
    (my/select-defun)
    (my/cling-wrap-region-and-send (mark) (point))
    (undo)
    (undo)));;;this is a rather leaky way of doing temporary changes. there should be some way to save buffer contents or something
   ;;;probably uses with-temp-buffer

;; *** Keys
(define-prefix-command 'my/c-mode-map)

(evil-define-key 'normal c-mode-map (kbd (concat my/leader-map-key " a")) 'my/c-mode-map)
(evil-define-key 'normal c++-mode-map (kbd (concat my/leader-map-key " a")) 'my/c-mode-map)

(define-key my/c-mode-map (kbd "e") 'cling)

(define-key my/c-mode-map (kbd "n") 'gud-step)
(define-key my/c-mode-map (kbd "N") 'gud-next)

(define-key my/c-mode-map (kbd "p") 'gud-print)
(define-key my/c-mode-map (kbd "P") 'gud-pstar)

(define-key my/c-mode-map (kbd "s") 'gud-go)

;; Run to selected line
(define-key my/c-mode-map (kbd "g") 'gud-until)

;; Run out of function
(define-key my/c-mode-map (kbd "f") 'gud-finish)

(define-key my/c-mode-map (kbd "v") 'gdb-display-locals-buffer)
(define-key my/c-mode-map (kbd "V") 'gdb-display-locals-for-thread)

(define-key my/c-mode-map (kbd "b") 'gdb-display-breakpoints-buffer)

;; ** C#
(straight-use-package 'csharp-mode)

;; csharp-maybe-insert-codedoc
(setq csharp-mode-map (make-sparse-keymap))

;; *** REPL
(defun my/csharp-run-repl()
  (interactive)
  (eshell)
  (insert "csharp")
  (eshell-send-input))

;; *** Omnisharp-emacs
(straight-use-package 'omnisharp)

;; (add-hook 'csharp-mode-hook (lambda () (push 'company-omnisharp company-backends)))

(with-eval-after-load 'company
  (add-to-list 'company-backends #'company-omnisharp))

;; This hack fixes omnisharp on arch linux
;; Read https://github.com/OmniSharp/omnisharp-emacs/issues/459
(if (eq system-type 'gnu/linux)
    (let ((dotnet-version (string-trim (shell-command-to-string "dotnet --version")))) (setenv "MSBuildSDKsPath" (format "/opt/dotnet/sdk/%s/Sdks" dotnet-version))))

;; **** Keys
(define-prefix-command 'my/csharp-mode-map)
(evil-define-key 'normal csharp-mode-map (kbd (concat my/leader-map-key " a")) 'my/csharp-mode-map)

(define-key my/csharp-mode-map (kbd "r") 'omnisharp-run-code-action-refactoring)
(define-key my/csharp-mode-map (kbd "S") 'omnisharp-reload-solution)
(define-key my/csharp-mode-map (kbd "u") 'omnisharp-fix-usings)
(define-key my/csharp-mode-map (kbd "i") 'omnisharp-find-implementations)
(define-key my/csharp-mode-map (kbd "e") 'omnisharp-solution-errors)
(define-key my/csharp-mode-map (kbd "o") 'omnisharp-show-overloads-at-point)

(define-key my/csharp-mode-map (kbd "g") 'omnisharp-navigate-to-solution-file)

;; **** Write formatting settings to omnisharp server config
;; omnisharp.json should be in ~/.omnisharp on all OSs
;; if(not(file-directory-p "~/.omnisharp")
;;     (make-directory "~/.omnisharp"))

;; (if(not(file-exists-p "~/.omnisharp/omnisharp.json"))
;;     (progn
;;       (write-region "
;;         {
;;             \"formattingOptions\": {
;;                 PUT OPTIONS HERE
;;             }
;;         }
;;        " nil "~/.omnisharp/omnisharp.json")

;;       (message "~/.omnisharp/omnisharp.json created")
;;       )
;;   )

;; *** My csharp mode
(defun my/csharp-mode ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq evil-shift-width-local 4))

(add-hook 'csharp-mode-hook 'my/csharp-mode)

;; ** F#
(straight-use-package 'fsharp-mode)

;; :mode ("\\.fs\\'" . fsharp-mode)
(setq fsharp-doc-idle-delay 0)

;; (setq-default fsharp-indent-offset 2)

;; *** Keys
(add-hook 'fsharp-mode-hook 'my/fsharp-keys-init)

(define-prefix-command 'my/fsharp-mode-map)

(defun my/fsharp-keys-init()
  (interactive)
  (evil-define-key 'normal fsharp-mode-map (kbd (concat my/leader-map-key " a")) 'my/fsharp-mode-map))

(define-key my/fsharp-mode-map (kbd "v") 'fsharp-mark-phrase)
(define-key my/fsharp-mode-map (kbd "b") 'fsharp-goto-block-up)
(define-key my/fsharp-mode-map (kbd "C-r") 'fsharp-ac-status)
(define-key my/fsharp-mode-map (kbd "C-k") 'fsharp-ac/stop-process)
(define-key my/fsharp-mode-map (kbd "C-s") 'fsharp-ac/start-process)

;; ** Markdown
(straight-use-package 'markdown-mode)

;; ** Web mode
;; TODO: Fix settings, grab them from package site
(straight-use-package 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; Messes with .cs files???
;; (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))

;; *** Debugger
(straight-use-package 'dap-mode)

;; ** Gnuplot
(straight-use-package 'gnuplot)

;; ** Structural editing
;; *** Lispy
(straight-use-package 'lispy)

(defhydra my/lispy-hydra (:hint nil
				:color red)
  "lisp"

  ("H" (call-interactively #'lispy-backward nil))
  ("L" (call-interactively #'lispy-flow nil))

  ("C-l" (call-interactively #'lispy-knight-down) nil)
  ("C-h" (call-interactively #'lispy-knight-up) nil)

  ("l" (call-interactively #'lispy-right) nil)
  ("h" (call-interactively #'lispy-left) nil)

  ;;   ("l" (call-interactively #'down-list) nil)
  ;;   ("h" (call-interactively #'up-list) nil)

  ("N" (call-interactively #'lispy-raise) nil)
  ("P" (call-interactively #'lispy-convolute) nil)

  ("n" (call-interactively #'lispy-down) nil)
  ("p" (call-interactively #'lispy-up) nil)

  ("u" (call-interactively #'undo nil))
  ;;("u" (call-interactively #'lispy-back nil))

  ("e" (call-interactively #'my/auto-eval nil))

  ("o" (call-interactively #'lispy-different nil))
  ("d" (call-interactively #'lispy-kill nil))

  ;;("y" (call-interactively #'lispy-occur nil))


  (">" (call-interactively #'lispy-slurp nil))
  ("<" (call-interactively #'lispy-barf nil))
  ("/" (call-interactively #'lispy-splice nil))

  ;;  ("r" (call-interactively #'lispy-raise nil))
  ;;  ("R" (call-interactively #'lispy-raise-some nil))

  ;;("+" (call-interactively #'lispy-join nil))

  ;;  ("C" (call-interactively #'lispy-splice nil))
  ;;  ("X" (call-interactively #'lispy-splice nil))
  ;;  ("w" (call-interactively #'lispy-splice nil))
  ;;  ("s" (call-interactively #'lispy-splice nil))
  ;;  ("/" (call-interactively #'lispy-splice nil))
  ;;  ("/" (call-interactively #'lispy-splice nil))
  ;;  ("/" (call-interactively #'lispy-splice nil))
  ;;  ("/" (call-interactively #'lispy-splice nil))
  ;;  ("/" (call-interactively #'lispy-splice nil))

  ("<escape>" nil nil))

;; ;; navigation
;; (lispy-define-key map "C" 'lispy-convolute)
;; (lispy-define-key map "X" 'lispy-convolute-left)
;; (lispy-define-key map "w" 'lispy-move-up)
;; (lispy-define-key map "s" 'lispy-move-down)
;; (lispy-define-key map "O" 'lispy-oneline)
;; (lispy-define-key map "M" 'lispy-alt-multiline)
;; (lispy-define-key map "S" 'lispy-stringify)
;; ;; marking
;; (lispy-define-key map "a" 'lispy-ace-symbol
;;   :override '(cond ((looking-at lispy-outline)
;;                     (lispy-meta-return))))
;; (lispy-define-key map "H" 'lispy-ace-symbol-replace)
;; (lispy-define-key map "m" 'lispy-mark-list)
;; ;; dialect-specific
;; (lispy-define-key map "e" 'lispy-eval)
;; (lispy-define-key map "E" 'lispy-eval-and-insert)
;; (lispy-define-key map "G" 'lispy-goto-local)
;; (lispy-define-key map "g" 'lispy-goto)
;; (lispy-define-key map "F" 'lispy-follow t)
;; (lispy-define-key map "D" 'pop-tag-mark)
;; (lispy-define-key map "A" 'lispy-beginning-of-defun)
;; (lispy-define-key map "_" 'lispy-underscore)
;; ;; miscellanea
;; (define-key map (kbd "SPC") 'lispy-space)
;; (lispy-define-key map "i" 'lispy-tab)
;; (lispy-define-key map "I" 'lispy-shifttab)
;; (lispy-define-key map "N" 'lispy-narrow)
;; (lispy-define-key map "W" 'lispy-widen)
;; (lispy-define-key map "c" 'lispy-clone)
;; (lispy-define-key map "u" 'lispy-undo)
;; (lispy-define-key map "q" 'lispy-ace-paren
;;   :override '(cond ((bound-and-true-p view-mode)
;;                     (View-quit))))
;; (lispy-define-key map "Q" 'lispy-ace-char)
;; (lispy-define-key map "v" 'lispy-view)
;; (lispy-define-key map "t" 'lispy-teleport
;;   :override '(cond ((looking-at lispy-outline)
;;                     (end-of-line))))
;; (lispy-define-key map "n" 'lispy-new-copy)
;; (lispy-define-key map "b" 'lispy-back)
;; (lispy-define-key map "B" 'lispy-ediff-regions)
;; (lispy-define-key map "x" 'lispy-x)
;; (lispy-define-key map "Z" 'lispy-edebug-stop)
;; (lispy-define-key map "V" 'lispy-visit)
;; (lispy-define-key map "-" 'lispy-ace-subword)
;; (lispy-define-key map "." 'lispy-repeat)
;; (lispy-define-key map "~" 'lispy-tilde)

;; Goto
;; ("d" lispy-goto)
;; ("l" lispy-goto-local)
;; ("r" lispy-goto-recursive)
;; ("p" lispy-goto-projectile)
;; ("f" lispy-follow)
;; ("b" pop-tag-mark)
;; ("q" lispy-quit)
;; ("j" lispy-goto-def-down)
;; ("a" lispy-goto-def-ace)
;; ("e" lispy-goto-elisp-commands)

;; Other
;; (("h" lispy-move-left)
;;  ("j" lispy-down-slurp)
;;  ("k" lispy-up-slurp)
;;  ("l" lispy-move-right)
;;  ("SPC" lispy-other-space)
;;  ("g" lispy-goto-mode)))

;; Knight
;;   ("j" lispy-knight-down)
;;   ("k" lispy-knight-up)
;;    ("z" nil))

;; *** Structured haskell mode
(straight-use-package 'shm)

(defhydra my/structured-haskell-hydra (:hint nil
					     :color red)
  "haskell"
  ("U" (call-interactively (lambda () (interactive) (insert "undefined"))) nil)

  ;; Also check forward/backward node
  ("l" shm/goto-parent-end nil)
  ("h" shm/goto-parent nil)
  ("RET" (call-interactively #'shm/newline-indent) nil)

  ("N" (call-interactively #'shm/raise) nil)

  ("u" (call-interactively #'undo) nil)

  ("e" (call-interactively #'my/auto-eval) nil)

  ("k" (call-interactively #'shm/yank) nil)
  ("d" (call-interactively #'shm/kill) nil)
  ("D" (call-interactively #'shm/kill-line) nil)

  ("c" (call-interactively #'shm/case-split) nil)

  ("<escape>" nil))

;; *** Keys
(my/evil-visual-define-key "z" 'my/lispy-hydra/body)
(my/evil-normal-define-key "z" 'my/lispy-hydra/body)

;; (defun my/structural-navigation-state ()
;;  (interactive)
;;  (pcase major-mode
;;    ('haskell-mode (my/structured-haskell-hydra/body))
;;    (_ (my/lispy-hydra/body))))

;; (my/evil-visual-define-key "z" 'my/structural-navigation-state)
;; (my/evil-normal-define-key "z" 'my/structural-navigation-state)

;; * Auto functions
;; ** Auto view documentation
(defun my/auto-view-docs ()
  (interactive)
  (pcase major-mode
    ('csharp-mode (omnisharp-current-type-documentation))
    ('haskell-mode
     (if my/haskell-hie-enable
	 (lsp-describe-thing-at-point)
       (call-interactively 'dante-info)))
    (_
     (if lsp-mode
	 (lsp-describe-thing-at-point)
       (call-interactively 'describe-function)))))

(define-key my/leader-map (kbd "d") 'my/auto-view-docs)

;; ** Auto rename
(defun my/auto-rename ()
  (interactive)
  (pcase major-mode
    ('csharp-mode (omnisharp-rename))
    (_ (find-function-at-point))))

(define-key my/leader-map (kbd "R") 'my/auto-rename)

;; ** Auto format
(defun my/auto-format-buffer ()
  (interactive)
  (if (string= evil-state 'visual)
      (my/auto-format-region)
    (pcase major-mode
      ('csharp-mode (omnisharp-code-format-entire-file))
      ('nix-mode (nix-format-buffer))
      ('haskell-mode (haskell-mode-stylish-buffer))
      (_ ()))))

(defun my/auto-format-region ()
  (interactive)
  (pcase major-mode
    ('csharp-mode (omnisharp-code-format-region))
    ('emacs-lisp-mode (elisp-format-region))
    (_ ())))

(define-key my/leader-map (kbd "<") 'my/auto-format-buffer)
(define-key my/leader-map (kbd ">") 'my/auto-format-buffer)

;; ** Auto kill buffer
(defun my/auto-kill-buffer ()
  (interactive)
  (pcase major-mode
    ('mu4e-headers-mode (kill-current-buffer)
			(run-with-timer 1 nil 'mu4e-alert-update-mail-count-modeline))
    ('gnus-summary-mode (gnus-summary-exit))
    ('ediff-mode (call-interactively #'ediff-quit))
    (_
     (let ((clocking (org-clocking-buffer)))
       (if (and clocking (eq (current-buffer) clocking))
	   (read-string "Clock out before killing this buffer.")
	 (kill-current-buffer))))))

;; ** Autotab
;; By default modes like outshine and org-mode redefines TAB. This changes the meaning of tab depending on modes
;; If for example you are in org-mode it checks if org-cycle did anything, if it didn't, then run yas-expand
(defun my/auto-tab ()
  (interactive)
  (pcase major-mode
    ('org-mode (org-cycle))
    ('org-msg-edit-mode (org-cycle))
    (_
     ;; Fixes a bug where if cursor is at heading, the one above gets narrowed
     (call-interactively 'yas-expand))))

;; *** Fix org-mode
(with-eval-after-load 'org
  (add-to-list 'org-tab-first-hook (lambda ()
				     (let ((yas-fallback-behavior 'return-nil))
				       (yas-expand)))))

;; * Wakatime
;; ** Custom wakatime-save mechanism
(with-eval-after-load 'wakatime-mode
  ;; Undefine wakatime-bind-hooks
  (defun wakatime-bind-hooks ())

  (setq my/wakatime-idle-time (* 60 4))
  (setq my/wakatime-report-time (/ my/wakatime-idle-time 2))
  (setq my/wakatime-user-present nil)
  (run-with-idle-timer my/wakatime-idle-time t (lambda () (message (concat "User no longer present at " (current-time-string))) (setq my/wakatime-user-present nil)))
  (add-hook 'post-command-hook (lambda () (when (not my/wakatime-user-present) (message (concat "User present again at: " (current-time-string)))) (setq my/wakatime-user-present t)))
  ;; Make the first report after the idle timer has been run
  (run-with-timer my/wakatime-report-time my/wakatime-report-time
		  (lambda () (when my/wakatime-user-present
			       (message (concat "Reporting to wakatime at: " (current-time-string)))
			       (wakatime-save))))
  (message "custom wakatime save mechanism loaded"))

;; * Macros
;; ** Newer macro manager
(setq my/macro-store nil)
(setq my/macro-last-temp nil)

;; *** Macro funcs
(defun my/macro-find-in-store (name)
  (cdr (seq-find (lambda (a) (string= (car a) name)) my/macro-store)))

;; **** Run
(defun my/macro-run-last (arg)
  (interactive "p")
  (if my/macro-last-temp
      (my/macro-run arg my/macro-last-temp)
    (error "No macros has been run yet")))

(defun my/macro-run-store (arg)
  (interactive "p")
  (let* ((name (completing-read "Run or save last macro: " my/macro-store))
	 (entry (my/macro-find-in-store name)))
    (if entry
	(my/macro-run arg entry)
      (my/macro-save-last name))))

(defun my/macro-run (arg string-macro)
  (setq my/macro-last-temp string-macro)
  (kmacro-call-macro arg nil nil string-macro))

;; **** Start stop
(defun my/macro-record (arg)
  (interactive "p")
  (if (or defining-kbd-macro executing-kbd-macro)
      (progn
	(kmacro-end-macro arg)
	(setq my/macro-last-temp last-kbd-macro))
    (kmacro-start-macro arg)))

;; **** Add
(defun my/macro-save-override-with-last (&optional name)
  (interactive)
  (let ((to-override (or name (completing-read "Override macro with last: " my/macro-store nil t))))
    (my/macro-delete to-override)
    (my/macro-save-last to-override)))

(defun my/macro-save-last (&optional name)
  (interactive)
  (my/macro-save (or name (read-string "Store macro as: ")) my/macro-last-temp))

;; **** Basic operations
(defun my/macro-save (name string-macro)
  (add-to-list 'my/macro-store (cons name string-macro)))

(defun my/macro-delete (name)
  (setq my/macro-store (remove-if
			(lambda (entry)
			  (if (string= name (car entry))
			      t
			    nil))
			my/macro-store)))

;; **** Insert in buffer
(defun my/macro-insert ()
  (interactive)
  (insert
   (concat
    "(defun macro- (arg)
       (interactive \"p\")
       (my/macro-run arg \""
    (my/macro-find-in-store (completing-read "Insert macro: " my/macro-store))
    "\"))")))

;; **** Edit macro
(defun my/macro-edit ()
  (interactive)
  (let* ((input (completing-read "Edit macro: " my/macro-store))
	 (found (my/macro-find-in-store input)))
    (setq my/macro-store (-replace-first (cons input found) (cons input (read-string "Edit macro: " found)) my/macro-store))))

;; *** Keys
(my/evil-normal-define-key "q" 'my/macro-record)
(my/evil-visual-define-key "q" 'my/macro-record)

(my/evil-normal-define-key "Q" 'my/macro-run-last)
(my/evil-visual-define-key "Q" 'my/macro-run-last)

(my/evil-normal-define-key "C-q" 'my/macro-save-override-with-last)
(my/evil-visual-define-key "C-q" 'my/macro-save-override-with-last)

(my/evil-normal-define-key "!" 'my/macro-run-store)
(my/evil-visual-define-key "!" 'my/macro-run-store)

;; (my/evil-normal-define-key "!" 'my/macro-edit)
;; (my/evil-visual-define-key "!" 'my/macro-edit)

;; * Encryption
;; ** GPG
;; *** Pinentry
(straight-use-package 'pinentry)

(if window-system
    (add-hook 'exwm-init-hook 'pinentry-start)
  (pinentry-start))

(setq epg-pinentry-mode 'loopback)

;; *** Reset GPG agent
(defun my/reset-gpg-agent ()
  (interactive)
  (shell-command "gpgconf --kill gpg-agent")
  (pinentry-stop)
  (pinentry-start))

;; ** Passwords
(define-prefix-command 'my/password-map)
(define-key my/leader-map (kbd "p") 'my/password-map)

;; Enable org mode for .org.gpg files
(add-to-list 'auto-mode-alist '("\\.org.gpg\\'" . org-mode))

;; *** Generate password
(defun my/generate-password ()
  (interactive)
  ;; TODO: Move this somewhere else
  (random t) ; Randomize

  (let* ((random-length-list '(14 15 16 17 18 19))
	 (random-length (nth (random (length random-length-list)) random-length-list)))
    (shell-command (concat "pwgen -C -s -y -n -c " (number-to-string random-length)))))

(define-key my/password-map (kbd "g") 'my/generate-password)

;; *** Espy
(straight-use-package 'espy)

(setq espy-password-file "~/pass/pass.org.gpg")

(define-key my/password-map (kbd "r") 'my/reset-gpg-agent)
(define-key my/password-map (kbd "u") 'espy-get-user)
(define-key my/password-map (kbd "p") 'espy-get-pass)

;; *** Auto-clean kill ring
;; (defvar my/pass-in-killring nil)

;; (defun my/ivy-pass ()
;; (interactive)
;; (setq my/pass-in-killring t)
;; (ivy-pass))

;; (defun my/pass-pop-killring ()
;; (if (eq my/pass-in-killring t)
;; (progn
;; (progn (pop kill-ring)
;; (message "Password removed"))
;; (setq my/pass-in-killring nil))))

;; (defun my/pop-killring ()
;;   (pop kill-ring)
;;   (setq my/pass-in-killring nil))

;; (define-key my/leader-map (kbd "C-k") 'my/pop-killring)
;; (advice-add 'evil-goggles--paste-advice :before (lambda () (interactive) (my/pass-pop-killring)))
;; (advice-add 'evil-goggles--paste-advice :before
;; (advice-add 'evil-paste-after :after (lambda (&rest r) (interactive) (my/pass-pop-killring)))
;; (advice-add 'evil-paste-before :after (lambda (&rest r) (interactive) (my/pass-pop-killring)))

;; * Terminal
;; ** Set max lines to a lot
(setq term-buffer-maximum-size 10000)

;; ** Disable line wrapping
(add-hook 'term-mode-hook (lambda () (interactive) (visual-line-mode -1)
			    (setq truncate-lines t)))

;; *** Keys
;; (my/evil-universal-define-key-in-mode 'term-raw-map "C-," 'term-char-mode)

;; * Eshell
;;  https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
;; Change to temporary name before renaming. This has to be unique. If it isn't the buffer with the same name will get its major mode changed to eshell
(setq eshell-buffer-name "*eshell-temp-name*")

(setq-default eshell-status-in-mode-line nil)

(defun my/eshell ()
  (interactive)
  (eshell)
  (my/give-buffer-unique-name "*eshell*"))

;; ** Allow to delete prompt
(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook (lambda () (setq-local inhibit-read-only t))))

;; ** History
(setq eshell-highlight-prompt t)
(setq eshell-hist-ignoredups t)
(setq eshell-history-size 10000)

;; *** Clean history
;; This cryptic code removes all duplicate lines in the eshell history file without sorting
;; The two tac here are here so that if a new command is a duplicate, the new command is kept while the old duplicate is removed
(with-eval-after-load 'em-hist
  (let ((content (shell-command-to-string (concat "tac " eshell-history-file-name " | awk '!seen[$0]++' | tac"))))
    (write-region content nil eshell-history-file-name)))

;; *** Append history
;; https://emacs.stackexchange.com/questions/18564/merge-history-from-multiple-eshells
(setq eshell-save-history-on-exit nil)

(defun my/eshell-append-history ()
  "Call `eshell-write-history' with the `append' parameter set to `t'."
  (when eshell-history-ring
    (let ((newest-cmd-ring (make-ring 1)))
      (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
      (let ((eshell-history-ring newest-cmd-ring))
	(my/append-to-file eshell-history-file-name (concat (car (ring-elements eshell-history-ring)) "\n"))))))

(with-eval-after-load 'eshell
  (add-hook 'eshell-pre-command-hook #'my/eshell-append-history))

(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook (lambda () (interactive) (setq eshell-exit-hook (remove 'eshell-write-history eshell-exit-hook)))))

;; ** Prefer lisp to bash
(setq eshell-prefer-lisp-functions nil)
(setq eshell-prefer-lisp-variables nil)

;; ** Use tramp for sudo
;; (require 'em-tramp)
;; (defalias 'sudo 'eshell/sudo)

;; ** Completion
;; *** Bash-completion
;; https://github.com/szermatt/emacs-bash-completion/issues/24
;; (straight-use-package 'bash-completion)

;; (add-hook 'eshell-mode-hook (lambda () (setq-local bash-completion-nospace t)))

;; (setq eshell-default-completion-function 'eshell-bash-completion)

;; (defun eshell-bash-completion ()
;;   (while (pcomplete-here
;;	  (nth 2 (bash-completion-dynamic-complete-nocomint (save-excursion (eshell-bol) (point)) (point))))))

;; *** Fish-completion
;; Uses bash-completion if fish wasn't found
;; (straight-use-package 'fish-completion)
;; (fish-completion-mode)

;; *** Fix inserting random tabs
;; https://github.com/company-mode/company-mode/issues/409#issuecomment-434820576
;; Also disables command completions
;; (add-hook 'eshell-mode-hook
;;	  (lambda () (setq-local completion-at-point-functions (remove 'pcomplete-completions-at-point completion-at-point-functions))))

;; Commented out what's changed
(with-eval-after-load 'em-cmpl
  (defun eshell-complete-parse-arguments ()
    "Parse the command line arguments for `pcomplete-argument'."
    (when (and eshell-no-completion-during-jobs
	       (eshell-interactive-process))
      ;; (insert-and-inherit "\t")
      (throw 'pcompleted t))
    (let ((end (point-marker))
	  (begin (save-excursion (eshell-bol) (point)))
	  (posns (list t))
	  args delim)
      (when (memq this-command '(pcomplete-expand
				 pcomplete-expand-and-complete))
	(run-hook-with-args 'eshell-expand-input-functions begin end)
	(if (= begin end)
	    (end-of-line))
	(setq end (point-marker)))
      (if (setq delim
		(catch 'eshell-incomplete
		  (ignore
		   (setq args (eshell-parse-arguments begin end)))))
	  (cond ((memq (car delim) '(?\{ ?\<))
		 (setq begin (1+ (cadr delim))
		       args (eshell-parse-arguments begin end)))
		((eq (car delim) ?\()
		 (eshell-complete-lisp-symbol)
		 (throw 'pcompleted t))
		(t
		 ;; (insert-and-inherit "\t")
		 (throw 'pcompleted t))))
      (when (get-text-property (1- end) 'comment)
	;; (insert-and-inherit "\t")
	(throw 'pcompleted t))
      (let ((pos begin))
	(while (< pos end)
	  (if (get-text-property pos 'arg-begin)
	      (nconc posns (list pos)))
	  (setq pos (1+ pos))))
      (setq posns (cdr posns))
      (cl-assert (= (length args) (length posns)))
      (let ((a args)
	    (i 0)
	    l)
	(while a
	  (if (and (consp (car a))
		   (eq (caar a) 'eshell-operator))
	      (setq l i))
	  (setq a (cdr a) i (1+ i)))
	(and l
	     (setq args (nthcdr (1+ l) args)
		   posns (nthcdr (1+ l) posns))))
      (cl-assert (= (length args) (length posns)))
      (when (and args (eq (char-syntax (char-before end)) ? )
		 (not (eq (char-before (1- end)) ?\\)))
	(nconc args (list ""))
	(nconc posns (list (point))))
      (cons (mapcar
	     (function
	      (lambda (arg)
		(let ((val
		       (if (listp arg)
			   (let ((result
				  (eshell-do-eval
				   (list 'eshell-commands arg) t)))
			     (cl-assert (eq (car result) 'quote))
			     (cadr result))
			 arg)))
		  (if (numberp val)
		      (setq val (number-to-string val)))
		  (or val ""))))
	     args)
	    posns))))

;; ** Eldoc
;; *** Eshell-eldoc
;; (straight-use-package '(eshell-eldoc :type git :host github :repo "defaultxr/eshell-eldoc"))
;; (add-hook 'eshell-mode-hook 'eshell-eldoc-enable-for-buffer)

;; *** Eshell-help
(straight-use-package 'esh-help)

(with-eval-after-load 'eshell
  (require 'esh-help)
  (setup-esh-help-eldoc))

;; ** Did you mean
;; (straight-use-package 'eshell-did-you-mean)

;; (require 'eshell-did-you-mean)
;; (eshell-did-you-mean-setup)

;; ** Aliases
(defun eshell/f (file)
  (find-file file))

;; ** Clear
;; Default eshell/clear only spams newlines
(with-eval-after-load 'eshell
  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Probably not needed
      ;;(eshell-send-input)
      )))

;; ** Term
(define-key my/leader-map (kbd "{") 'ansi-term)

;; ** Use ansi term for certain applications
(with-eval-after-load 'eshell
  (require 'em-term)
  (add-to-list 'eshell-visual-commands "vim")
  (add-to-list 'eshell-visual-commands "wifi-menu")
  (add-to-list 'eshell-visual-commands "htop"))

(setq eshell-visual-subcommands
      '(("git" "log" "diff" "show")
	("sudo" "wifi-menu")
	("sudo" "htop")
	("sudo" "vi" "visudo")))

;; ** Remove banner
(setq eshell-banner-message "")

;; ** Custom prompt
(defvar my/eshell-prompt-symbol)
(if window-system
    (setq my/eshell-prompt-symbol "λ")
  (setq my/eshell-prompt-symbol "$"))

(setq eshell-prompt-function
      (lambda ()
	(concat (abbreviate-file-name (eshell/pwd))
		(if (= (user-uid) 0) " # " ;;(concat " " my/eshell-prompt-symbol " ")
		  "\n"
		  ))))

(setq eshell-prompt-regexp
      (concat "^[^#$\n]* [#" my/eshell-prompt-symbol "] "))

;; ** Alert when task is done
(add-hook 'eshell-post-command-hook (lambda ()
				      ;; Fix for eshell-mode boot-up triggering hook
				      (when (or (not eshell-mode) (not (= 2 (line-number-at-pos (point)))))
					(my/alert-process-completed))))

;; ** Custom Goto beg of line
(defun my/eshell-goto-beg-of-line ()
  (interactive)
  (let ((start-point (point)))
    (if (and
	 (re-search-backward my/eshell-prompt-symbol nil t) (= (line-number-at-pos) (line-number-at-pos start-point)))
	(evil-forward-char 2)
      (goto-char start-point)
      (evil-digit-argument-or-evil-beginning-of-line))))

;; ** With-editor
(straight-use-package 'with-editor)

(add-hook 'eshell-mode-hook (lambda ()
			      (with-editor-export-editor)
			      ;; Clear echo area to remove annoying messages. A problem with this is that it also hides error messages
			      (message nil)))

;; ** Keys
(define-key my/leader-map (kbd "[") 'my/eshell)

;; Eshell keys are buffer local... Bind them on startup
(defun my/bind-eshell-keys ()
  (define-prefix-command 'my/eshell-mode-map)
  (evil-define-key 'normal eshell-mode-map (kbd (concat my/leader-map-key " a")) 'my/eshell-mode-map)

  (evil-define-key 'normal eshell-mode-map (kbd "RET") 'eshell-send-input)
  (define-key eshell-mode-map [remap evil-ret] 'eshell-send-input)

  (define-key my/eshell-mode-map (kbd "k") 'counsel-esh-history)

  (evil-define-key 'normal eshell-mode-map (kbd "0") 'my/eshell-goto-beg-of-line)

  (evil-define-key '(normal insert visual replace) eshell-mode-map (kbd "C-c") (lambda () (interactive) (insert "") (call-interactively 'eshell-send-input)))
  ;;(evil-define-key '(normal insert visual replace) eshell-mode-map (kbd "C-x") 'eshell-interrupt-process)
  (evil-define-key '(normal insert visual replace) eshell-mode-map (kbd "C-z") 'eshell-kill-process)

  (evil-define-key '(normal insert) eshell-mode-map (kbd "C-p") 'eshell-previous-matching-input-from-input)
  (evil-define-key '(normal insert) eshell-mode-map (kbd "C-n") 'eshell-next-matching-input-from-input)

  (evil-define-key '(normal insert) eshell-mode-map (kbd "TAB") 'completion-at-point))

(add-hook 'eshell-mode-hook 'my/bind-eshell-keys)

;; * Keyboard layouts
;; ** Carpalx
(defun my/carpalx-enable ()
  (interactive)
  (async-shell-command (concat "setxkbmap -I ~/.emacs.d/configs/kbd-layouts/ carpalx.xkb -print | xkbcomp -I"(expand-file-name user-emacs-directory)"configs/kbd-layouts/ - $DISPLAY")))

(when my/carpalx-enable
  (my/carpalx-enable))

;; * Keys
;; ** Key rebinds
;; (require 'evil-maps)

;; *** General
(eval-and-compile
  (straight-use-package 'general)
  (general-evil-setup))

;; *** Language specific symbols
;; **** Lower
(define-key input-decode-map (kbd "M-p") (kbd "å"))
(define-key input-decode-map (kbd "M-,") (kbd "ä"))
(define-key input-decode-map (kbd "M-.") (kbd "ö"))

;; **** Capital
(define-key input-decode-map (kbd "M-P") (kbd "Å"))
(define-key input-decode-map (kbd "M-<") (kbd "Ä"))
(define-key input-decode-map (kbd "M->") (kbd "Ö"))

;; *** Rebind backspace with C-f
;; (keyboard-translate ?\C-f ?\C-?)

;; 127 is backspace
(define-key input-decode-map (kbd "C-f") [127])
;; There are 2 unbinds here for compatibility
(define-key input-decode-map (kbd "<backspace>") (kbd "C-="))

;; Don't split up tabs on delete
;; (global-set-key (kbd "DEL") 'backward-delete-char)

;; *** Rebind delete with
;; (keyboard-translate ?\C-l 'delete)
(define-key input-decode-map (kbd "C-l") (kbd "<deletechar>"))
;; There are 2 unbinds here for compatibility
(define-key input-decode-map (kbd "<deletechar>") (kbd "C-="))
(define-key input-decode-map (kbd "<delete>") (kbd "C-="))

;; *** k(Move up) <--> p(Paste)
;; **** k
(my/evil-normal-define-key "k" 'evil-paste-after)
(my/evil-normal-define-key "K" 'evil-paste-before)

(define-key evil-visual-state-map "k" 'evil-visual-paste)

;; Universal paste key
(global-set-key (kbd "C-k") 'evil-paste-after)
(global-set-key (kbd "C-K") 'evil-paste-before)
(define-key evil-insert-state-map (kbd "C-k") 'evil-paste-after)
(define-key evil-insert-state-map (kbd "C-K") 'evil-paste-before)

(define-key evil-window-map "k" 'evil-window-mru)

;; **** p
(define-key evil-window-map "p" 'evil-window-up)
(define-key evil-window-map "P" 'evil-window-move-very-top)

(my/evil-normal-define-key "p" 'evil-previous-line)

;; Rebind to make consistent with N
(my/evil-normal-define-key "P" 'delete-indentation)

(define-key evil-window-map (kbd "C-S-p") 'evil-window-move-very-top)

;; *** n(search-next) <--> j(Move up)
;; **** n
(my/evil-normal-define-key "n" 'evil-next-line)

(my/evil-normal-define-key "N" 'evil-join)

;; ex
;;  (evil-ex-define-cmd "j[oin]" 'evil-ex-join)
;;  (evil-ex-define-cmd "ju[mps]" 'evil-show-jumps)

(define-key evil-window-map (kbd "C-S-n") 'evil-window-move-very-bottom)

;; **** j
;; (my/evil-normal-define-key "j" 'evil-search-next)
;; (my/evil-normal-define-key "J" 'evil-search-previous)

;; (my/evil-normal-define-key "j" 'isearch-repeat-forward)
;; (my/evil-normal-define-key "J" 'isearch-repeat-backward)

;; ex
;; (evil-ex-define-cmd "new" 'evil-window-new)
;; (evil-ex-define-cmd "norm[al]" 'evil-ex-normal)
;; (evil-ex-define-cmd "noh[lsearch]" 'evil-ex-nohighlight)

(my/evil-normal-define-key "gj" 'evil-next-match)
(my/evil-normal-define-key "gJ" 'evil-previous-match)

;; *** Rebind search key
(my/evil-normal-define-key "s" 'my/isearch-forward-regexp)
(my/evil-normal-define-key "S" 'my/isearch-backward-regexp)

(my/evil-visual-define-key "s" 'my/isearch-forward-regexp)
(my/evil-visual-define-key "S" 'my/isearch-backward-regexp)

(defun my/isearch-repeat-forward-with-cleanup ()
  (interactive)
  (call-interactively 'isearch-repeat-forward)
  ;;(run-with-idle-timer 1 nil 'lazy-highlight-cleanup)
  (run-with-idle-timer 1 nil 'isearch-done))

(defun my/isearch-repeat-backward-with-cleanup ()
  (interactive)
  (call-interactively 'isearch-repeat-backward)
  (run-with-idle-timer 1 nil 'isearch-done))

(my/evil-normal-define-key "g n" 'my/isearch-repeat-forward-with-cleanup)
(my/evil-normal-define-key "g p" 'my/isearch-repeat-backward-with-cleanup)

(my/evil-visual-define-key "g n" 'my/isearch-repeat-forward-with-cleanup)
(my/evil-visual-define-key "g p" 'my/isearch-repeat-backward-with-cleanup)

;; (my/evil-normal-define-key "/" 'evil-substitute)
;; (my/evil-normal-define-key "?" 'evil-change-whole-line)

;; (define-key evil-visual-state-map "/" 'evil-substitute)
;; (define-key evil-visual-state-map "?" 'evil-change-whole-line)

;; *** Rebind save key
(general-simulate-key "C-x C-s")

(defun my/save-buffer ()
  (interactive)
  ;; (my/backup-buffer-per-session)
  ;; (my/backup-original-buffer)
  (general-simulate-C-x_C-s))

(define-key my/leader-map (kbd "s") 'my/save-buffer)
(define-key my/leader-map (kbd "C-s") 'write-file)

;; *** Rebind C-d
(my/evil-normal-define-key "C-d" nil)

;; *** Rebind esc
(define-key input-decode-map (kbd "<escape>") (kbd "C-e"))
(define-key input-decode-map (kbd "C-e") (kbd "<escape>"))
;; (keyboard-translate ?\C-e ?\C-\[)

;; *** Rebind enter
(define-key input-decode-map (kbd "RET") (kbd "C-a"))
(define-key input-decode-map (kbd "C-a") (kbd "RET"))
;; (keyboard-translate ?\C-a ?\C-m)

;; *** Rebind tab
;; (define-key my/keys-mode-map (kbd "C-e") 'my/simulate-esc)
;; (define-key input-decode-map (kbd "?\\t") (kbd "C-="))

;; If window system, unbind tab key and not C-=

(define-key input-decode-map (kbd "TAB") (kbd "C-="))
(define-key input-decode-map (kbd "<tab>") (kbd "C-="))
(define-key input-decode-map (kbd "C-t") (kbd "TAB"))
(define-key input-decode-map (kbd "M-C-t") (kbd "C-TAB"))
;; This doesn't work here because you can't cross-bind like above. All keys involved would point to the same output key
;; (keyboard-translate ?\C-t ?\C-i)

;; (when window-system
;;  (define-key input-decode-map (kbd "TAB") (kbd "C--"))
;;  (define-key input-decode-map (kbd "<tab>") (kbd "C--"))

;;  (define-key input-decode-map (kbd "C-i") (kbd "C-~")))

;; *** Disable backspace
;; (define-key input-decode-map (kbd "C-e") (kbd "TAB"))
;; (define-key input-decode-map (kbd "M-C-i") (kbd "C-TAB"))

;; *** Rebind number row
;; **** Numbers
;; ***** Disable number row
;; (define-key input-decode-map (kbd "C-[") [control-bracketleft])

(define-key input-decode-map (kbd "1") (kbd "C-="))
(define-key input-decode-map (kbd "2") (kbd "C-="))
(define-key input-decode-map (kbd "3") (kbd "C-="))
(define-key input-decode-map (kbd "4") (kbd "C-="))
(define-key input-decode-map (kbd "5") (kbd "C-="))
(define-key input-decode-map (kbd "6") (kbd "C-="))
(define-key input-decode-map (kbd "7") (kbd "C-="))
(define-key input-decode-map (kbd "8") (kbd "C-="))
(define-key input-decode-map (kbd "9") (kbd "C-="))
(define-key input-decode-map (kbd "0") (kbd "C-="))

;; ***** Set new number row
(define-key input-decode-map (kbd "M-d") (kbd "1"))
(define-key input-decode-map (kbd "M-s") (kbd "2"))
(define-key input-decode-map (kbd "M-t") (kbd "3"))
(define-key input-decode-map (kbd "M-n") (kbd "4"))
(define-key input-decode-map (kbd "M-r") (kbd "5"))
(define-key input-decode-map (kbd "M-i") (kbd "6"))
(define-key input-decode-map (kbd "M-a") (kbd "7"))
(define-key input-decode-map (kbd "M-e") (kbd "8"))
(define-key input-decode-map (kbd "M-o") (kbd "9"))
(define-key input-decode-map (kbd "M-h") (kbd "0"))

;; **** Symbols
;; ***** Disable symbol keys
(define-key input-decode-map (kbd "!") (kbd "C-="))
(define-key input-decode-map (kbd "@") (kbd "C-="))
(define-key input-decode-map (kbd "#") (kbd "C-="))
(define-key input-decode-map (kbd "$") (kbd "C-="))
(define-key input-decode-map (kbd "%") (kbd "C-="))
(define-key input-decode-map (kbd "^") (kbd "C-="))
(define-key input-decode-map (kbd "&") (kbd "C-="))
(define-key input-decode-map (kbd "*") (kbd "C-="))
(define-key input-decode-map (kbd "(") (kbd "C-="))
(define-key input-decode-map (kbd ")") (kbd "C-="))

;; ***** Set new keys
(define-key input-decode-map (kbd "M-q") (kbd "!"))
(define-key input-decode-map (kbd "M-g") (kbd "@"))
(define-key input-decode-map (kbd "M-m") (kbd "#"))
(define-key input-decode-map (kbd "M-l") (kbd "$"))
(define-key input-decode-map (kbd "M-w") (kbd "%"))
(define-key input-decode-map (kbd "M-;") (kbd "^"))
(define-key input-decode-map (kbd "M-b") (kbd "&"))
(define-key input-decode-map (kbd "M-y") (kbd "*"))
(define-key input-decode-map (kbd "M-f") (kbd "("))
(define-key input-decode-map (kbd "M-u") (kbd ")"))

;; * nix
;; ** Direnv
(eval-and-compile
  (straight-use-package 'envrc)
  (require 'envrc))

;; Needs to be enabled as late as possible
(add-hook 'after-init-hook 'envrc-global-mode)

(add-hook 'after-save-hook (lambda ()
			     (when (and (string= major-mode "nix-mode") (string= envrc--status 'on))
			       (run-with-timer 5 nil 'envrc-reload))))

;; (straight-use-package 'direnv)
;; (direnv-mode)

;; ** Nix-mode
(straight-use-package 'nix-mode)

;; ** Nix-options
;; *** Company
(straight-use-package 'company-nixos-options)

;; Run nixos-options on a hook because otherwise it's run on every startup which means increased startup time even if you aren't going to edit nix files
(add-hook 'nix-mode-hook (lambda () (interactive)
			   (setq-local company-backends (pushnew 'company-nixos-options company-backends))))

;; *** Ivy
(defun my/nixos-options-ivy ()
  (interactive)
  (require 'nixos-options)
  (switch-to-buffer
   (nixos-options-doc-buffer
    (nixos-options-get-documentation-for-option
     (nixos-options-get-option-by-name
      (completing-read "nix-options: " nixos-options))))))

;; ** Pretty sha paths
;;  (straight-use-package 'pretty-sha-path)

;;  (add-hook 'eshell-mode-hook 'pretty-sha-path-mode)

;; * Xinput
(defun my/get-xinput-device-ID (search-term)
  (let* ((xinput-list (shell-command-to-string "xinput --list"))
	 (correct-line-pos (string-match-p search-term xinput-list))
	 (id-pos (string-match-p "id=" xinput-list correct-line-pos))
	 (number-pos-beg (string-match-p "[[:digit:]]" xinput-list id-pos))
	 (number-pos-end (string-match-p "[[:blank:]]" xinput-list (+ 1 number-pos-beg)))
	 (num (string-to-number (substring-no-properties xinput-list number-pos-beg number-pos-end))))
    num))

(defun my/xinput-is-device-enabled (device-id)
  (let* ((xinput-list (shell-command-to-string (concat "xinput list-props " (number-to-string device-id))))
	 (enabled-pos (string-match-p "Device Enabled" xinput-list))
	 (colon-pos (string-match-p "\:[[:blank:]]*" xinput-list enabled-pos))
	 (num-pos-beg (string-match-p "[[:digit:]]" xinput-list colon-pos))
	 (num-pos-end (string-match-p "$" xinput-list num-pos-beg))
	 (num (string-to-number (substring-no-properties xinput-list num-pos-beg num-pos-end))))
    (if (= num 1)
	t
      nil)))

(defun my/xinput-toggle-device (device-id)
  (if (my/xinput-is-device-enabled device-id)
      (my/xinput-disable-device device-id)
    (my/xinput-enable-device device-id)))

(defun my/xinput-enable-device (device-id)
  (shell-command (concat "xinput --enable " (number-to-string device-id))))

(defun my/xinput-disable-device (device-id)
  (shell-command (concat "xinput --disable " (number-to-string device-id))))

;; ** Touchpad
(defun my/xinput-toggle-touchpad ()
  (interactive)
  (my/xinput-toggle-device (my/get-xinput-device-ID "Touchpad")))

(defun my/xinput-enable-touchpad ()
  (interactive)
  (my/xinput-enable-device (my/get-xinput-device-ID "Touchpad")))

(defun my/xinput-disable-touchpad ()
  (interactive)
  (my/xinput-disable-device (my/get-xinput-device-ID "Touchpad")))

;; * Shr
;; ** Fix background colors shr
;; Try fixing colors
;; (setq shr-color-visible-luminance-min 80)
;; (setq shr-color-visible-distance-min 5)

;; Fully disables colors
;; (with-eval-after-load 'shr
;;   (advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore))))

;; ** Auto-open image at point
;; ;; Redefine function to attempt to open image if link at point wasn't found
(with-eval-after-load 'shr
  (defun shr-browse-url (&optional external mouse-event)
    "Browse the URL at point using `browse-url'.
    If EXTERNAL is non-nil (interactively, the prefix argument), browse
    the URL using `shr-external-browser'.
    If this function is invoked by a mouse click, it will browse the URL
    at the position of the click.  Optional argument MOUSE-EVENT describes
    the mouse click event."
    (interactive (list current-prefix-arg last-nonmenu-event))
    (mouse-set-point mouse-event)
    (let ((url (get-text-property (point) 'shr-url)))
      (cond
       ((not url)
	;; Was unsuccessful in opening link, attempt to open image
	(shr-browse-image))
       ((string-match "^mailto:" url)
	(browse-url-mail url))
       (t
	(if external
	    (funcall shr-external-browser url)
	  (browse-url url)))))))

;; ** Shrface
;; (straight-use-package '(shrface :type git :host github :repo "chenyanming/shrface"))
;;(require 'shrface)

;; * RSS reader
(straight-use-package 'elfeed)

(setq elfeed-search-filter "+blog")
(setq elfeed-db-directory (concat user-emacs-directory ".cache/elfeed"))

(setq elfeed-search-mode-hook nil)
(add-hook 'elfeed-search-mode-hook (lambda ()
				      (run-with-timer nil nil (lambda ()
								(toggle-truncate-lines 1)))))

;; ** Elfeed org
(straight-use-package 'elfeed-org)

(with-eval-after-load 'elfeed
  (elfeed-org))

(setq rmh-elfeed-org-files '("~/Notes/Documentation/Emacs/Elfeed/Feeds.org"))

;; ** Feed discovery
(straight-use-package '(feed-discovery :type git :host github :repo "HKey/feed-discovery"))

;; * Browser
;; ** Text browser stuff
(defun my/launch-text-browser (&optional str)
  (interactive)
  (let ((url (my/get-search-url str)))
    (if my/use-w3m
	(w3m url t)
      (eww-browse-url url t))))

(define-key my/leader-map (kbd "b") 'my/launch-text-browser)

(defun my/text-browser-define-at-point ()
  (interactive)
  (my/launch-text-browser
   (concat "define " (substring-no-properties (thing-at-point 'word)) t)))

(define-key my/leader-map (kbd "B") 'my/text-browser-define-at-point)

;; *** Get addr
(defun my/get-search-url (&optional str)
  (interactive)
  (let ((search (or str (completing-read "search: " nil))))
    ;; Don't do a google search for anything that has a dot then a letter
    ;; There are two (not whitespace) here because otherwise the * wildcard would accept strings without any char after a dot
    (if (or
	 ;; All strings spaces are searches
	 (string-match-p (rx whitespace) search)

	 (not
	  (or
	   ;; All strings without spaces and with dots aren't searches
	   ;; The not whitespace here combined with the * and oel makes sure cases like "foo.bar." aren't interpreted as direct addresses
	   (string-match-p (rx (char ".") (* graph) eol) search)

	   ;; All strings without spaces and with http aren't searches
	   (string-match-p (rx bol "http" (* graph) eol) search)
	   )))

	(concat "https://www.google.com/search?q=" search)
      search)))

;; *** Set default browser
(if my/use-w3m
    (setq-default browse-url-browser-function 'w3m-browse-url)
  (setq-default browse-url-browser-function 'eww-browse-url))

;; ** w3m
(straight-use-package 'w3m)

(with-eval-after-load 'w3m
  (w3m-display-mode 'plain))

(setq w3m-use-title-buffer-name t)

(setq w3m-session-crash-recovery nil)

(setq w3m-search-word-at-point nil)

;; *** Images
;; Make images load instantly
(setq w3m-default-display-inline-images t)
(setq w3m-idle-images-show-interval 0)

;; *** Switch w3m buffer
(defun my/switch-w3m-buffer ()
  "Switch w3m buffer"
  (interactive)
  (setq this-command #'my/switch-w3m-buffer)
  (ivy-read "Switch to buffer: " #'internal-complete-buffer
	    :keymap ivy-switch-buffer-map
	    :preselect (buffer-name (other-buffer (current-buffer)))
	    :action #'ivy--switch-buffer-action
	    :matcher #'ivy--switch-buffer-matcher
	    :caller 'ivy-switch-buffer
	    :initial-input "*w3m*"))

;; *** Keys
(evil-define-key 'normal w3m-mode-map (kbd "RET") 'w3m-view-this-url)
(evil-define-key 'normal w3m-mode-map (kbd "o") 'w3m-search)
(evil-define-key 'insert w3m-mode-map (kbd "q") 'undefined)

(evil-define-key 'normal w3m-mode-map (kbd "u") 'w3m-history)
(evil-define-key 'normal w3m-mode-map (kbd "U") 'w3m-db-history)

;; ** Eww/shr
;; *** Disable header line
(with-eval-after-load 'eww
  (defun eww-update-header-line-format () nil))

;; *** Add URL to buffer name
(with-eval-after-load 'eww
  (add-hook 'eww-after-render-hook (lambda () (interactive) (my/give-buffer-unique-name (concat "eww - " (plist-get eww-data :title))))))

;; *** Keys
;; (define-key eww-mode-map [?\d] 'eww-back-url)
;; (evil-define-key 'normal eww-mode-map [?\d] 'eww-back-url)
;; (evil-define-key 'visual eww-mode-map [?\d] 'eww-back-url)

(evil-define-key 'normal eww-mode-map (kbd "w") 'evil-forward-word-begin)
(setq shr-map (make-sparse-keymap))

(evil-define-key 'normal eww-mode-map (kbd "H") 'eww-back-url)
(evil-define-key 'normal eww-mode-map (kbd "L") 'eww-forward-url)

(define-prefix-command 'my/eww-mode-map)
(evil-define-key 'normal eww-mode-map (kbd (concat my/leader-map-key " a")) 'my/eww-mode-map)

(define-key my/eww-mode-map (kbd "d") 'eww-download)
(define-key my/eww-mode-map (kbd "h") 'eww-history-browse)
(define-key my/eww-mode-map (kbd "o") 'eww-open-in-new-buffer)
(define-key my/eww-mode-map (kbd "r") 'eww-reload)
(define-key my/eww-mode-map (kbd "f") 'eww-open-file)
;; (define-key my/eww-mode-map (kbd "C-c") 'my/eww-toggle-code-highlighting)

;; ** Firefox exwm integration
(with-eval-after-load 'exwm
  (progn
    (straight-use-package '(exwm-firefox-core :type git :host github :repo "walseb/exwm-firefox-core"))
    (straight-use-package '(exwm-firefox-evil :type git :host github :repo "walseb/exwm-firefox-evil"))
    ;;    (straight-use-package 'exwm-firefox-core)
    ;;    (straight-use-package 'exwm-firefox-evil)
    (require 'exwm-firefox-evil)

    ;; Run firefox buffers in normal mode
    (add-hook 'exwm-firefox-evil-mode-hook 'exwm-firefox-evil-normal)))

;; Auto enable exwm-firefox-evil-mode on all firefox buffers
(add-hook 'exwm-manage-finish-hook (lambda ()
				     (evil-emacs-state)
				     (exwm-firefox-evil-activate-if-firefox)))


(setq exwm-firefox-core-search-bookmarks '(("google.com")
					   ("youtube.com")
					   ("github.com")
					   ("gmail.com")))

(setq exwm-firefox-evil-link-hint-end-key nil)

;; *** Tabs
;; http://doc.rix.si/cce/cce-browsers.html
(defun my/browser-activate-tabs-cb (dbus ivy-hash choice)
  (require 'dbus)
  (funcall dbus "Activate" :int32 (truncate (string-to-number (gethash choice ivy-hash)))))

(defun my/browser-activate-tab ()
  "Activate a browser tab using Ivy. Requires plasma-browser integration"
  (interactive)
  (require 'dbus)
  (let ((ivy-hash (make-hash-table :test 'equal))
	(dbus (apply-partially 'dbus-call-method :session
			       "org.kde.plasma.browser_integration" "/TabsRunner"
			       "org.kde.plasma.browser_integration.TabsRunner")))
    (let ((cb (-partial #'my/browser-activate-tabs-cb dbus ivy-hash))
	  (res (funcall dbus "GetTabs")))
      (mapc
       (lambda (obj)
	 (let ((id (number-to-string (car (car (alist-get "id" (car obj) nil nil #'equal)))))
	       (title (car (car (alist-get "title" (car obj) nil nil #'equal)))))
	   (puthash title id ivy-hash)))
       res)
      (ivy-read "Activate tab: " ivy-hash :action cb))))

;; *** Keys
(with-eval-after-load 'exwm
  (progn
       ;;; Normal
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "p") 'exwm-firefox-core-up)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "n") 'exwm-firefox-core-down)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-p") 'exwm-firefox-core-up)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-n") 'exwm-firefox-core-down)

    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "l") 'exwm-firefox-core-right)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "h") 'exwm-firefox-core-left)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "<deletechar>") 'exwm-firefox-core-right)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-h") 'exwm-firefox-core-left)

    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-w") 'exwm-firefox-core-half-page-down)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "e") 'my/browser-activate-tab)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "N") 'exwm-firefox-core-tab-next)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "P") 'exwm-firefox-core-tab-previous)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "k") 'exwm-firefox-core-tab-close)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "K") 'exwm-firefox-core-tab-close)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "g n") 'exwm-firefox-core-find-next)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "g p") 'exwm-firefox-core-find-previous)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-s") 'exwm-firefox-core-quick-find)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-y") 'exwm-firefox-core-copy)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-k") 'exwm-firefox-core-paste)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "t") 'exwm-firefox-core-tab-new)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "O") 'exwm-firefox-core-tab-new)

    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "s") 'exwm-firefox-core-find)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "S") 'exwm-firefox-core-find)

    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "<prior>") 'exwm-firefox-core-page-up)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "<next>") 'exwm-firefox-core-page-down)

    ;; (evil-define-key '(normal motion visual insert) exwm-firefox-evil-mode-map (kbd "C-n") 'exwm-firefox-core-find-next)
    ;; (evil-define-key '(normal motion visual insert) exwm-firefox-evil-mode-map (kbd "C-p") 'exwm-firefox-core-find-previous)

    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "f") 'exwm-firefox-evil-link-hint)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "F") 'exwm-firefox-evil-link-hint-new-tab)

       ;;; Visual
    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "p") 'exwm-firefox-core-up-select)
    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "n") 'exwm-firefox-core-down-select)

    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "C-w") 'exwm-firefox-core-half-page-down-select)

    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "j") 'exwm-firefox-core-find-next)
    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "J") 'exwm-firefox-core-find-previous)

    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "C-y") 'exwm-firefox-core-copy)
    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "C-k") 'exwm-firefox-core-paste)

       ;;; Insert
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-u") (lambda () (interactive) (exwm-firefox-evil-normal) (exwm-firefox-core-half-page-up)))
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-w") (lambda () (interactive) (exwm-firefox-evil-normal) (exwm-firefox-core-half-page-down)))
    ;;
    ;;    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-f") (lambda () (interactive) (exwm-input--fake-key "å")))
    ;;    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-u") (lambda () (interactive) (exwm-input--fake-key "ä")))
    ;;    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-b") (lambda () (interactive) (exwm-input--fake-key "ö")))
    ;;
    ;;    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-F") (lambda () (interactive) (exwm-input--fake-key "Å")))
    ;;    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-U") (lambda () (interactive) (exwm-input--fake-key "Ä")))
    ;;    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-B") (lambda () (interactive) (exwm-input--fake-key "Ö")))

    ;;    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-,") (lambda () (interactive) (exwm-input--fake-key ?ä)))
    ;;    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "ä") (lambda () (interactive) (exwm-input--fake-key ?ä)))

    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-y") 'exwm-firefox-core-copy)
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-k") 'exwm-firefox-core-paste)))
;; (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-l") (lambda () (interactive) (exwm-input--fake-key 'delete)))
;; (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "DEL") (lambda () (interactive) (exwm-input--fake-key 'backspace)))))

;; * Version control
;; ** Ediff
(setq-default ediff-forward-word-function 'forward-char)

;; Stops ediff from creating a new frame dedicated to the control panel
;; This also fixes exwm from crashing or something
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq ediff-split-window-function 'split-window-horizontally)

;; *** A and B to Ancestor
(with-eval-after-load 'ediff
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
		     (concat
		      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
		      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer)))))

;; *** Ediff-dired
;; https://oremacs.com/2017/03/18/dired-ediff/
(defun my/ediff-dired ()
  (interactive)
  (require 'ediff)
  (let ((files (dired-get-marked-files))
	(wnd (current-window-configuration)))
    (if (<= (length files) 2)
	(let ((file1 (car files))
	      (file2 (if (cdr files)
			 (cadr files)
		       (read-file-name
			"file: "
			(dired-dwim-target-directory)))))
	  (if (file-newer-than-file-p file1 file2)
	      (ediff-files file2 file1)
	    (ediff-files file1 file2)))
      (error "no more than 2 files should be marked"))))

;; *** Hydra
(defhydra hydra-ediff (:color blue
			      :hint nil
			      :pre (setq hydra-hint-display-type 'posframe)
			      :post (setq hydra-hint-display-type 'message))
  "
    ^Buffers           Files           VC                     Ediff regions
    ----------------------------------------------------------------------
    _b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
    _B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
    _c_urrent file
    "
  ("b" ediff-buffers)
  ("B" ediff-buffers3)
  ("=" ediff-files)
  ("f" ediff-files)
  ("F" ediff-files3)
  ("c" ediff-current-file)
  ("r" ediff-revision)
  ("l" ediff-regions-linewise)
  ("w" ediff-regions-wordwise))

(define-key my/leader-map (kbd "D") 'hydra-ediff/body)

;; *** Counsel ediff control
;; Only shows ediff control buffers
(defun my/counsel-switch-buffer-ediff ()
  (interactive)
  (ivy-read "Switch to buffer: " 'internal-complete-buffer
	    :preselect (buffer-name (other-buffer (current-buffer)))
	    :keymap ivy-switch-buffer-map
	    :action #'ivy--switch-buffer-action
	    :matcher #'ivy--switch-buffer-matcher
	    :caller 'counsel-switch-buffer
	    :unwind #'counsel--switch-buffer-unwind
	    :update-fn 'counsel--switch-buffer-update-fn
	    :initial-input "\*Ediff  Control  Panel"))

;; *** Keys
(define-prefix-command 'my/ediff-mode-map)
(evil-define-key 'normal ediff-mode-map (kbd (concat my/leader-map-key " a")) 'my/ediff-mode-map)

;; ** Projectile
(straight-use-package 'projectile)

;; *** Remove nix project detection
;; It's pretty poorly implemented so just remove it
(with-eval-after-load 'projectile
  (delete '(nix marker-files ("default.nix") project-file "default.nix" compilation-dir nil configure-command nil compile-command #17="nix-build" test-command #17# run-command nil)
	  projectile-project-types))

;; *** Set completion system
(setq projectile-completion-system 'ivy)

;; *** Find file in project
(defun my/projectile-find-file (target-name)
  (let ((file (seq-find
	       (lambda (name)
		 (string-match (concat target-name "$") name))
	       (projectile-current-project-files))))
    (when file
      (projectile-expand-root file))))

;; *** Open projects in projects folder
(defun my/open-project ()
  (interactive)
  (find-file
   (concat "~/Projects/"
	   (completing-read "Project: "
			    (directory-files "~/Projects/" nil directory-files-no-dot-files-regexp))))
  (projectile-find-file))

;; *** Auto compile project
(defun my/auto-compile-project ()
  (interactive)
  (pcase major-mode
    ('org-mode (counsel-M-x "^org to "))
    ('plantuml-mode (plantuml-preview-buffer 0))
    ('java-mode (call-interactively 'dap-java-debug))
    ('nix-mode
     (if (file-in-directory-p (buffer-file-name) "/etc/nixos/")
	 (progn
	   ;; Not sure why but this is required
	   (require 'ivy)

	   ;; Run rebuild as sudo
	   (let ((default-directory (concat "/sudo::" default-directory)))
	     (compile "nix-channel --update; nixos-rebuild switch")))
       (if (file-in-directory-p (buffer-file-name) "~/.config/nixpkgs/")
	   (compile "nix-channel --update; home-manager switch"))))
    (_
     (pcase (projectile-project-type)
       ('haskell-cabal (my/cabal-compile))
       ;; Placeholder
       ('haskell-stack (my/cabal-compile))
       (_
	;; Reflex obelisk
	(if (seq-contains (directory-files (projectile-project-root)) "frontend" 'string=)
	    (my/haskell-reflex-compile)))))))

;; ** Counsel projectile
;; If enabled it auto enables projectile, which has high CPU usage
(straight-use-package 'counsel-projectile)

(projectile-mode 1)
(counsel-projectile-mode 1)

;; *** Disable mode line
(defun projectile-update-mode-line()
  ())

;; ** Diff
;; Puts + and - in the diff buffer on the fringe so that text from there can be more easily copied.
;; Only works on emacs 27
(setq diff-font-lock-prettify t)

;; ** Magit
(straight-use-package 'magit)

(setq git-commit-summary-max-length 50)

;; *** Performance
;; Disable magit in commit view, makes it possible to do large commits
(setq magit-commit-show-diff nil)

;; *** Diff
;; (require 'magit-diff)
(setq-default magit-diff-refine-hunk 'all)
;; (setq-default magit-diff-refine-ignore-whitespace nil)

;; *** Forge
(straight-use-package '(forge :type git :host github :repo "magit/forge"))

;; *** Todos
;; (straight-use-package 'magit-todos)
;; (with-eval-after-load 'magit
;;   (require 'magit-todos)
;;   (magit-todos-mode 1))

;; ;; Same as default but removed the last colon
;; (setq magit-todos-keyword-suffix (rx (optional "(" (1+ (not (any ")"))) ")")))

;; *** Keys
(with-eval-after-load 'magit
  (evil-define-key '(normal motion) magit-mode-map (kbd "0") 'magit-diff-default-context)
  (evil-define-key '(normal motion) magit-mode-map (kbd "1") #'magit-section-show-level-1)
  (evil-define-key '(normal motion) magit-mode-map  (kbd "2") #'magit-section-show-level-2)
  (evil-define-key '(normal motion) magit-mode-map  (kbd "3") #'magit-section-show-level-3)
  (evil-define-key '(normal motion) magit-mode-map  (kbd "4") #'magit-section-show-level-4))

;; Can't unbind "s"?
;; (my/evil-normal-define-key-in-mode magit-mode-map  "s" 'isearch-forward)
;; (define-key magit-status-mode-map "s" 'isearch-forward)
;; (my/evil-normal-define-key-in-mode magit-status-mode-map  "s" 'isearch-forward)
;; (my/evil-normal-define-key-in-mode magit-untracked-section-map  "s" 'isearch-forward)

;; ** diff-hl
(straight-use-package '(diff-hl :type git :host github :repo "walseb/diff-hl"))

(setq diff-hl-side 'right)

(add-hook 'after-init-hook (lambda ()
			     (require 'diff-hl)
			     (global-diff-hl-mode)))

;; If there is no fringe (terminal), use margin instead
(with-eval-after-load 'diff-hl
  (unless (display-graphic-p) (diff-hl-margin-mode)))

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(with-eval-after-load 'magit
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(setq diff-hl-draw-borders nil)

;; *** Ignore major modes
;; TODO: Keep here while image-mode bug exists
(with-eval-after-load 'diff-hl
  (add-to-list 'diff-hl-global-modes 'minibuffer-inactive-mode t))

;; ** Keys
(with-eval-after-load 'magit
  (evil-define-key 'insert magit-mode-map (kbd "a") 'evil-append))

(define-prefix-command 'my/vc-map)
(define-key my/leader-map (kbd "v") 'my/vc-map)

(define-key my/vc-map (kbd "n") 'diff-hl-next-hunk)
(define-key my/vc-map (kbd "p") 'diff-hl-previous-hunk)

(define-key my/vc-map (kbd "k") 'diff-hl-revert-hunk)
(define-key my/vc-map (kbd "=") 'diff-hl-diff-goto-hunk)

(define-key my/vc-map (kbd "s") 'counsel-git-grep)
;; (define-key my/vc-map (kbd "s") 'counsel-projectile-ag)
(define-key my/vc-map (kbd "d") 'projectile-dired)
(define-key my/vc-map (kbd "D") 'counsel-projectile-find-dir)

(define-key my/vc-map (kbd "K") 'projectile-kill-buffers)
(define-key my/vc-map (kbd "f") 'counsel-projectile-switch-to-buffer)
(define-key my/vc-map (kbd "F") 'projectile-ibuffer)

(define-key my/vc-map (kbd "O") 'projectile-save-project-buffers)
(define-key my/vc-map (kbd "e") 'my/auto-compile-project)

(define-key my/vc-map (kbd "!") 'projectile-run-shell-command-in-root)
(define-key my/vc-map (kbd "&") 'projectile-run-async-shell-command-in-root)

(define-key my/vc-map (kbd "o") 'magit-status)

;; (define-key my/vc-map (kbd "a") 'counsel-projectile-switch-to-buffer)
;; (define-key my/vc-map (kbd "A") 'counsel-projectile-switch-project)

;; (define-key my/vc-map (kbd "f") 'counsel-projectile-find-file)
;; (define-key my/vc-map (kbd "F") 'counsel-projectile-ag)

;; * Media
;; ** Volume keys
(defun my/pulse-mute-toggle ()
  (interactive)
  (my/message-at-point (shell-command-to-string "amixer -M set Master toggle")))

(defun my/pulse-raise-volume ()
  (interactive)
  (my/message-at-point (shell-command-to-string "amixer -M set Master 2.5%+")))

(defun my/pulse-lower-volume ()
  (interactive)
  (my/message-at-point (shell-command-to-string "amixer -M set Master 2.5%-")))

;; *** Hydra
(with-eval-after-load 'hydra
  (defhydra my/pulse-hydra (:hint nil
				  :color red
				  :pre
				  (setq my/pulse-hydra/hint "Pulse control"))
    ("p" my/pulse-raise-volume)
    ("n" my/pulse-lower-volume)
    ("m" my/pulse-mute-toggle)))

;; *** Keys
(global-set-key (kbd "<XF86AudioLowerVolume>") 'my/pulse-lower-volume)
(global-set-key (kbd "<XF86AudioRaiseVolume>") 'my/pulse-raise-volume)
(global-set-key (kbd "<XF86AudioMute>") 'my/pulse-mute-toggle)

(global-set-key (kbd "s--") 'my/pulse-lower-volume)
(global-set-key (kbd "s-=") 'my/pulse-raise-volume)

(global-set-key (kbd "s-`") 'my/pulse-mute-toggle)

;; ** Music
(define-prefix-command 'my/music-map)
(define-key my/leader-map (kbd "m") 'my/music-map)

(define-key my/music-map (kbd "p") (lambda ()
				     (interactive)
				     (require 'hydra)
				     (my/pulse-raise-volume)
				     (my/pulse-hydra/body)))

(define-key my/music-map (kbd "n") (lambda ()
				     (interactive)
				     (require 'hydra)
				     (my/pulse-lower-volume)
				     (my/pulse-hydra/body)))

;; ***  Spotify
(setq spotify-oauth2-client-id my/spotify-client-id)
(setq spotify-oauth2-client-secret my/spotify-client-secret)

(setq counsel-spotify-client-id my/spotify-client-id)
(setq counsel-spotify-client-secret my/spotify-client-secret)

;; **** Counsel-spotify
(straight-use-package 'counsel-spotify)
(defvar my/is-spotify-loaded nil)

(defun my/spotify-start ()
  (my/async-start-process-shell-command "spotify" "spotify"
					(lambda (a)
					  (my/alert nil 'high)
					  (message (concat "ERROR: async process: spotify has died!")))))

(with-eval-after-load 'counsel-spotify
  (my/spotify-start)
  (setq my/is-spotify-loaded t))

;; ***** Keys
(define-key my/music-map (kbd "e") 'counsel-spotify-search-track)
(define-key my/music-map (kbd "E") 'counsel-spotify-search-playlist)
(define-key my/music-map (kbd "s") 'counsel-spotify-toggle-play-pause)

(define-key my/music-map (kbd "l") 'counsel-spotify-next)
(define-key my/music-map (kbd "h") 'counsel-spotify-previous)

(define-key my/music-map (kbd "o") (lambda () (interactive)
				     (if my/is-spotify-loaded
					 (progn
					   (unless (get-buffer "Spotify")
					     (my/spotify-start))
					   (switch-to-buffer (get-buffer "Spotify")))
				       (require 'counsel-spotify))))

(global-set-key (kbd "<XF86AudioPlay>") 'counsel-spotify-play)
(global-set-key (kbd "<XF86AudioStop>") 'counsel-spotify-toggle-play-pause)

;; **** Spotify.el
;; Not on melpa
;; (straight-use-package '(spotify.el :type git :host github :repo "danielfm/spotify.el"))
;; (setq spotify-transport 'dbus)
;; (setq spotify-transport 'connect)

;; *** EMMS
;; Setup emms
(defvar my/emms-has-init nil)

(defvar my/emms-init-hook nil
  "Hook called when emms has to init fully")

(straight-use-package 'emms)

(add-hook 'my/emms-init-hook (lambda () (interactive)
			       (unless my/emms-has-init
				 (setq my/emms-has-init t)
				 (require 'emms-setup)
				 (require 'emms-player-mpd)

				 (emms-all)
				 ;; Disable name of playing track in modeline (time is kept though)
				 (emms-mode-line-disable))))

(setq emms-mode-line-format nil)

(setq emms-seek-seconds 5)
(setq emms-player-list '(emms-player-mpd))
(setq emms-info-functions '(emms-info-mpd))

(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6600")

;; (setq mpc-host "localhost:6600")

;; **** Sort by directory name instead of metadata
(setq emms-browser-get-track-field-function 'emms-browser-get-track-field-use-directory-name)

;; **** Open playlist
;; emms doesn't automatically connect to mpd when loading playlist, results in empty playlist
(defun my/open-emms-and-connect()
  "Reconnect to MPD and open emms playlist"
  (interactive)
  (run-hooks 'my/emms-init-hook)
  (emms-player-mpd-connect)
  (emms-smart-browse))

;; **** Sync MPD and emms
(defun my/sync-mpd-and-emms ()
  "Updates the MPD and emms database synchronously."
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (emms-player-mpd-update-all-reset-cache)
  (emms-cache-set-from-mpd-all)
  (emms-player-mpd-connect)
  (message "MPD database and emms updated!"))

;; **** Keys
;; (define-key my/music-map (kbd "u") 'my/sync-mpd-and-emms)

;; (define-key my/music-map (kbd "o") 'my/open-emms-and-connect)
;; (define-key my/music-map (kbd "g") 'emms-seek-to)
;; (define-key my/music-map (kbd "s") 'emms-pause)

;; (with-eval-after-load 'emms-browser
;;   (define-key emms-browser-mode-map (kbd "s") 'emms-pause)

;;   (evil-define-key 'normal emms-browser-mode-map (kbd "RET") 'emms-browser-add-tracks)

;;   (evil-define-key 'normal emms-playlist-mode-map (kbd "RET") 'emms-playlist-mode-play-smart))

;; (global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)
;; (global-set-key (kbd "<XF86AudioStop>") 'emms-stop)

;; *** MPD
;; **** Start MPD
(defun my/start-mpd ()
  "Start MPD, connect to it and sync the metadata cache."
  (interactive)
  (start-process "mpd" nil "mpd")
  (message "MPD started and synced!"))

;; **** Kill daemon
(defun my/kill-music-daemon ()
  "Stops playback and kill the music daemon."
  (interactive)
  (emms-stop)
  (call-process "killall" nil nil nil "mpd")
  (message "MPD killed!"))

;; **** View MPD info
(defun my/mpd-info ()
  "Runs mpc, showing info in message field"
  (interactive)
  (shell-command "mpc"))

;; **** Shuffle playlist random
;; ***** Random on
(defun my/mpd-random-on ()
  "Turns on MPD random play"
  (interactive)
  (shell-command "mpc random on"))

;; ***** Random off
(defun my/mpd-random-off ()
  "Turns off MPD random play"
  (interactive)
  (shell-command "mpc random off"))

;; **** Volume control
;; ***** Raise volume
(defun my/mpd-raise-volume()
  (interactive)
  (shell-command "mpc volume +4"))

;; ***** Lower volume
(defun my/mpd-lower-volume ()
  (interactive)
  (shell-command "mpc volume -4"))

;; **** Change song
;; ***** Next song
(defun my/mpd-next-song()
  (interactive)
  (shell-command "mpc next"))

;; ***** Previous song
(defun my/mpd-previous-song()
  (interactive)
  (shell-command "mpc prev"))

;; **** Change time on track
;; ***** Forward
(defun my/mpd-wind-forward()
  (interactive)
  (shell-command "mpc seek +10"))

;; ***** Forward far
(defun my/mpd-wind-far-forward()
  (interactive)
  (shell-command "mpc seek +60"))

;; ***** Backwards
(defun my/mpd-wind-backward()
  (interactive)
  (shell-command "mpc seek -10"))

;; ***** Backwards far
(defun my/mpd-wind-far-backward()
  (interactive)
  (shell-command "mpc seek -60"))

;; **** Keys
;; (define-key my/music-map (kbd "C-s") 'my/start-mpd)
;; (define-key my/music-map (kbd "C-k") 'my/kill-music-daemon)
;; (define-key my/music-map (kbd "i") 'my/mpd-info)

;; (define-key my/music-map (kbd "r") 'my/mpd-random-on)
;; (define-key my/music-map (kbd "C-r") 'my/mpd-random-off)

;; (define-key my/music-map (kbd "=") 'my/mpd-raise-volume)
;; (define-key my/music-map (kbd "-") 'my/mpd-lower-volume)

;; (define-key my/music-map (kbd "n") 'my/mpd-next-song)
;; (define-key my/music-map (kbd "p") 'my/mpd-previous-song)

;; (define-key my/music-map (kbd "l") 'my/mpd-wind-forward)
;; (define-key my/music-map (kbd "h") 'my/mpd-wind-backward)
;; (define-key my/music-map (kbd "L") 'my/mpd-wind-far-forward)
;; (define-key my/music-map (kbd "H") 'my/mpd-wind-far-backward)

;; (global-set-key (kbd "<XF86AudioNext>") 'my/mpd-next-song)
;; (global-set-key (kbd "<XF86AudioPrev>") 'my/mpd-previous-song)

;; * Screenshots
;; ** Functions
;; *** Entire screen
(defun my/take-screenshot ()
  "Takes a fullscreen screenshot of the current workspace"
  (interactive)
  (when window-system
    (sit-for 1)
    (start-process "screenshot" nil "import" "-window" "root"
		   (concat (getenv "HOME") "/Pictures/Screenshots/" (subseq (number-to-string (float-time)) 0 10) ".png"))))

;; *** Region
(defun my/take-screenshot-region ()
  "Takes a screenshot of a region selected by the user."
  (interactive)
  (when window-system
    (call-process "import" nil nil nil ".newScreen.png")
    (call-process "convert" nil nil nil ".newScreen.png" "-shave" "1x1"
		  (concat (getenv "HOME") "/Pictures/Screenshots/" (subseq (number-to-string (float-time)) 0 10) ".png"))
    (call-process "rm" nil nil nil ".newScreen.png")))

;; *** Region ask for name
(defun my/take-screenshot-region-and-ask-for-name ()
  "Takes a screenshot of a region selected by the user and asks for file path"
  (interactive)
  (when window-system
    ;; Check if there is a directory called "images" in current dir, if so start read-file-name inside that directory
    (if (file-exists-p (concat default-directory "images/"))
	(setq screenshot-base-path (concat default-directory "images/"))
      (setq screenshot-base-path default-directory))

    ;; If screenshot path is not empty
    (call-process "import" nil nil nil ".newScreen.png")

    ;; Ask for path
    (setq screenshot-path (read-file-name "Screenshot file (.png already added) " screenshot-base-path))

    (call-process "convert" nil nil nil ".newScreen.png" "-shave" "1x1" (concat screenshot-path ".png"))
    (call-process "rm" nil nil nil ".newScreen.png")))

;; ** Keys
(global-set-key (kbd "<print>") 'my/take-screenshot-region-and-ask-for-name)

;;  (define-key my/leader-map (kbd "p r") 'my/take-screenshot-region)
;;  (define-key my/leader-map (kbd "p w") 'my/take-screenshot)

;; * Mail
(setq sendmail-program "msmtp")
(setq send-mail-function 'message-send-mail-with-sendmail)
;; (setq read-mail-command 'gnus)
;; (setq mail-user-agent 'gnus-user-agent)
;; (setq message-signature nil)
;; (setq message-send-mail-partially-limit nil)

;; ** mu4e
(setq mu4e-completing-read-function 'completing-read)

(setq mu4e-html2text-command "w3m -T text/html")
;; (setq mu4e-html2text-command 'mu4e-shr2text)
;; (setq mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
;; Doesn't work very well
;; (setq mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t org")

;; (setq mu4e-view-use-gnus t)
;; (setq mu4e-view-prefer-html t)
(setq mu4e-headers-auto-update t)
(setq mu4e-compose-signature-auto-include nil)
(setq mu4e-compose-format-flowed t)

;; enable inline images
(setq mu4e-view-show-images t)

;; don't save message to Sent Messages, IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; Show addresses instead of just name of sender
(setq mu4e-view-show-addresses t)

(setq mu4e-headers-time-format "%T")

;; Don't move cursor forward after marking
(setq mu4e-headers-advance-after-mark nil)

;; *** org-mime
;; (straight-use-package 'org-mime)
;; (with-eval-after-load 'mu4e
;;   (require 'org-mime)
;;   (setq org-mime-library 'mu4e))

;; *** org-integration
;; Seems like you have to require this in order for org features to work
(with-eval-after-load 'mu4e
  (require 'org-mu4e))

;; *** Find nixos install location
;; https://www.reddit.com/r/NixOS/comments/6duud4/adding_mu4e_to_emacs_loadpath/
(setq my/mu4epath
      (ignore-errors
	(concat
	 (f-dirname
	  (file-truename
	   (executable-find "mu")))
	 "/../share/emacs/site-lisp/mu4e")))

(when my/mu4epath
  (add-to-list 'load-path my/mu4epath))

;; *** mu4e mail counter
(straight-use-package 'mu4e-alert)

(defvar my/mu4e-unread-mail-count nil)

(setq mu4e-alert-modeline-formatter (lambda (count)
				      (setq my/mu4e-unread-mail-count (number-to-string count))))

;; *** Fetch mail at time interval
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-update-interval nil)

(my/allocate-update-time (lambda ()
			   (require 'mu4e)
			   (mu4e-update-mail-and-index t)
			   (require 'mu4e-alert)
			   (mu4e-alert-update-mail-count-modeline)) (* 60 5))

;; (with-eval-after-load 'mu4e
;;   (mu4e-update-mail-and-index t))

;; *** Keys
(define-key my/leader-map (kbd "M") (lambda () (interactive)
				      (require 'mu4e)
				      (mu4e t)
				      (call-interactively 'mu4e~headers-jump-to-maildir)))

(with-eval-after-load 'mu4e-view
  (define-key mu4e-view-mode-map (kbd "n") 'mu4e-view-headers-next)
  (define-key mu4e-view-mode-map (kbd "p") 'mu4e-view-headers-prev)

  (define-key mu4e-view-mode-map (kbd "N") 'mu4e-view-headers-next-unread)
  (define-key mu4e-view-mode-map (kbd "P") 'mu4e-view-headers-prev-unread))

;; **** View in different browser
(with-eval-after-load 'mu4e-view
  (add-to-list 'mu4e-view-actions '("Eww" . mu4e-action-view-in-browser) t)

  (defun my/mu4e-view-in-w3m (msg)
    (w3m (concat "file://" (mu4e~write-body-to-html msg))))
  (add-to-list 'mu4e-view-actions '("W3m" . my/mu4e-view-in-w3m) t)

  (defun my/mu4e-view-in-firefox (msg)
    (my/open-in-browser (concat "file://" (mu4e~write-body-to-html msg))))
  (add-to-list 'mu4e-view-actions '("Firefox" . my/mu4e-view-in-firefox) t))

;; *** Send messages
(setq mail-user-agent 'mu4e-user-agent)

;; **** org-mime
;; (straight-use-package 'org-mime)

;; (setq org-mime-library 'mml)

;; (with-eval-after-load 'mu4e-message
;;   (require 'org-mime))

;; **** org-msg
(straight-use-package 'org-msg)

(org-msg-mode)

(with-eval-after-load 'mu4e-message
  (require 'org-msg))

(setq org-msg-enforce-css nil)

;; *** Dynamically setting the width of the columns so it takes up the whole width
;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
(add-hook 'mu4e-headers-mode-hook
	  (defun my/mu4e-change-headers ()
	    (interactive)
	    (setq mu4e-headers-fields
		  `((:human-date . 25) ;; alternatively, use :date
		    (:flags . 6)
		    (:from . 22)
		    (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
		    (:size . 7)))))

;; ** Random color gnus logo colors
;; Show with (gnus-group-startup-message)
(with-eval-after-load 'gnus
  (random t)) ; Randomize sequence of random numbers

(defun my/random-hex (&optional num)
  (interactive "P")
  (let (($n (if (numberp num) (abs num) 6)))
    (format  (concat "%0" (number-to-string $n) "x" ) (random (1- (expt 16 $n))))))

(setq gnus-logo-colors (list (concat "#" (my/random-hex 6)) (concat "#" (my/random-hex 6))))

;; * System
(define-prefix-command 'my/system-commands-map)
(define-key my/leader-map (kbd "S") 'my/system-commands-map)

;; ** Suspend
(define-prefix-command 'my/system-suspend-map)
;; (define-key my/system-commands-map (kbd "s") 'my/system-suspend-map)

(defun my/systemd-suspend-PC ()
  (interactive)
  (ignore-errors
    (org-clock-out))
  (shell-command "systemctl suspend"))
;; (define-key my/system-suspend-map (kbd "C-s") 'my/systemd-suspend-PC)
(defalias 'my/systemd-sleep #'my/systemd-suspend-PC)

(defun my/systemd-hibernate-PC()
  (interactive)
  (ignore-errors
    (org-clock-out))
  (shell-command "systemctl hibernate"))

;; (define-key my/system-suspend-map (kbd "C-h") 'my/systemd-hibernate-PC)

;; ** Multi-monitor
(define-prefix-command 'my/system-monitor-map)
(define-key my/system-commands-map (kbd "m") 'my/system-monitor-map)

(defun my/x-suspend-monitor()
  (interactive)
  (shell-command "xset dpms force suspend"))
(define-key my/system-monitor-map (kbd "s") 'my/x-suspend-monitor)

(defun my/print-monitors ()
  (interactive)
  (shell-command "xrandr"))
(define-key my/system-monitor-map (kbd "p") 'my/print-monitors)

(if (and (window-system) my/device/monitor-setup-command)
    (async-shell-command my/device/monitor-setup-command "xrandr setup buffer"))

;; ** Proced
(setq-default proced-tree-flag nil)
(define-key my/system-commands-map (kbd "e") 'proced)

;; *** Disable line wrapping
(defun my/proced-mode ()
  (interactive)
  (visual-line-mode -1)
  ;; I need to delay this because of some reason
  (run-with-timer 0.000001 nil (lambda ()
				 (setq truncate-lines t))))

;; (add-hook 'proced-post-display-hook 'my/proced-mode)
(add-hook 'proced-mode-hook 'my/proced-mode)

;; *** Auto update interval
(setq proced-auto-update-interval 0.5)

;; *** Keys
(with-eval-after-load 'proced
  (define-key proced-mode-map (kbd "d") 'proced-send-signal)
  (define-key proced-mode-map (kbd "s") 'proced-sort-interactive))

;; ** Profiler
(define-prefix-command 'my/profiler-map)
(define-key my/system-commands-map (kbd "p") 'my/profiler-map)

(define-key my/profiler-map (kbd "s") 'profiler-start)
(define-key my/profiler-map (kbd "e") 'profiler-stop)
(define-key my/profiler-map (kbd "r") 'profiler-report)
(define-key my/profiler-map (kbd "R") 'profiler-reset)

;; * Networking
(define-prefix-command 'my/network-map)
(define-key my/system-commands-map (kbd "n") 'my/network-map)

;; ** Network manager
;; Right now enwc seems to only be able to switch wifi networks and display network status in modeline
;; Also check out https://github.com/Kodkollektivet/emacs-nm
(straight-use-package 'enwc)
(setq enwc-default-backend 'nm)

(define-key my/network-map (kbd "e") 'enwc)

;; *** Connect to wifi networks
(defun my/nm-connect-to-wifi-network ()
  (interactive)
  (shell-command
   (concat "nmcli device wifi connect "
	   (completing-read "Select network: "
			    (progn
			      (setq enwc-scan-interactive t)
			      (map 'list
				   (lambda (net) (enwc-value-from-scan 'essid net))
				   (enwc-get-networks))))
	   " password "
	   (read-passwd "Enter password: "))))

(define-key my/network-map (kbd "c") 'my/nm-connect-to-wifi-network)

;; *** Delete wifi networks
(defun my/nm-delete-wifi-network ()
  (interactive)
  (shell-command
   (concat "nmcli con delete "
	   (completing-read "Select network: "
			    (progn
			      (setq enwc-scan-interactive t)
			      (map 'list
				   (lambda (net) (enwc-value-from-scan 'essid net))
				   (enwc-get-networks)))))))

(define-key my/network-map (kbd "d") 'my/nm-delete-wifi-network)

;; ** Tramp
;; (setq tramp-default-method "scpx")

;; *** Buffer naming
;; Tramp buffers aren't prefixed with server name by default
;; https://emacs.stackexchange.com/questions/26444/include-host-in-buffer-name-for-all-files-opened-with-tramp
;; (defun my/tramp-add-server-prefix ()
;;   "Add the name of the connection type and server to the buffer name"
;;   (let* ((file (or buffer-file-name default-directory))
;;	 (is-file-remote (file-remote-p file)))
;;     (when is-file-remote
;;       (rename-buffer file))))

;; (add-hook 'find-file-hook #'my/tramp-add-server-prefix)
;; (add-hook 'dired-mode-hook #'my/tramp-add-server-prefix)

;; *** Performance
;; https://gist.github.com/ralt/a36288cd748ce185b26237e6b85b27bb

;; ** Netstat
(defun my/net-utils-mode ()
  (interactive)
  (toggle-truncate-lines 1)
  (visual-line-mode -1))

(add-hook 'net-utils-mode-hook 'my/net-utils-mode)

;; ** Keys
(define-key my/network-map (kbd "s") 'netstat)
(define-key my/network-map (kbd "p") 'ping)
(define-key my/network-map (kbd "P") (lambda () (interactive) (ping "8.8.8.8")))
(define-key my/network-map (kbd "i") 'ifconfig)

;; * Hardware
(define-prefix-command 'my/hardware-info-map)
(define-key my/system-commands-map (kbd "h") 'my/hardware-info-map)

;; Memory
(defun my/unix-get-memory-available()
  (interactive)
  (shell-command "grep \"MemAvailable\" /proc/meminfo"))
(define-key my/hardware-info-map (kbd "m") 'my/unix-get-memory-available)

;; GPU
(defun my/unix-get-gpu()
  (interactive)
  (shell-command "lspci | grep ' VGA ' | cut -d\" \" -f 1 | xargs -i lspci -v -s {}"))
(define-key my/hardware-info-map (kbd "g") 'my/unix-get-gpu)

;; Blocked devices
(defun my/rfkill-get-blocked-devices()
  (interactive)
  (shell-command "rfkill list"))
(define-key my/hardware-info-map (kbd "b") 'my/rfkill-get-blocked-devices)

;; Get devices
(defun my/rfkill-get-devices()
  (interactive)
  (shell-command "cat /proc/devices"))
(define-key my/hardware-info-map (kbd "d") 'my/rfkill-get-devices)

;; ** CPU
;; Linux temps
(if (file-exists-p "/proc/cpuinfo")
    (progn
      (define-prefix-command 'my/cpu-info-map)
      (define-key my/hardware-info-map (kbd "c") 'my/cpu-info-map)

      (defun my/unix-cpu-get-clock()
	(interactive)
	(shell-command "grep \"cpu MHz\" /proc/cpuinfo"))
      ;; Clock speed
      (define-key my/cpu-info-map (kbd "f") 'my/unix-cpu-get-clock)
      ;; Model name
      (defun my/unix-cpu-get-name()
	(interactive)
	(shell-command "grep \"model name\" /proc/cpuinfo"))
      (define-key my/cpu-info-map (kbd "n") 'my/unix-cpu-get-name)
      ;; Core count
      (defun my/unix-cpu-get-core-count()
	(interactive)
	;; Linux
	(shell-command "grep \"cores\" /proc/cpuinfo"))

      (define-key my/cpu-info-map (kbd "c") 'my/unix-cpu-get-core-count)
      ;; Flags
      (defun my/unix-cpu-get-flags()
	(interactive)
	(shell-command "grep \"flags\" /proc/cpuinfo"))
      (define-key my/cpu-info-map (kbd "F") 'my/unix-cpu-get-flags)
      ;; Vendor
      (defun my/unix-cpu-get-vendor-id()
	(interactive)
	(shell-command "grep \"vendor_id\" /proc/cpuinfo"))
      (define-key my/cpu-info-map (kbd "v") 'my/unix-cpu-get-vendor-id)
      ;; Bugs (Bugs that has affected CPU model)
      (defun my/unix-cpu-get-bugs()
	(interactive)
	(shell-command "grep \"bugs\" /proc/cpuinfo"))
      (define-key my/cpu-info-map (kbd "b") 'my/unix-cpu-get-bugs)
      ;; Cache size
      (defun my/unix-cpu-get-cache-size()
	(interactive)
	(shell-command "grep \"cache size\" /proc/cpuinfo"))
      (define-key my/cpu-info-map (kbd "C") 'my/unix-cpu-get-cache-size)))

;; Windows cpu core count
;; (if (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
;; (let ((number-of-processors (getenv "NUMBER_OF_PROCESSORS")))
;; (if number-of-processors
;; (string-to-number number-of-processors))))

;; * Find
;; ** Ellocate
(straight-use-package '(ellocate :type git :host github :repo "walseb/ellocate"))

;; ** Find get file name
;; Only difference to this and counsel-file-jump is that this returns the string of the file name
(defun my/counsel-file-jump-get-name (&optional initial-input initial-directory)
  (interactive
   (list nil
	 (when current-prefix-arg
	   (counsel-read-directory-name "From directory: "))))
  (counsel-require-program find-program)
  (let ((default-directory (or initial-directory default-directory)))
    (ivy-read "Find file: "
	      (counsel--find-return-list counsel-file-jump-args)
	      :matcher #'counsel--find-file-matcher
	      :initial-input initial-input
	      :preselect (counsel--preselect-file)
	      :require-match 'confirm-after-completion
	      :history 'file-name-history
	      :caller 'counsel-file-jump)))


;; ** Auto grep
(defun my/auto-grep ()
  (interactive)
  (if (projectile-project-p)
      (counsel-projectile-rg)
    ;; (counsel-git-grep)
    (my/counsel-grep)))

;; ** Auto find
(defun my/auto-find ()
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (ellocate)))

;; ** grep
(setq grep-mode-map (make-sparse-keymap))

;; ** wgrep
;; Disable confirmation when saving edits
(setq wgrep-auto-save-buffer t)

;; *** Keys
;; Used in ivy occur
(setq wgrep-mode-map (make-sparse-keymap))

(with-eval-after-load 'wgrep
  (define-key wgrep-mode-map [remap save-buffer] 'wgrep-finish-edit)

  (define-prefix-command 'my/wgrep-map)
  (evil-define-key 'normal wgrep-mode-map (kbd (concat my/leader-map-key " a")) 'my/wgrep-map)

  (define-key my/wgrep-map (kbd "s") 'wgrep-finish-edit)
  (define-key my/wgrep-map (kbd "k") 'wgrep-abort-changes))

;; ** Occur
(setq occur-mode-map (make-sparse-keymap))
(setq occur-edit-mode-map (make-sparse-keymap))

;; ** Ivy occur
;; *** Keys
;; Also ivy-occur-grep
(setq ivy-occur-mode-map (make-sparse-keymap))
(setq-default ivy-occur-mode-map (make-sparse-keymap))

(with-eval-after-load 'ivy
  (define-prefix-command 'my/ivy-occur-map)
  (evil-define-key 'normal ivy-occur-mode-map (kbd (concat my/leader-map-key " a")) 'my/ivy-occur-map)
  (evil-define-key 'normal ivy-occur-grep-mode-map (kbd (concat my/leader-map-key " a")) 'my/ivy-occur-map)

  (define-key my/ivy-occur-map (kbd "w") 'ivy-wgrep-change-to-wgrep-mode)
  (define-key ivy-occur-grep-mode-map (kbd "w") 'ivy-wgrep-change-to-wgrep-mode)

  (evil-define-key '(normal visual insert) ivy-occur-mode-map (kbd "RET") 'ivy-occur-press)
  (evil-define-key '(normal visual) ivy-occur-mode-map (kbd "C-y") 'ivy-occur-read-action)

  (define-key ivy-occur-mode-map "g" 'ivy-occur-revert-buffer)

  (setq ivy-occur-grep-mode-map (copy-keymap ivy-occur-mode-map))
  (setq-default ivy-occur-grep-mode-map (copy-keymap ivy-occur-mode-map)))

;; ** Loccur
(straight-use-package 'loccur)

(defvar-local my/loccur-search-running nil)

(defun my/loccur-isearch ()
  (interactive)
  (require 'loccur)
  (setq-local my/loccur-search-running t)
  (my/isearch-forward-regexp)
  (setq-local my/loccur-search-running nil))

(add-hook 'isearch-update-post-hook 'my/loccur-isearch-update)

(defun my/loccur-isearch-update ()
  (when my/loccur-search-running
    (loccur-mode -1)
    (if (not (string= isearch-string ""))
	(loccur (ivy--regex-plus isearch-string)))))

(add-hook 'isearch-mode-end-hook 'my/loccur-isearch-quit)

(defun my/loccur-isearch-quit ()
  (if (and my/loccur-search-running isearch-mode-end-hook-quit)
      (loccur-mode -1)))

;; *** Keys
(my/evil-universal-define-key "C-S-s" 'my/loccur-isearch)
(my/evil-universal-define-key "C-M-s" 'my/loccur-isearch)

;; * Spelling
(define-prefix-command 'my/spell-map)
;; (define-key my/leader-map (kbd "S") 'my/spell-map)

;; http://aspell.net/0.50-doc/man-html/4_Customizing.html#suggestion
;; Allow 5 words to be connected without spaces. Default is 2
;; Run-together causes a performance loss while typing but bad-spellers only

;; ** Spelling configuration
;; Run-together makes it so words can be linked together without spaces
;; (setq ispell-extra-args (list "--sug-mode=bad-spellers" "--run-together" "--run-together-limit=5"))

(setq ispell-extra-args '("--sug-mode=bad-spellers" "--ignore-case"))

;; ** Flyspell
(setq flyspell-mode-map (make-sparse-keymap))

;; This binds a key if t
(setq flyspell-use-meta-tab nil)

(define-key my/spell-map (kbd "d") 'ispell-change-dictionary)
(define-key my/spell-map (kbd "s") 'flyspell-mode)

;; List of major modes not to check
(setq my/flyspell-do-not-check '(
				 minibuffer-inactive-mode
				 eshell-mode
				 shell-mode
				 term-mode

				 wdired-mode

				 haskell-interactive-mode
				 ))

(defun my/flyspell-mode-auto-select ()
  ;; Don't run this right when flyspell mode is on, the mode might not have changed yet. Instead wait a millisecond until the mode has been decided and then check for prog-mode
  (run-with-timer 0.5 nil (lambda ()
			    (if (derived-mode-p 'prog-mode)
				(flyspell-prog-mode)
			      ;; It has to be both writable and not a part of the do not check list for spell checking to activate
			      (when (and (not buffer-read-only) (not (member major-mode my/flyspell-do-not-check)))
				(flyspell-mode 1))))))

(define-globalized-minor-mode global-my/flyspell-mode
  flyspell-mode my/flyspell-mode-auto-select)
(global-my/flyspell-mode 1)

;; *** Personal directory
(setq ispell-personal-dictionary (concat user-emacs-directory ".aspell.en.pws"))

;; *** Flyspell-prog enable only for certain faces
;; Don't auto correct strings
(setq flyspell-prog-text-faces
      '(
	;; font-lock-string-face
	font-lock-comment-face
	font-lock-doc-face))

;; *** Flyspell-Correct
(straight-use-package 'flyspell-correct)

;; **** Key
(my/evil-universal-define-key "C-d" 'flyspell-correct-at-point)

;; *** Company
(defun my/toggle-company-ispell ()
  (interactive)
  (cond
   ((memq 'company-ispell company-backends)
    (setq company-backends (delete 'company-ispell company-backends))
    (message "company-ispell disabled"))
   (t
    (add-to-list 'company-backends 'company-ispell)
    (message "company-ispell enabled!"))))

(define-key my/spell-map (kbd "c") 'my/toggle-company-ispell)

;; ** Langtool
(straight-use-package 'langtool)

(setq langtool-language-tool-jar
      (ignore-errors
	(concat
	 (f-dirname
	  (file-truename
	   (executable-find "languagetool")))
	 "/../share/languagetool-commandline.jar")))

(setq langtool-autoshow-idle-delay 0)
(setq langtool-mother-tongue "en-US")

(define-key my/spell-map (kbd "l") 'langtool-check)
(define-key my/spell-map (kbd "L") 'langtool-check-done)

;; * Calc
(define-key my/leader-map (kbd "c") 'calc)

(defun my/calc-kill-current-line ()
  (interactive)
  (calc-kill-region (line-beginning-position) (line-end-position)))

(evil-define-key 'normal calc-mode-map [remap evil-delete-whole-line] 'my/calc-kill-current-line)
(evil-define-key 'visual calc-mode-map (kbd "d") 'calc-kill-region)

;; Multiplication should have the same precedence as division
(setq calc-multiplication-has-precedence nil)

;; ** calc-at-point
(straight-use-package '(calc-at-point :type git :host github :repo "walseb/calc-at-point"))

(evil-define-operator my/calc-at-point-repeat (beg end type)
  (interactive "<R>")
  (calc-at-point-repeat-last beg end))

(my/evil-normal-define-key "_" 'my/calc-at-point-repeat)
(my/evil-visual-define-key "_" 'my/calc-at-point-repeat)

(evil-define-operator my/calc-at-point-add (beg end type)
  (interactive "<R>")
  (calc-at-point-add beg end))

(my/evil-normal-define-key "+" 'my/calc-at-point-add)
(my/evil-visual-define-key "+" 'my/calc-at-point-add)

(evil-define-operator my/calc-at-point-sub (beg end type)
  (interactive "<R>")
  (calc-at-point-sub beg end))

(my/evil-normal-define-key "-" 'my/calc-at-point-sub)
(my/evil-visual-define-key "-" 'my/calc-at-point-sub)

;; ** macro-math
(straight-use-package 'macro-math)

(evil-define-operator evil-macro-math (beg end type)
  "Run eval on BEG to END."
  (interactive "<R>")
  (macro-math-eval-region beg end))

(my/evil-normal-define-key "'" 'evil-macro-math)

;; * Artist mode
;; ** Completing read
;; https://www.emacswiki.org/emacs/ArtistMode
(defun my/artist-select-operation (type)
  "Use ido to select a drawing operation in artist-mode"
  (interactive (list (completing-read "Drawing operation: "
				      (list "Pen" "Pen Line" "line" "straight line" "rectangle"
					    "square" "poly-line" "straight poly-line" "ellipse"
					    "circle" "text see-thru" "text-overwrite" "spray-can"
					    "erase char" "erase rectangle" "vaporize line" "vaporize lines"
					    "cut rectangle" "cut square" "copy rectangle" "copy square"
					    "paste" "flood-fill"))))
  (artist-select-operation type))

(defun my/artist-select-settings (type)
  "Use ido to select a setting to change in artist-mode"
  (interactive (list (completing-read "Setting: "
				      (list "Set Fill" "Set Line" "Set Erase" "Spray-size" "Spray-chars"
					    "Rubber-banding" "Trimming" "Borders"))))
  (if (equal type "Spray-size")
      (artist-select-operation "spray set size")
    (call-interactively (artist-fc-get-fn-from-symbol
			 (cdr (assoc type '(("Set Fill" . set-fill)
					    ("Set Line" . set-line)
					    ("Set Erase" . set-erase)
					    ("Rubber-banding" . rubber-band)
					    ("Trimming" . trimming)
					    ("Borders" . borders)
					    ("Spray-chars" . spray-chars))))))))

;; ** Picture mode keys
(setq picture-mode-map (make-sparse-keymap))
(setq-default picture-mode-map (make-sparse-keymap))

;; ** Keys
(define-key my/leader-map (kbd "A") 'artist-mode)

(setq artist-mode-map (make-sparse-keymap))

(with-eval-after-load 'artist
  ;; (evil-define-key 'normal artist-mode-map (kbd "p") 'artist-previous-line)
  ;; (evil-define-key 'normal artist-mode-map (kbd "n") 'artist-next-line)

  (define-key artist-mode-map [down-mouse-1] 'artist-down-mouse-1)
  ;; (define-key artist-mode-map [S-down-mouse-1] 'artist-down-mouse-1)
  (define-key artist-mode-map [down-mouse-2] 'artist-mouse-choose-operation)
  ;; (define-key artist-mode-map [S-down-mouse-2] 'artist-mouse-choose-operation)
  (define-key artist-mode-map [down-mouse-3] 'artist-mouse-choose-operation)
  ;; (define-key artist-mode-map [S-down-mouse-3] 'artist-down-mouse-3)
  (define-key artist-mode-map [C-mouse-4] 'artist-select-prev-op-in-list)
  (define-key artist-mode-map [C-mouse-5] 'artist-select-next-op-in-list))

;; * Image modes
;; ** PDF view
(defun my/pdf-view-mode()
  (interactive)
  (display-line-numbers-mode -1))

(add-hook 'pdf-view-mode-hook 'my/pdf-view-mode t)

;; ** PDF tools
(straight-use-package 'pdf-tools)

(add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . my/init-pdf-tools))
;; (add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))

(setq my/pdf-tools-installed nil)

(defun my/init-pdf-tools ()
  (interactive)
  (if (not my/pdf-tools-installed)
      (progn
	(require 'pdf-view)
	(pdf-tools-install)
	;; reload buffer with everything set
	(revert-buffer :ignore-auto :noconfirm)))
  (setq my/pdf-tools-installed t)
  (pdf-view-mode))

;; (add-hook 'pdf-view-mode-hook 'my/init-pdf-tools)

;; Enable pdf-links
(add-hook 'pdf-view-mode-hook 'pdf-links-minor-mode)

;; Remove default keys
(setq pdf-view-mode-map (make-sparse-keymap))

;; *** Keys
;; Disable insert mode
(define-key pdf-view-mode-map [remap evil-insert] 'evil-force-normal-state)

;; Scroll half page
(define-key pdf-view-mode-map [remap View-scroll-half-page-backward] 'pdf-view-scroll-down-or-previous-page)
(define-key pdf-view-mode-map [remap View-scroll-half-page-forward] 'pdf-view-scroll-up-or-next-page)

(define-key pdf-view-mode-map [remap evil-scroll-up] 'pdf-view-scroll-down-or-previous-page)
(define-key pdf-view-mode-map [remap evil-scroll-down] 'pdf-view-scroll-up-or-next-page)

;; goto
(define-key pdf-view-mode-map [remap evil-goto-first-line] 'pdf-view-first-page)

;; (kbd "G") = (evil-goto-line LAST-LINE)
(define-key pdf-view-mode-map [remap evil-goto-line] 'pdf-view-last-page)
;; search
(define-key pdf-view-mode-map [remap counsel-grep-or-swiper] 'isearch-forward)
(define-key pdf-view-mode-map [remap swiper] 'isearch-forward)
(define-key pdf-view-mode-map [remap counsel-grep] 'isearch-forward)

;; Movement
(define-key pdf-view-mode-map [remap evil-next-line] (lambda () (interactive) (image-next-line 4)))
(define-key pdf-view-mode-map [remap evil-previous-line] (lambda () (interactive) (image-previous-line 4)))

(define-key pdf-view-mode-map [remap evil-forward-char] (lambda () (interactive) (image-forward-hscroll 8)))
(define-key pdf-view-mode-map [remap evil-backward-char] (lambda () (interactive) (image-backward-hscroll 8)))

;; Disable other modes
(evil-define-key 'normal pdf-view-mode-map (kbd "i") 'nil)
(evil-define-key 'normal pdf-view-mode-map (kbd "v") 'nil)
(evil-define-key 'normal pdf-view-mode-map (kbd "R") 'nil)

;; Zoom
(evil-define-key 'normal pdf-view-mode-map (kbd "-") 'pdf-view-shrink)
(evil-define-key 'normal pdf-view-mode-map (kbd "=") 'pdf-view-enlarge)
(evil-define-key 'normal pdf-view-mode-map (kbd "_") 'pdf-view-scale-reset)
(evil-define-key 'normal pdf-view-mode-map (kbd "+") 'pdf-view-scale-reset)

;; Add to leader map
(define-prefix-command 'my/pdf-view-mode-map)
(evil-define-key 'normal pdf-view-mode-map (kbd (concat my/leader-map-key " a")) 'my/pdf-view-mode-map)

(define-key my/pdf-view-mode-map (kbd "o") 'pdf-occur)
(define-key my/pdf-view-mode-map (kbd "t") 'doc-view-open-text)
(define-key my/pdf-view-mode-map (kbd "n") 'pdf-view-midnight-minor-mode)
(define-key my/pdf-view-mode-map (kbd "g") 'pdf-view-goto-label)
(define-key my/pdf-view-mode-map (kbd "i") 'pdf-view-extract-region-image)

;; ** Image mode
(with-eval-after-load 'image-mode
  (add-hook 'image-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'image-mode-hook (lambda () (auto-revert-mode 1))))

;; Make animated images loop
(setq image-animate-loop t)

;; *** Instant auto resize
(setq image-auto-resize-on-window-resize 0)

;; *** Open otf fonts with image mode
(add-to-list 'auto-mode-alist '("\\.otf\\'" . image-mode))

;; *** Blimp
(straight-use-package 'blimp)

(setq eimp-minor-mode-map (make-sparse-keymap))

(setq eimp-enable-undo t)

;; (add-hook 'image-mode-hook 'blimp-mode)

;; **** Recolor
(defun my/blimp-annotate-middle()
  (interactive)
  (blimp-add-to-command-stack (list "-gravity" "Center" "-fill" "red" "-pointsize" "25" "-annotate" "0,0" (completing-read "enter text " nil)))
  (blimp-execute-command-stack)
  (sleep-for 0.2))

;; *** Keys
(define-key my/pdf-view-mode-map (kbd "g") 'pdf-view-goto-label)

(setq image-mode-map (make-sparse-keymap))

(with-eval-after-load 'image-mode
  (define-key image-mode-map (kbd "-") 'image-decrease-size)
  (define-key image-mode-map (kbd "_") 'image-decrease-size)
  (define-key image-mode-map (kbd "=") 'image-increase-size)
  (define-key image-mode-map (kbd "+") 'image-increase-size)

  (define-key image-mode-map (kbd "C-u") 'image-scroll-down)
  (define-key image-mode-map (kbd "C-w") 'image-scroll-up)

  (define-key image-mode-map (kbd "n") (lambda () (interactive) (image-next-line 8)))
  (define-key image-mode-map (kbd "p") (lambda () (interactive) (image-previous-line 8)))
  (define-key image-mode-map (kbd "h") (lambda () (interactive) (image-backward-hscroll 8)))
  (define-key image-mode-map (kbd "l") (lambda () (interactive) (image-forward-hscroll 8)))

  (define-key image-mode-map (kbd "w") (lambda () (interactive) (image-forward-hscroll (* 8 5))))
  (define-key image-mode-map (kbd "e") (lambda () (interactive) (image-forward-hscroll (* 8 5))))
  (define-key image-mode-map (kbd "b") (lambda () (interactive) (image-backward-hscroll (* 8 5))))

  (define-key image-mode-map (kbd "W") (lambda () (interactive) (image-forward-hscroll (* 8 10))))
  (define-key image-mode-map (kbd "E") (lambda () (interactive) (image-forward-hscroll (* 8 10))))
  (define-key image-mode-map (kbd "B") (lambda () (interactive) (image-backward-hscroll (* 8 10))))

  (define-key image-mode-map (kbd "N") 'image-next-file)
  (define-key image-mode-map (kbd "P") 'image-previous-file)

  (define-key image-mode-map (kbd "L") 'image-transform-fit-to-width)
  (define-key image-mode-map (kbd "H") 'image-transform-fit-to-height)

  (define-key image-mode-map (kbd "G") (lambda () (interactive) (image-next-line 1000)))
  (define-key image-mode-map (kbd "g g") (lambda () (interactive) (image-previous-line 1000)))

  (define-key image-mode-map (kbd "$") (lambda () (interactive) (image-forward-hscroll 1000)))
  (define-key image-mode-map (kbd "0") (lambda () (interactive) (image-backward-hscroll 1000))))

(with-eval-after-load 'blimp-mode
  (define-prefix-command 'my/image-mode-map)
  (evil-define-key 'normal image-mode-map (kbd (concat my/leader-map-key " a")) 'my/image-mode-map)

  (define-key my/image-mode-map (kbd "i") 'blimp-interface)
  (define-key my/image-mode-map (kbd "I") 'blimp-interface-execute)

  (define-key my/image-mode-map (kbd "r") 'blimp-clear-command-stack)
  (define-key my/image-mode-map (kbd "e") 'blimp-execute-command-stack)
  (define-key my/image-mode-map (kbd "p") 'blimp-toggle-prefix)
  (define-key my/image-mode-map (kbd "p") 'blimp-toggle-prefix)

  (define-key my/image-mode-map (kbd "a") 'my/blimp-annotate-middle))

;; * Spray
(straight-use-package 'spray)

(setq spray-wpm 500)

(with-eval-after-load 'spray
  (define-key spray-mode-map (kbd "p") 'spray-slower)
  (define-key spray-mode-map (kbd "n") 'spray-faster))

(define-key my/leader-map (kbd "M-v") 'spray-mode)

;; * Ligatures
;; Check out prettify-utils
(if window-system
    (global-prettify-symbols-mode 1))
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Redefine so that prettify mode is enabled even if a buffer local symbols alist isn't defined
;; (defun turn-on-prettify-symbols-mode ()
;; (when (not prettify-symbols-mode)
;; (prettify-symbols-mode 1)))

;; Replace comments with symbol
;; "^[\s\\|\t]*;+"
;; \\(^ *;; \\*\\)

;; ** Redefine prettify-symbols--post-command-hook to not error
(defun prettify-symbols--post-command-hook ()
  ;; This line is the only change
  (ignore-errors
    (cl-labels ((get-prop-as-list
		 (prop)
		 (remove nil
			 (list (get-text-property (point) prop)
			       (when (and (eq prettify-symbols-unprettify-at-point 'right-edge)
					  (not (bobp)))
				 (get-text-property (1- (point)) prop))))))
      ;; Re-apply prettification to the previous symbol.
      (when (and prettify-symbols--current-symbol-bounds
		 (or (< (point) (car prettify-symbols--current-symbol-bounds))
		     (> (point) (cadr prettify-symbols--current-symbol-bounds))
		     (and (not (eq prettify-symbols-unprettify-at-point 'right-edge))
			  (= (point) (cadr prettify-symbols--current-symbol-bounds)))))
	(apply #'font-lock-flush prettify-symbols--current-symbol-bounds)
	(setq prettify-symbols--current-symbol-bounds nil))
      ;; Unprettify the current symbol.
      (when-let* ((c (get-prop-as-list 'composition))
		  (s (get-prop-as-list 'prettify-symbols-start))
		  (e (get-prop-as-list 'prettify-symbols-end))
		  (s (apply #'min s))
		  (e (apply #'max e)))
	(with-silent-modifications
	  (setq prettify-symbols--current-symbol-bounds (list s e))
	  (remove-text-properties s e '(composition nil)))))))

;; ** Magit
;; Prettify symbols doesn't work with magit
(add-hook 'magit-mode-hook (lambda () (interactive) (prettify-symbols-mode -1)))

;; ** Symbols
;; Add more symbols from here:
;; https://github.com/pretty-mode/pretty-mode/blob/master/pretty-mode.el#L356

;; Read =reference-point-alist= to understand how to merge characters and add spaces to characters

;; *** Generic
(defconst my/generic-equality-symbols
  '(
    ("==" . ?≡)
    ("/=" . ?≠)
    ("!=" . ?≠)
    (">=" . ?≥)
    ("<=" . ?≤)
    ))

(defconst my/generic-arrow-symbols
  '(
    ;; Fish here is a bit wrong but there isn't a proper double arrowed one in the utf spec that I could find
    ;; ("<=<" . ?↢)
    ;; (">=>" . ?↣)

    ("<-<" . ?↢)
    (">->" . ?↣)

    ;; (">=>" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe146))

    ("-<" . ?⤙)
    (">-" . ?⤚)

    ("~>" . ?⇝)
    ("<~" . ?⇜)

    ("->" . ?→)
    ("<-" . ?←)

    ("=>" . ?⇒)
    ;; Conflicting with equality symbols
    ;; ("<=" . ?⇐)

    ("->>" . ?↠)
    ("<<-" . ?↞)

    ("|>" . ?⊳)
    ("<|" . ?⊲)

    ("<<" . ?≪)
    (">>" . ?≫)

    ("<<<" . ?⋘)
    (">>>" . ?⋙)

    ("><" . ?⋈)
    ))

(defconst my/generic-greek-symbols
  '(("lambda" . ?λ)))

(defconst my/generic-logic-symbols
  '(("&&" . ?∧)
    ("||" . ?∨)))

;; *** Org
;; Doesn't work without regexps because there isn't any space between org emphasis markers and the contents. For example this ~code~
;; (defconst my/org-hide-emphasis
;;   '(
;;     ("*" . ? )
;;     ("/" . ? )
;;     ("_" . ? )
;;     ("=" . ? )
;;     ("~" . ? )
;;     ("+" . ? )))

;; *** Comment delimiter
;; Font lock automatically handles comment highlighting through the function font-lock-fontify-syntactically-region

;; https://www.w3schools.com/charsets/ref_utf_block.asp
;; https://en.wikipedia.org/wiki/Block_Elements
;; █ comment
;; ▉ comment
;; ▊ comment
;; ▋ comment
;; ▌ comment
;; ▌ comment
;; ▍ comment
;; ▎ comment
;; ▏ comment
;; ▐ comment
;; Here we just use prettify symbols mode to hide the comment so that font-lock can highlight its background
;; Using prettify-symbol to display the comment box leaves gaps between them, using font lock doesn't
(setq my/pretty-comment-symbol ? )

;; Use font lock to
(add-hook 'prog-mode-hook (lambda ()
			    (setq-local font-lock-comment-start-skip (concat (s-trim-right comment-start) "+"))))

(defun my/prettify-comment ()
  `((,(string-trim comment-start) . ,my/pretty-comment-symbol)))

(defun my/prettify-comment-lisp ()
  `((,(concat (string-trim comment-start) (string-trim comment-start)) . ,my/pretty-comment-symbol)))

;; *** Outline headings
(defun my/prettify-outline-heading ()
  `(
    (,(concat (string-trim comment-start) " *") . ?◉)
    (,(concat (string-trim comment-start) " **") . (?\s (Br . Bl) ?○))
    (,(concat (string-trim comment-start) " ***") . (?\s (Br . Bl) ?\s (Br . Bl) ?✸))
    (,(concat (string-trim comment-start) " ****") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✿))
    (,(concat (string-trim comment-start) " *****") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?◉))
    (,(concat (string-trim comment-start) " ******") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?○))
    (,(concat (string-trim comment-start) " *******") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✸))
    (,(concat (string-trim comment-start) " ********") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✿))))

(defun my/prettify-outline-heading-lisp ()
  `((,(concat (string-trim comment-start) (string-trim comment-start) " *") . ?◉)
    (,(concat (string-trim comment-start) (string-trim comment-start) " **") . (?\s (Br . Bl) ?○))
    (,(concat (string-trim comment-start) (string-trim comment-start) " ***") . (?\s (Br . Bl) ?\s (Br . Bl) ?✸))
    (,(concat (string-trim comment-start) (string-trim comment-start) " ****") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✿))
    (,(concat (string-trim comment-start) (string-trim comment-start) " *****") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?◉))
    (,(concat (string-trim comment-start) (string-trim comment-start) " ******") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?○))
    (,(concat (string-trim comment-start) (string-trim comment-start) " *******") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✸))
    (,(concat (string-trim comment-start) (string-trim comment-start) " ********") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✿))))

(defun my/prettify-outline-heading-lisp-classic ()
  `((,(my/repeat-char (string-trim comment-start) "" 3) . ?◉)
    (,(my/repeat-char (string-trim comment-start) "" 4) . (?\s (Br . Bl) ?○))
    (,(my/repeat-char (string-trim comment-start) "" 5) . (?\s (Br . Bl) ?\s (Br . Bl) ?✸))
    (,(my/repeat-char (string-trim comment-start) "" 6) . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✿))
    (,(my/repeat-char (string-trim comment-start) "" 7) . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?◉))
    (,(my/repeat-char (string-trim comment-start) "" 8) . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?○))
    (,(my/repeat-char (string-trim comment-start) "" 9) . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✸))
    (,(my/repeat-char (string-trim comment-start) "" 10) . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✿))))

;; *** Fsharp
(defconst my/fsharp-symbols
  (list))

;; *** Haskell
;; https://github.com/roelvandijk/base-unicode-symbols
;; https://github.com/enomsg/vim-haskellConcealPlus/blob/master/after/syntax/haskell.vim
;; http://haskell.github.io/haskell-mode/manual/latest/Unicode-support.html#Unicode-support
;; https://github.com/roelvandijk/emacs-haskell-unicode-input-method/blob/master/haskell-unicode-input-method.el
;; https://emacs.nasy.moe/#orgf407c8c

(defconst my/haskell-symbols
  '(("\\" . ?λ)
    ("()" . ?∅)
    ("!!" . ?‼)
    ("sqrt" . ?√)
    ("undefined" . ?⊥)
    ("pi" . ?π)
    ("not" . ?¬)
    ;;("::" . ?∷)
    ("exists" . ?∃)

    ;; Here we construct a custom symbol that has the spaces that are removed when replacing " . " with a single char
    (" . " . (?\s (Br . Bl) ?\s (Bc . Bc) ?\s (Br . Bl) ?\s (Bc . Bc) ?∘)) ; "○"
    ;; Doesn't work?
    ;;haskell-font-lock-dot-is-not-composition)
    ("forall" . ?∀)

    ;; ("(*)" . ?×)

    ;; Foldable
    ("elem" . ?∈)
    ("notElem" . ?∉)

    ;; List
    ("[]" . ?ε)
    ("++" . ?⧺)
    ("union" . ?∪)
    ("intersect" . ?∩)
    ("isSubsetOf" . ?⊆)
    ("isProperSubsetOf" . ?⊂)

    ("theta" . ?θ)
    ("delta" . ?Δ)
    ("deltaV" . (?Δ (Br . Bl) ?V))
    ("deltaV'" . (?Δ (Br . Bl) ?V (Br . Bl) ?'))
    ("iDeltaV" . (?i (Br . Bl) ?Δ (Br . Bl) ?V ))


    ;; ("\\" . ?∖)

    ;; Monoid
    ("mempty" . ?∅)
    ("mappend" . ?⊕)

    ("integral" . ?∫)
    ("integralFrom" . ?∫)
    ("imIntegral" . ?∫)

    ;; Arrows
    ;; ("***" . ?⁂)
    ;; ("|||" . ?⫴)
    ;; ("+++" . ?⧻)
    ))

;; https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
(defconst my/haskell-type-symbols
  '(
    ("Bool" . ?𝔹)
    ("Real" . ?ℝ)
    ("Integer" . ?ℤ)
    ("Natural" . ?ℕ)
    ("Rational" . ?ℚ)
    ("Irrational" . ?ℙ)
    ("False" . ?𝔽)
    ("True" . ?𝕋)
    ))

;; *** Elisp
(defconst my/elisp-symbols
  '(("defun" . ?λ)
    ("not" . ?¬)))

;; *** Set symbol by mode
(defun my/get-pretty-symbols-by-mode (mode)
  (pcase mode
    ('haskell-mode (append
		    (my/prettify-comment)
		    my/haskell-symbols
		    my/haskell-type-symbols
		    my/generic-greek-symbols
		    my/generic-equality-symbols
		    my/generic-arrow-symbols
		    my/generic-logic-symbols
		    (my/prettify-outline-heading)))
    ('fsharp-mode (append
		   (my/prettify-comment)
		   my/fsharp-symbols
		   my/generic-greek-symbols
		   my/generic-equality-symbols
		   ;; my/generic-arrow-symbols
		   (my/prettify-outline-heading)
		   ))
    ('emacs-lisp-mode (append
		       (my/prettify-comment-lisp)
		       my/elisp-symbols
		       my/generic-greek-symbols
		       my/generic-equality-symbols
		       ;; my/generic-arrow-symbols
		       (my/prettify-outline-heading-lisp)
		       ;; (my/prettify-outline-heading-lisp-classic)
		       ))
    ('lisp-interaction-mode (append
			     (my/prettify-comment-lisp)
			     my/elisp-symbols
			     my/generic-greek-symbols
			     my/generic-equality-symbols
			     ;; my/generic-arrow-symbols
			     (my/prettify-outline-heading-lisp)
			     ;; (my/prettify-outline-heading-lisp-classic)
			     ))
    (_ (append
	(my/prettify-comment)
	my/generic-greek-symbols
	my/generic-equality-symbols
	;; my/generic-arrow-symbols
	my/generic-logic-symbols
	(my/prettify-outline-heading)
	))))

(add-hook 'prog-mode-hook (lambda () (interactive)
			    (setq-local prettify-symbols-alist
					(my/get-pretty-symbols-by-mode major-mode))))

;; ** Enable modify symbols inside comment blocks
(defun my/prettify-symbols-default-compose-p (start end _match)
  "Return true iff the symbol MATCH should be composed.
   The symbol starts at position START and ends at position END.
   This is the default for `prettify-symbols-compose-predicate'
   which is suitable for most programming languages such as C or Lisp."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((syntaxes-beg (if (memq (char-syntax (char-after start)) '(?w ?_))
			   '(?w ?_) '(?. ?\\)))
	 (syntaxes-end (if (memq (char-syntax (char-before end)) '(?w ?_))
			   '(?w ?_) '(?. ?\\))))
    (not (or (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg)
	     (memq (char-syntax (or (char-after end) ?\s)) syntaxes-end)
	     ;; It looks like this part makes it ignore comments, remove it
	     ;;(nth 8 (syntax-ppss))
	     ))))

(setq-default prettify-symbols-compose-predicate #'my/prettify-symbols-default-compose-p)

;; * Visuals
;; ** Define inverted default
(defface my/default-inverted
  '((t :inherit default))
  "Inverted default face")

;; ** Indicate empty lines
(setq-default indicate-empty-lines t)

;; ** Center text
(straight-use-package 'olivetti)

(setq olivetti-enable-visual-line-mode nil)
(setq olivetti-recall-visual-line-mode-entry-state nil)

(setq olivetti-mode-map (make-sparse-keymap))

(setq-default olivetti-body-width 130)

(setq my/olivetti-disabled-modes '(minibuffer-inactive-mode
				   exwm-mode
				   mu4e-headers-mode
				   pdf-view-mode
				   image-mode
				   org-agenda-mode
				   elfeed-search-mode))

(define-globalized-minor-mode global-olivetti-mode
  nil (lambda ()
	(unless (memq major-mode my/olivetti-disabled-modes)
	  (unless (string= " *diff-hl* " (buffer-name (current-buffer)))
	    (olivetti-mode 1)))))

(global-olivetti-mode 1)

(define-key my/leader-map (kbd "V") 'olivetti-mode)

;; ** Fringe size
;; Used by diff-hl and flycheck
;; Fringe only on the left side
(fringe-mode '(5 . 5))

;; ** Beacon
;; (straight-use-package 'beacon)

;; (beacon-mode 1)

;; ** Rainbow
;; Changes background of eg. #FF00FF to represent color
(straight-use-package 'rainbow-mode)

;; ** Highlight current line
(global-hl-line-mode t)

;; ** Symbol overlay - highlight thing
(straight-use-package '(symbol-overlay :type git :host github :repo "walseb/symbol-overlay" :branch "working-commit"))

(setq symbol-overlay-idle-time nil)

;; *** Enable instant highlighting
(defun my/symbol-overlay-post-command ()
  ;; It's required for this mode to be on, but since I use my own mode, just fake it being on
  ;; (setq-local symbol-overlay-mode t)
  (setq symbol-overlay-mode t)
  (ignore-errors
    ;; (let ((symbol-overlay-mode t))
    (symbol-overlay-remove-temp)
    (when (eq evil-state 'normal)
      (symbol-overlay-maybe-put-temp))))
;; )

;; *** Make it global
(define-minor-mode my/symbol-overlay-mode "" nil "" nil
  (when my/symbol-overlay-mode
    (require 'symbol-overlay)
    (add-hook 'post-command-hook 'my/symbol-overlay-post-command nil t)))

(define-globalized-minor-mode my/global-symbol-overlay my/symbol-overlay-mode my/symbol-overlay-mode)
(my/global-symbol-overlay 1)

;; ** Put lv at top
(with-eval-after-load 'lv
  (defun lv-window ()
    "Ensure that LV window is live and return it."
    (if (window-live-p lv-wnd)
	lv-wnd
      (let ((ori (selected-window))
	    buf)
	(prog1 (setq lv-wnd
		     (select-window
		      (let ((ignore-window-parameters t))
			(split-window
			 ;; Change is here
			 (frame-root-window) -1 'above))))
	  (if (setq buf (get-buffer " *LV*"))
	      (switch-to-buffer buf)
	    (switch-to-buffer " *LV*")
	    (set-window-hscroll lv-wnd 0)
	    (setq window-size-fixed t)
	    (setq mode-line-format nil)
	    (setq header-line-format nil)
	    (setq cursor-type nil)
	    (setq display-line-numbers nil)
	    (setq display-fill-column-indicator nil)
	    (set-window-dedicated-p lv-wnd t)
	    (set-window-parameter lv-wnd 'no-other-window t))
	  (select-window ori))))))

;; ** Show paren
;; Highlights matching paren under cursor

;; *** Delay
(setq show-paren-delay 0)

;; *** Set paren style
(setq show-paren-style 'paren)

(setq show-paren-highlight-openparen t)
(setq show-paren-when-point-inside-paren nil)
(setq show-paren-when-point-in-periphery t)

(show-paren-mode 1)

;; *** Show offscreen expression
;; https://with-emacs.com/posts/editing/show-matching-lines-when-parentheses-go-off-screen/
;; Not implemented yet
;; Define face to use
(defface my/show-paren-offscreen-face
  '((t :inherit highlight))
  "Face for showing function names offscreen")

;; ** Highlight parens
;; Highlights surrounding parens
(straight-use-package 'highlight-parentheses)

(setq hl-paren-colors '("Green4" "Green3" "Green2" "Green1"))

(global-highlight-parentheses-mode 1)

;; *** Set delay
(setq hl-paren-delay 0)

;; ** Highlight changes
(define-key my/leader-map (kbd "q") 'highlight-changes-mode)

;; ** Scrollbar
(straight-use-package 'yascroll)
(global-yascroll-bar-mode)
(setq yascroll:scroll-bar '(left-fringe))
(setq yascroll:disabled-modes '(image-mode))

;; *** Fix for emacs 27
;; The function ~window-fringes~ returns a list of 4 results on some versions of emacs because of some reason. This fixes that
(when (>= emacs-major-version 27)
  (defun yascroll:choose-scroll-bar ()
    (when (memq window-system yascroll:enabled-window-systems)
      (cl-destructuring-bind (left-width right-width outside-margins pers)
	  (window-fringes)
	(cl-loop for scroll-bar in (yascroll:listify yascroll:scroll-bar)
		 if (or (eq scroll-bar 'text-area)
			(and (eq scroll-bar 'left-fringe)
			     (> left-width 0))
			(and (eq scroll-bar 'right-fringe)
			     (> right-width 0)))
		 return scroll-bar)))))

;; ** Hl-Todo
(straight-use-package 'hl-todo)

(global-hl-todo-mode)

;; ** Hl-anything
;; Really buggy and makes buffer switching slow
;; (straight-use-package 'hl-anything)

;; (define-globalized-minor-mode global-hl-highlight-mode
;;   hl-highlight-mode hl-highlight-mode)
;; (hl-highlight-mode)
;; (global-hl-highlight-mode 1)

;; (define-key my/leader-map (kbd "M") 'hl-highlight-thingatpt-local)

;; ** Disable blinking cursor
(blink-cursor-mode 0)

;; ** Disable GUI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; ** Disable comments with toggle
(straight-use-package 'hide-comnt)
(defun my/hide-show-comment-toggle ()
  (interactive)
  (require 'hide-comnt)
  (call-interactively 'hide/show-comments-toggle))

(define-key my/leader-map (kbd "C-c") 'my/hide-show-comment-toggle)

;; ** Font lock
;; *** Font lock profiler
;; Use font-lock-profiler-buffer
(straight-use-package 'font-lock-profiler)

;; *** Remove unnecessary font-locks
;; **** Haskell
(setq haskell-font-lock-keywords '("do" "let" "proc" "where" "if" "then" "else"))

(defun haskell-font-lock-keywords ()
  '())

;; ***** Cabal
(setq haskell-cabal-font-lock-keywords '())

;; **** Elisp
(setq lisp-el-font-lock-keywords '())
(setq lisp-el-font-lock-keywords-1 '())
(setq lisp-el-font-lock-keywords-2 '())

;; **** Csharp
;; Doesn't seem to work fully, there are still lots of keywords left
(setq csharp-font-lock-keywords '())
(setq csharp-font-lock-keywords-1 '())
(setq csharp-font-lock-keywords-2 '())
(setq csharp-font-lock-keywords-3 '())

;; **** C
(setq cpp-font-lock-keywords '())
(setq c++-font-lock-keywords '())
(setq c++-font-lock-keywords-1 '())
(setq c++-font-lock-keywords-2 '())
(setq c++-font-lock-keywords-3 '())
(setq c-font-lock-keywords '())
(setq c-font-lock-keywords-1 '())
(setq c-font-lock-keywords-2 '())
(setq c-font-lock-keywords-3 '())

;; **** Racket
(setq racket-font-lock-keywords '())
(setq racket-font-lock-keywords-0 '())
(setq racket-font-lock-keywords-1 '())
(setq racket-font-lock-keywords-2 '())
(setq racket-font-lock-keywords-3 '())
(setq racket-font-lock-keywords-level-0 '())
(setq racket-font-lock-keywords-level-1 '())
(setq racket-font-lock-keywords-level-2 '())
(setq racket-font-lock-keywords-level-3 '())

;; **** Nix
(with-eval-after-load 'nix-mode
  (setq nix-font-lock-keywords '()))

;; * Modeline
;; ** Calculate frame width
(defvar my/frame-width (frame-width))

(defun my/frame-width-update()
  (interactive)
  (setq my/frame-width (frame-width)))

;; ** Mode line highlight face
(defface my/mode-line-highlight
  '((t :inherit highlight))
  "Face for highlighting something in mode line")

;; ** Mode line contents
;; *** Cursor position settings
(setq mode-line-position
      '(;; %p print percent of buffer above top of window, o Top, Bot or All
	;; (-3 "%p")
	;; %I print the size of the buffer, with kmG etc
	;; (size-indication-mode ("/" (-4 "%I")))
	;; " "
	;; %l print the current line number
	;; %c print the current column
	(line-number-mode ("%l" (column-number-mode ":%c")))))

;; ** Modules
;; *** Count buffer line numbers
;; This is run on every key-press and is pretty fast:
;; (benchmark 1 (my/count-line-numbers))
;; (benchmark 1000000 (my/count-line-numbers))

(defun my/count-line-numbers ()
  ;; Diff-hl mode should know when it's fine to measure buffer length
  (if (not (member major-mode '(pdf-view-mode minibuffer-inactive-mode image-mode)))
      (int-to-string (count-lines (point-min) (point-max)))
    "???"))

;; *** Git project and branch name
(require 'vc-git)

;; When projectile-mode is on, project name is updated on every keypress, here it is fixed
(defvar-local my/projectile-project-name nil)
(defvar-local my/buffer-git-branch nil)
;; Make sure every buffer is only scanned once
(defvar-local my/projectile-project-buffer-already-scanned nil)

(defun my/update-projectile-project-name()
  (interactive)
  ;; Some virtual buffers don't work, but dired-mode does
  (when (or (string= major-mode 'dired-mode) (and buffer-file-name (file-exists-p buffer-file-name) (not my/projectile-project-buffer-already-scanned)))
    (setq my/projectile-project-buffer-already-scanned t)
    (setq my/projectile-project-name (projectile-project-name))
    (setq my/buffer-git-branch (car (vc-git-branches)))))

(if (>= emacs-major-version 27)
    (add-hook 'window-state-change-hook 'my/update-projectile-project-name)
  (add-hook 'window-configuration-change-hook 'my/update-projectile-project-name))

;; *** Git changes
(defvar-local my/git-changes-string nil)

(defvar-local my/vc-insert-count 0)
(defvar-local my/vc-change-count 0)
(defvar-local my/vc-delete-count 0)

(defun my/mode-line-update-git-changes-string ()
  (setq my/git-changes-string (format "+%d ~%d -%d"
				      my/vc-insert-count
				      my/vc-change-count
				      my/vc-delete-count)))

(defun my/mode-line-update-git-changes-string-reset ()
  (setq my/vc-insert-count 0)
  (setq my/vc-change-count 0)
  (setq my/vc-delete-count 0)
  (setq my/git-changes-string nil))

(defun my/modeline-update-git-changes (changes)
  "CHANGES is generated by `(diff-hl-changes)'"
  (my/mode-line-update-git-changes-string-reset)
  (mapc (lambda (entry)
	  (pcase (nth 2 entry)
	    ('insert (setq my/vc-insert-count (+ my/vc-insert-count (nth 1 entry))))
	    ('change (setq my/vc-change-count (+ my/vc-change-count (nth 1 entry))))
	    ('delete (setq my/vc-delete-count (+ my/vc-delete-count (nth 1 entry))))))
	changes)
  (my/mode-line-update-git-changes-string))

;; **** Override old function
(with-eval-after-load 'diff-hl
  (defun diff-hl-changes ()
    (my/mode-line-update-git-changes-string-reset)
    (let* ((file buffer-file-name)
	   (backend (vc-backend file)))
      (when backend
	(let ((state (vc-state file backend)))
	  (cond
	   ((diff-hl-modified-p state)
	    (let* (diff-auto-refine-mode res)
	      (with-current-buffer (diff-hl-changes-buffer file backend)
		(goto-char (point-min))
		(unless (eobp)
		  (ignore-errors
		    (diff-beginning-of-hunk t))
		  (while (looking-at diff-hunk-header-re-unified)
		    (let ((line (string-to-number (match-string 3)))
			  (len (let ((m (match-string 4)))
				 (if m (string-to-number m) 1)))
			  (beg (point)))
		      (diff-end-of-hunk)
		      (let* ((inserts (diff-count-matches "^\\+" beg (point)))
			     (deletes (diff-count-matches "^-" beg (point)))
			     (type (cond ((zerop deletes) 'insert)
					 ((zerop inserts) 'delete)
					 (t 'change))))
			(when (eq type 'delete)
			  (setq len 1)
			  (cl-incf line))
			(push (list line len type) res))))))
	      (my/modeline-update-git-changes res)
	      (nreverse res)))
	   ((eq state 'added)
	    `((1 ,(line-number-at-pos (point-max)) insert)))
	   ((eq state 'removed)
	    `((1 ,(line-number-at-pos (point-max)) delete)))))))))

;; ** Format
;; Don't set it directly here, because the variable is needed to fix exwm
(setq-default header-line-format
	      (quote
	       (
		;; Print if recursive editing
		"%["

		;; Information bar
		mode-line-mule-info
		mode-line-client

		;; If buffer is modified
		mode-line-modified

		;; Turns into @ when remote
		mode-line-remote

		" "

		;; Print current line number
		;;"%l"
		;;"%p"
		;;(:eval (format "%d" (/ (window-start) 0.01 (point-max))))
		;;"%p"

		;;"@"
		;; Print total line number and buffer position
		;; (:eval
		;; (let
		;; ((line-number-count (+ (count-lines (point-min) (point-max)) 1))
		;; (point-pos (count-lines (point) (point-min))))
		;; (let
		;; ((point-in-buffer-percentage (floor (* (/ (float point-pos) line-number-count) 100))))
		;; (concat (int-to-string point-in-buffer-percentage) "% ~" (int-to-string line-number-count)))))

		(:eval (my/count-line-numbers))

		;;"%I"

		;; is narrowed
		"%n"

		;; Is loccur
		(:eval (when (and (boundp 'loccur-mode) loccur-mode)
			 " Loccur"))

		" | "

		;; Print error if any
		"%e"

		;; Print mode
		(:eval (when defining-kbd-macro
			 (concat
			  (propertize
			   "[MACRO]"
			   'face 'my/mode-line-highlight)
			  " ")))

		;; Print buffer name
		"%b > "

		;; Print mode
		;; "%m"
		;; With this it also works properly in exwm-mode
		(:eval (symbol-name major-mode))
		" >"

		;; Git
		(:eval
		 (if (and my/projectile-project-name my/buffer-git-branch (not (string= my/projectile-project-name "-")))
		     (concat
		      " "
		      my/buffer-git-branch
		      ;; "@"
		      ;; my/projectile-project-name
		      (when my/git-changes-string
			(concat
			 "["
			 my/git-changes-string
			 "]"
			 ))
		      " >"
		      )))

		(:eval (envrc--lighter))

		(" "
		 (company-candidates
		  (:eval
		   (if (consp company-backend)
		       (my/company--group-lighter (nth company-selection
						       company-candidates)
						  company-lighter-base)
		     (when company-backend
		       (concat
			"| "
			(symbol-name company-backend)
			))))
		  ;; Symbol when company is not in use
		  "")))))

;; * Status line
;; ** Modules
;; http://www.holgerschurig.de/en/emacs-tayloring-the-built-in-mode-line/

;; *** Garbage Collection
(defvar my/mode-line-show-GC-stats nil)
(defun my/mode-line-toggle-show-GC-stats ()
  (interactive)
  (setq my/mode-line-show-GC-stats (not my/mode-line-show-GC-stats)))

;; *** CPU heat
(defvar my/mode-line-enable-cpu-temp nil)

(if (and
     ;; If lm_sensors is not installed
     (my/is-system-package-installed 'sensors)
     ;; If there aren't any cpu heat sensors (eg. virtual machine)
     (= 0 (string-match-p ""
			  (shell-command-to-string "sensors | grep \"Core 0:\"")))
     ;; If it returns "no sensors found"
     (not (string-match-p "No sensors found"
			  (shell-command-to-string "sensors | grep \"Core 0:\""))))
    (setq my/mode-line-enable-cpu-temp t))

(defvar my/cpu-temp "")

(defun my/validate-cpu-temp (temp)
  (if (and temp (not (string= temp "")))
      temp
    nil))

(defun my/update-cpu-temp ()
  (interactive)
  ;; Ryzen test string:
  ;; "Tdie:         +40.2°C  (high = +70.0°C)\n"
  ;; Intel test string:
  ;; "Core 0:       +46.0°C  (high = +105.0°C, crit = +105.0°C)\n"
  (let* ((intel-cpu-temp-str (my/validate-cpu-temp (shell-command-to-string "sensors | grep \"Core 0:\"")))
	 (ryzen-cpu-temp-str (my/validate-cpu-temp (shell-command-to-string "sensors | grep \"Tdie\:\"")))
	 (cpu-temp-str (or intel-cpu-temp-str ryzen-cpu-temp-str))
	 (cpu-temp-pos-beg (string-match-p "\+.*C\s" cpu-temp-str))
	 (cpu-temp-pos-end (string-match-p "  " cpu-temp-str cpu-temp-pos-beg)))
    (setq my/cpu-temp (substring cpu-temp-str cpu-temp-pos-beg cpu-temp-pos-end))))

(if my/mode-line-enable-cpu-temp
    (my/allocate-update-time 'my/update-cpu-temp))

;; *** Disk space
(defvar my/disk-space nil)
(defun my/update-disk-space ()
  (interactive)
  (setq my/disk-space (my/file-size-human-readable (floor (* 1000 (string-to-number (get-free-disk-space user-emacs-directory)))))))

;; *** Network traffic
;; **** Linux
(defvar my/mode-line-enable-network-traffic nil)

(if (file-exists-p "/proc/net/dev")
    (setq my/mode-line-enable-network-traffic t))

;; ***** RX
;; Received
(defvar my/rx 0)
(defvar my/rx-delta-formatted "0")

(defun my/linux-get-network-rx ()
  (with-temp-buffer
    (insert-file-contents "/proc/net/dev")
    (goto-char 1)
    (let ((rx 0))
      (while (search-forward-regexp "^[\s\t]*\\(.*\\):" nil t)
	(unless (string= (match-string 1) "lo")
	  (setq rx (+ rx (read (current-buffer))))))
      rx)))

(defun my/linux-update-network-rx-delta ()
  (interactive)
  (setq my/rx-new (my/linux-get-network-rx))
  (setq my/rx-delta-formatted (my/file-size-human-readable (- my/rx-new my/rx)))
  (setq my/rx my/rx-new))

(if my/mode-line-enable-network-traffic
    (my/allocate-update-time 'my/linux-update-network-rx-delta))

;; ***** TX
;; Transmitted
(setq my/tx 0)
(defvar my/tx-delta-formatted "0")

(defun my/linux-get-network-tx ()
  (with-temp-buffer
    (insert-file-contents "/proc/net/dev")
    (goto-char 1)
    (let ((tx 0))
      (while (search-forward-regexp "^[\s\t]*\\(.*\\):" nil t)
	(unless (string= (match-string 1) "lo")
	  (forward-word 8)
	  (setq tx (+ tx (read (current-buffer))))))
      tx)))

(defun my/linux-update-network-tx-delta ()
  (interactive)
  (setq my/tx-new (my/linux-get-network-tx))
  (setq my/tx-delta-formatted  (my/file-size-human-readable (- my/tx-new my/tx)))
  (setq my/tx my/tx-new))

(if my/mode-line-enable-network-traffic
    (my/allocate-update-time 'my/linux-update-network-tx-delta))

;; *** Battery
;; If there is a battery, display it in the mode line
(require 'battery)

(setq battery-mode-line-format "%th - %p")

;; (display-battery-mode 1)

(defun my/battery-update ()
  (unless battery-status-function
    (setq battery-status-function (my/get-battery-status-function)))
  (ignore-errors (battery-update)))

;; **** Get battery status function
(defun my/get-battery-status-function ()
  (if (>= emacs-major-version 28)
      ;; Emacs 28 version
      (cond ((member battery-upower-service (dbus-list-activatable-names))
	     #'battery-upower)
	    ((and (eq system-type 'gnu/linux)
		  (battery--find-linux-sysfs-batteries))
	     #'battery-linux-sysfs)
	    ((and (eq system-type 'gnu/linux)
		  (file-directory-p "/proc/acpi/battery"))
	     #'battery-linux-proc-acpi)
	    ((and (eq system-type 'gnu/linux)
		  (file-readable-p "/proc/apm"))
	     #'battery-linux-proc-apm)
	    ((and (eq system-type 'berkeley-unix)
		  (file-executable-p "/usr/sbin/apm"))
	     #'battery-bsd-apm)
	    ((and (eq system-type 'darwin)
		  (ignore-errors
		    (with-temp-buffer
		      (and (eq (call-process "pmset" nil t nil "-g" "ps") 0)
			   (not (bobp))))))
	     #'battery-pmset)
	    ((fboundp 'w32-battery-status)
	     #'w32-battery-status))
    ;; Emacs 27 version
    (cond ((and (eq system-type 'gnu/linux)
		(file-readable-p "/proc/apm"))
	   #'battery-linux-proc-apm)
	  ((and (eq system-type 'gnu/linux)
		(file-directory-p "/proc/acpi/battery"))
	   #'battery-linux-proc-acpi)
	  ((and (eq system-type 'gnu/linux)
		(file-directory-p "/sys/class/power_supply/")
		(battery--find-linux-sysfs-batteries))
	   #'battery-linux-sysfs)
	  ((and (eq system-type 'berkeley-unix)
		(file-executable-p "/usr/sbin/apm"))
	   #'battery-bsd-apm)
	  ((and (eq system-type 'darwin)
		(condition-case nil
		    (with-temp-buffer
		      (and (eq (call-process "pmset" nil t nil "-g" "ps") 0)
			   (> (buffer-size) 0)))
		  (error nil)))
	   #'battery-pmset)
	  ((fboundp 'w32-battery-status)
	   #'w32-battery-status))))

;; **** Timer
(my/allocate-update-time 'my/battery-update)

;; *** Date and time
;; Display time and date in good format (also displays CPU load)
(defvar my/date "")
(defvar my/time "")

(defun my/update-date ()
  (interactive)
  ;; Day:Month:Year
  ;; (setq my/date (format-time-string "%d-%m-%Y"))
  (setq my/date (format-time-string "%Y-%m-%d")))

(defun my/update-time ()
  (interactive)
  (setq my/time (format-time-string "%H:%M")))

(my/allocate-update-time 'my/update-time)
(my/allocate-update-time 'my/update-date (* 60 60))

;; *** CPU load average
(defvar my/cpu-load-average 0)
(defvar my/high-cpu-load-average 2)

(defun my/update-cpu-load-average ()
  (interactive)
  (setq my/cpu-load-average (/ (nth 0 (load-average)) 100.0)))

(my/allocate-update-time 'my/update-cpu-load-average)

;; *** Ram usage
(defvar my/mode-line-enable-available-mem nil)

(if (and (file-exists-p "/proc/meminfo")
	 (progn
	   (with-temp-buffer

	     (insert-file-contents "/proc/meminfo")
	     (setq my/mem-string (buffer-string))
	     (ignore-errors
	       (string-match "MemAvailable:.*\s" my/mem-string)))))
    (setq my/mode-line-enable-available-mem t))

(defvar my/available-mem-formatted "nil")
(defvar my/available-mem 0)

(defun my/linux-update-available-mem ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents "/proc/meminfo")
    (setq my/mem-string (buffer-string))

    (when (ignore-errors
	    (string-match "MemAvailable:.*\s" my/mem-string))
      (setq my/mem-string (match-string 0 my/mem-string))

      ;; Default returns kb, *1000 to get it to bytes
      (setq my/available-mem
	    (* 1000 (string-to-number
		     (substring my/mem-string (string-match "[0-9]" my/mem-string) -1))))

      (setq my/available-mem-formatted (my/file-size-human-readable my/available-mem nil t)))))

(if my/mode-line-enable-available-mem
    (my/allocate-update-time 'my/linux-update-available-mem))

;; *** Uptime
(defvar my/uptime-start-time (float-time))
(defvar my/uptime-total-time-formated "0M")

(defun my/get-uptime-formated-time ()
  (let* ((total-time  (- (float-time) my/uptime-start-time)))
    (if (> total-time (* 60 60))
	(concat (format "%0.1f"(/ total-time 3600.0)) "H")
      (concat (int-to-string (/ (floor total-time) 60)) "M"))))

(defun my/update-uptime-timer ()
  (interactive)
  (setq my/uptime-total-time-formated (my/get-uptime-formated-time)))

(my/allocate-update-time 'my/update-uptime-timer)

;; *** Org-clock mode line
(defun my/org-clock-mode-line-schedule ()
  (remove-hook 'org-clock-in-hook 'my/org-clock-mode-line-schedule)
  (my/allocate-update-time 'org-clock-update-mode-line))

(with-eval-after-load 'org-clock
  (add-hook 'org-clock-in-hook 'my/org-clock-mode-line-schedule))

;; ** Format
;; Only applicable to X since terminal never stretches, etc
(add-hook 'exwm-workspace-switch-hook 'my/frame-width-update)

(defun my/mode-line-align (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- my/frame-width (length left) 2)))
    (format (format "%%s %%%ds" available-width) left right)))

;; Remember to update `mini-modeline-r-format' after setting this manually
(progn
  (setq-default my/status-line-format
		'(:eval
		  (format-mode-line
		   (quote
		    (
		     (:eval my/past-alerts)
		     " "

		     (:eval
		      (when (string= major-mode 'exwm-mode)
			(concat
			 (propertize (concat " ") 'face `(:background ,(my/get-current-evil-cursor-color)))
			 " ")))

		     ;; Org clock
		     (:eval (if (org-clocking-p)
				(concat "Org: " (propertize (string-trim-left (substring-no-properties org-mode-line-string)) 'face 'default) " | ")
			      (concat "Org: " (propertize "No clock" 'face 'diff-removed) " | ")))

		     (:eval (if my/mode-line-show-GC-stats
				(concat
				 " GC: " (number-to-string (truncate gc-elapsed))
				 "(" (number-to-string gcs-done) ")"
				 " | "
				 )))

		     ;; (:eval (if (and my/gnus-unread-string (not (string= my/gnus-unread-string "")))
		     ;;	       (concat " | "
		     ;;		       my/gnus-unread-string)))

		     (:eval (when my/mu4e-unread-mail-count
			      (concat
			       (let ((str (concat "Mail: " my/mu4e-unread-mail-count)))
				 (if (> (string-to-number my/mu4e-unread-mail-count) 0)
				     (propertize str 'face `(:inherit my/default-inverted))
				   str))
			       " | "
			       )))



		     ;; (:eval (if my/mode-line-enable-network-traffic
		     ;;		(concat
		     ;;		 my/tx-delta-formatted " ↑ "
		     ;;		 my/rx-delta-formatted " ↓ "
		     ;;		 "| "
		     ;;		 )))

		     (:eval
		      (when my/mode-line-enable-available-mem
			(concat
			 "MEM: "
			 ;; If < 100 mb mem, make text red
			 (if (< my/available-mem 1000000000)
			     (propertize my/available-mem-formatted 'face `(:background "red"))
			   my/available-mem-formatted)
			 " | ")))

		     (:eval (when (and battery-mode-line-string (not (eq battery-mode-line-string "")))
			      (concat "BAT: " battery-mode-line-string "%%%   | ")))

		     (:eval (if my/mode-line-enable-cpu-temp
				(concat "HT: " my/cpu-temp " | ")))

		     (:eval
		      (concat
		       "C: "
		       (number-to-string my/cpu-load-average)
		       " | "))

		     ;; (:eval (concat "Up: " my/uptime-total-time-formated))
		     ;; " | "

		     (:eval my/date)

		     " "

		     (:eval my/time)

		     )))))
  (setq-default mini-modeline-r-format my/status-line-format))

;; ** mini-modeline
(straight-use-package 'mini-modeline)

(setq mini-modeline-enhance-visual nil)
;; Mini-modeline flashes during GC if this is t
(setq garbage-collection-messages nil)

;; This fixes a bug with ~counsel-describe-function~
(setq mini-modeline-echo-duration 99999)

;; (setq-default mini-modeline-r-format my/status-line-format)
(mini-modeline-mode 1)

;; (setq mini-modeline-frame (window-frame (minibuffer-window)))
;; (setq mini-modeline-frame nil)

(setq mode-line-format nil)
(setq-default mode-line-format nil)

;; * Backups
;; Stop emacs from creating backup files
(setq make-backup-files nil)

;; ** Inhibit backup
(defvar my/enable-backup nil)

(defun my/should-backup ()
  (pcase my/enable-backup
    ('true t)
    ('false nil)
    ('nil (setq-local my/enable-backup (my/calculate-should-backup))
	  (my/should-backup))
    (_ (error "my/calculate-should-backup had wrong return type"))))

(defun my/calculate-should-backup ()
  (if (or
       (my/is-file-gpg-protected (buffer-file-name))
       (memq major-mode '(image-mode))
       )
      'false
    'true))

;; ** Auto-save
;; Saves so that no data is lost in the event of a crash
(defvar my/auto-saves-directory (expand-file-name (concat user-emacs-directory "auto-saves/")))
(my/create-dir-if-not-exist my/auto-saves-directory)

(setq auto-save-file-name-transforms
      `((".*" ,my/auto-saves-directory t)))

;; ** git-backup
(straight-use-package 'git-backup)
(straight-use-package 'git-backup-ivy)

(setq git-backup-ivy-preview-remove-header nil)

(add-hook 'after-save-hook (lambda ()
			     (require 'git-backup-ivy)
			     (when (my/should-backup)
			       (git-backup-version-file git-backup-ivy-git-path git-backup-ivy-backup-path nil (buffer-file-name)))))

(define-key my/leader-map (kbd "C-u") 'git-backup-ivy)

;; ** Undo
;; *** Disable undo warning buffer
;; There is a warning window that pops up if you have made too many changes to a buffer, this might stop long macros, so stop that window from popping up
(require 'warnings)
(add-to-list 'warning-suppress-types '(undo discard-info))

;; ** Undo tree
(straight-use-package 'undo-tree)

;; Fixes errors
(setq undo-tree-enable-undo-in-region nil)
(setq-default undo-tree-enable-undo-in-region nil)

(setq-default undo-tree-visualizer-lazy-drawing nil)

(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)

;; *** Global undo-tree mode
(eval-and-compile
  (require 'undo-tree))

(defun my/turn-on-undo-tree-mode (&optional print-message)
  (when (my/should-backup)
    (turn-on-undo-tree-mode print-message)))

(define-globalized-minor-mode my/global-undo-tree-mode
  undo-tree-mode my/turn-on-undo-tree-mode)

(my/global-undo-tree-mode t)

;; *** Persistent history
(setq my/undo-tree-history-dir (concat user-emacs-directory "undo-tree"))

(ignore-errors
  (make-directory my/undo-tree-history-dir))

;; (setq-default undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist `(("." . ,my/undo-tree-history-dir)))

;; *** Disable modes in visualizer
(add-hook 'undo-tree-visualizer-mode-hook (lambda () (add-hook 'visual-line-mode-hook (lambda () (when visual-line-mode (visual-line-mode -1))) nil t)))

;; *** Keys
(setq undo-tree-visualizer-mode-map (make-sparse-keymap))

(with-eval-after-load 'undo-tree
  (evil-define-key 'insert undo-tree-visualizer-mode-map (kbd "p") #'undo-tree-visualize-undo)

  (evil-define-key 'insert undo-tree-visualizer-mode-map (kbd "p") #'undo-tree-visualize-undo)
  (evil-define-key 'insert undo-tree-visualizer-mode-map (kbd "n") #'undo-tree-visualize-redo)
  (evil-define-key 'insert undo-tree-visualizer-mode-map (kbd "l") #'undo-tree-visualize-switch-branch-right)
  (evil-define-key 'insert undo-tree-visualizer-mode-map (kbd "h") #'undo-tree-visualize-switch-branch-left)
  (evil-define-key 'insert undo-tree-visualizer-mode-map (kbd "d") #'undo-tree-visualizer-toggle-diff))

;; * Startup view
;; ** Startup view
(defun my/startup-view ()
  (interactive)
  (delete-other-windows)
  ;; Wall
  (my/show-random-wallpaper)

  (split-window-below)
  (other-window 1)

  ;; Agenda
  (my/org-agenda-show-agenda-and-todo)

  (split-window-right)
  (other-window 1)

  (find-file (concat my/notes-folder "Organize/Timeline/Achievements.png"))
  (image-increase-size 50)
  (image-forward-hscroll 10))

;; ** Run it on startup
;; Timer here fixes crash on slow PCs
(add-hook 'exwm-init-hook (lambda () (run-with-timer nil nil 'my/startup-view)))

;; * Run command on boot
(if my/run-command-on-boot
    (async-shell-command my/run-command-on-boot))

;; * Restore gc mem
(setq gc-cons-threshold my/gc-mem)
(garbage-collect)

;; * Report start time
(run-with-timer 4 nil (lambda () (interactive) (message (concat "Booted in " (emacs-init-time)))))
(message "Config loaded!")
